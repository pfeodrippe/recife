(ns recife.core
  (:require
   [alandipert.interpol8 :as int]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.repl :as repl]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [lambdaisland.deep-diff2 :as ddiff]
   [malli.core :as m]
   [malli.error :as me]
   [medley.core :as medley]
   [recife.schema :as schema]
   [tla-edn.core :as tla-edn]
   [tla-edn.spec :as spec])
  (:import
   (java.io File)
   (lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion)
   (tlc2.output IMessagePrinterRecorder MP EC)
   (tlc2.value.impl Value StringValue ModelValue)
   (util UniqueString)))

(defn- custom-munge
  [v]
  (str/replace (munge v) #"\." "___"))

(defn- to-edn-string
  [v]
  (let [s (str/replace (str v)  #"___" ".")]
    (keyword (repl/demunge s))))

(extend-protocol tla-edn/TLAPlusEdn
  StringValue
  (-to-edn [v]
    (let [s (str/replace (str (.val v)) #"___" ".")]
      (keyword (repl/demunge s))))

  UniqueString
  (-to-edn [v]
    (to-edn-string v)))

(defmethod tla-edn/to-tla-value clojure.lang.Keyword
  [v]
  (StringValue. (custom-munge (symbol v))))

(defmethod tla-edn/to-tla-value clojure.lang.Seqable
  [v]
  (tla-edn/to-tla-value (into [] v)))

(defmethod tla-edn/to-tla-value clojure.lang.Symbol
  [v]
  (ModelValue/make (str v)))

(def ^:private exception-filename
  "recife-exception.ser")

(def ^:private invariant-data-filename
  "invariant-data.ser")

(def ^:private Lock (Object.))

(defn- serialize-obj [object filename]
  (locking Lock
    (when-not (.exists (io/as-file filename))
      (with-open [outp (-> (java.io.File. filename) java.io.FileOutputStream. java.io.ObjectOutputStream.)]
        (.writeObject outp object)))))

(defn- deserialize-obj [filename]
  (with-open [inp (-> (java.io.File. filename) java.io.FileInputStream. java.io.ObjectInputStream.)]
    (.readObject inp)))

(defn- process-operator*
  "Created so we can use the same \"meat\" when invoking the function
  in a context outside TLC."
  [identifier f self context main-var]
  (let [global main-var
        local (get-in main-var [::procs self])
        result (f (merge {:self self} context global local))
        result-global (medley/filter-keys namespace result)
        result-local (medley/remove-keys namespace result)
        metadata (if (:recife/metadata result-global)
                   ;; There is some bug somewhere which prevent us of the form
                   ;;    Caused by java.lang.ClassCastException
                   ;;    clojure.lang.KeywordLookupSite$1 incompatible with
                   ;;    clojure.lang.IPersistentCollection
                   ;; which is triggered by multiple threads only, so we do this
                   ;; check here to try to prevent it.
                   (merge (:recife/metadata result-global)
                          {:context [identifier (merge {:self self} context)]})
                   {:context [identifier (merge {:self self} context)]})]
    (if (nil? result)
      (assoc main-var :recife/metadata metadata)
      (merge
       ;; All namespaced keywords are global.
       (dissoc result-global :recife/metadata)
       ;; While unamespaced keywords are part of the
       ;; process as local variables.
       {::procs (merge (::procs result-global)
                       {self
                        ;; Remove `extra-args` and `self`
                        ;; so they don't become stateful.
                        (apply dissoc result-local :self (keys context))})
        :recife/metadata metadata}))))

(defn process-operator
  [identifier f self-tla extra-args-tla ^Value main-var-tla]
  (try
    (let [self (tla-edn/to-edn self-tla {:string-to-keyword? true})
          main-var (tla-edn/to-edn main-var-tla {:string-to-keyword? true})
          ;; `"_no"` is a indicator that the operator is not using extra args.
          extra-args (if (contains? (set (mapv str (.-names extra-args-tla))) "_no")
                       {}
                       (tla-edn/to-edn extra-args-tla))
          result (process-operator* identifier f self extra-args main-var)]
      (tla-edn/to-tla-value result))
    (catch Exception e
      (serialize-obj e exception-filename)
      (throw e))))

(defn process-config-operator
  [f ^Value main-var-tla]
  (try
    (let [main-var (tla-edn/to-edn main-var-tla {:string-to-keyword? true})
          result (f main-var)]
      (if (vector? result)
        ;; If we have a vector, the first element is a boolean  and the
        ;; second one is data which will be appended to the result if
        ;; the boolean was falsy.
        (do (when-not (first result)
              ;; Serialize data to be used later when building the result.
              (serialize-obj (second result) invariant-data-filename))
            (tla-edn/to-tla-value (boolean (first result))))
        (tla-edn/to-tla-value (boolean result))))
    (catch Exception e
      (serialize-obj e exception-filename)
      (throw e))))

(defn- parse
  "`f` is a function which receives one argument, a vector, the
  first element is the `result` and the last one is the `expr`."
  ([expr]
   (parse expr first))
  ([expr f]
   (let [result (cond
                  (vector? expr)
                  (let [[id & args] expr]
                    (case id
                      :exists
                      (format "\\E %s : (%s)"
                              (->> (first args)
                                   (mapv (fn [[k v]]
                                           (format "%s \\in (%s)" (parse k f) (parse v f))))
                                   (str/join ", "))
                              (parse (last args) f))

                      :forall
                      (format "\\A %s : (%s)"
                              (->> (first args)
                                   (mapv (fn [[k v]]
                                           (format "%s \\in (%s)" (parse k f) (parse v f))))
                                   (str/join ", "))
                              (parse (last args) f))

                      :eventually
                      (format "<>(%s)" (parse (first args) f))

                      :always
                      (format "[](%s)" (parse (first args) f))

                      :invoke
                      (parse {:env (first args)
                              :fn (last args)}
                             f)

                      :or
                      (->> args
                           (mapv (comp #(str "(" % ")") #(parse % f)))
                           (str/join " \\/ "))

                      :and
                      (->> args
                           (mapv (comp #(str "(" % ")") #(parse % f)))
                           (str/join " /\\ "))

                      :raw
                      (first args)

                      (format "%s(%s)"
                              (symbol (custom-munge id))
                              (->> (conj (vec args) "main_var")
                                   (str/join ", ")))))

                  (keyword? expr)
                  (tla-edn/to-tla-value expr)

                  (set? expr)
                  (tla-edn/to-tla-value expr)

                  :else
                  expr)]
     (f [result expr]))))

(defn tla
  [identifier expr]
  (let [form (parse expr)]
    {:identifier (str (symbol (custom-munge identifier))
                      (some->> (:receives expr)
                               (str/join ", ")
                               (format "(%s, _main_var)")))
     :form form
     :recife.operator/type :tla-only}))

(defn context-from-state
  [state]
  (if (vector? state)
    (get-in state [1 :recife/metadata :context])
    (get-in state [:recife/metadata :context])))

(defmulti simulate
  (fn [context _state]
    (first context)))

(defn reg
  ([identifier expr]
   (reg identifier {} expr))
  ([identifier opts expr]
   (let [op (eval
             `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
                [~'self ^Value ~'extra-args ~'main-var]
                (process-operator ~identifier ~expr ~'self ~'extra-args ~'main-var)))]
     (defmethod simulate identifier
       [context state]
       (let [self (get-in context [1 :self])
             identifier (first context)
             context' (second context)]
         ;; This is the same function that we use at `process-operator`.
         (process-operator* identifier expr self context' (if (vector? state)
                                                            (second state)
                                                            state))))
     (with-meta
       {:identifier (str (symbol (str (custom-munge identifier) "2")) "(self, _extra_args, _main_var) == self = self /\\ _main_var = _main_var /\\ _extra_args = _extra_args\n\n"
                         (symbol (custom-munge identifier))
                         "(self, _main_var)")
        :op-ns (-> op meta :op-ns)
        :recife.operator/name (str (symbol (custom-munge identifier)))
        :recife/fairness (cond
                           (or (-> expr meta :fair)
                               (-> opts meta :fair)) :recife.fairness/weakly-fair
                           (or (-> expr meta :fair+)
                               (-> opts meta :fair+)) :recife.fairness/strongly-fair)
        :form (parse [:and
                      [:raw (format "\nmain_var[%s][self][\"pc\"] = %s"
                                     (tla-edn/to-tla-value ::procs)
                                     (tla-edn/to-tla-value identifier))]
                      (if (seq opts)
                        [:raw (parse [:exists (->> opts
                                                     (mapv (fn [[k v]]
                                                             (if (coll? v)
                                                               [(symbol (str (custom-munge k)))
                                                                (parse (set v))]
                                                               [(symbol (str (custom-munge k)))
                                                                [:raw (str " main_var[" (parse v) "]")]]))))
                                       [:raw (str "\nmain_var' = "
                                                   (symbol (str (custom-munge identifier) "2"))
                                                   "(self, "
                                                   (->> (keys opts)
                                                        (mapv #(hash-map % (symbol (custom-munge %))))
                                                        (into {})
                                                        tla-edn/to-tla-value)
                                                   ", main_var)")]])]
                        ;; If no opts, send a bogus structure.
                        [:raw (str "main_var' = "
                                    (symbol (str (custom-munge identifier) "2"))
                                    "(self, [_no |-> 0], main_var)")])
                      ;; With it we can test deadlock.
                      [:raw "[x \\in DOMAIN main_var' \\ {\"recife_SLASH_metadata\"} |-> main_var'[x]] /= [x \\in DOMAIN main_var \\ {\"recife_SLASH_metadata\"} |-> main_var[x]]"]])
        :recife.operator/type :operator}
       (merge (meta expr) (meta opts))))))

(defn invariant
  [identifier expr]
  (let [op (eval
            `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
               [^Value ~'main-var]
               (process-config-operator ~expr ~'main-var)))]
    {:identifier (symbol (custom-munge identifier))
     :op-ns (-> op meta :op-ns)
     :identifier-2 (str (symbol (str (custom-munge identifier) "2")) "(_main_var) == _main_var = _main_var")
     :form (str (symbol (str (custom-munge identifier) "2"))
                "(main_var)")
     :recife.operator/type :invariant}))

(defn checker
  [identifier expr]
  (let [op (eval
            `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
               [^Value ~'main-var]
               (process-config-operator (complement ~expr) ~'main-var)))]
    {:identifier (symbol (custom-munge identifier))
     :op-ns (-> op meta :op-ns)
     :identifier-2 (str (symbol (str (custom-munge identifier) "2")) "(_main_var) == _main_var = _main_var")
     :form (str (symbol (str (custom-munge identifier) "2"))
                "(main_var)")
     :recife.operator/type :invariant}))

(defn state-constraint
  [identifier expr]
  (let [op (eval
            `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
               [^Value ~'main-var]
               (process-config-operator ~expr ~'main-var)))]
    {:identifier (symbol (custom-munge identifier))
     :op-ns (-> op meta :op-ns)
     :identifier-2 (str (symbol (str (custom-munge identifier) "2")) "(_main_var) == _main_var = _main_var")
     :form (str (symbol (str (custom-munge identifier) "2"))
                "(main_var)")
     :recife.operator/type :state-constraint}))

(defn- compile-temporal-property
  [collector identifier]
  (fn [id-and-args]
    (parse id-and-args (fn [[result expr]]
                         (cond
                           (fn? expr)
                           (let [op (eval
                                     `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
                                        [^Value ~'main-var]
                                        (process-config-operator ~expr ~'main-var)))]
                             (swap! collector conj {:identifier (str (symbol (str (custom-munge identifier) "2"))
                                                                     "(_main_var) == _main_var = _main_var")
                                                    :op-ns (-> op meta :op-ns)})
                             (str (symbol (str (custom-munge identifier) "2"))
                                  "(main_var)"))

                           ;; This means that we called invoke and we are calling a native
                           ;; function.
                           (map? expr)
                           (let [{:keys [:env] f :fn} expr
                                 op (eval
                                     `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
                                        [^Value ~'main-var]
                                        (process-config-operator ~f ~'main-var)))]
                             (swap! collector conj {:identifier (str (symbol (str (custom-munge identifier) "2"))
                                                                     "(_main_var) == _main_var = _main_var")
                                                    :op-ns (-> op meta :op-ns)})
                             (str (symbol (str (custom-munge identifier) "2"))
                                  (format "(main_var @@ %s)" (tla-edn/to-tla-value env))))

                           :else
                           result)))))

(defn temporal-property
  [identifier expr]
  (let [collector (atom [])
        form ((compile-temporal-property collector identifier) expr)]
    {:identifier (symbol (custom-munge identifier))
     :identifiers (mapv :identifier @collector)
     :op-nss (mapv :ns @collector)
     :form form
     :recife.operator/type :temporal-property}))

(def ^:private show-example-invariant
  "This operator is supposed to be used when the user does not pass
  any invariant or temporal property."
  (invariant ::show-example-invariant
             (fn [db]
               (not (every? #(= (:pc %) ::done)
                            (-> db ::procs vals))))))

(defn goto
  [db identifier]
  (assoc db :pc identifier))

(defn done
  [db]
  (assoc db :pc ::done))

(defn all-done?
  [db]
  (every? #(= (:pc %) ::done)
          (-> db ::procs vals)))

(defmacro implies
  "Like `when`, but it returns `true` if `condition` is falsy."
  [condition & body]
  `(if ~condition
     ~@body
     true))

(defn one-of
  "Tells Recife to choose one of the values as its initial value."
  ([values]
   (one-of nil values))
  ([identifier values]
   {::type ::one-of
    ::possible-values values
    ::identifier (some->> identifier hash Math/abs (str "G__") symbol)}))

(defn- module-template
  [{:keys [:init :next :spec-name :operators]}]
  (let [collected-ranges (atom #{})
        formatted-init-expressions (-> (->> init
                                            (walk/prewalk (fn [v]
                                                            (if (= (::type v) ::one-of)
                                                              (let [identifier (or (::identifier v )
                                                                                   (gensym))
                                                                    v' (assoc v ::identifier identifier)]
                                                                (swap! collected-ranges conj v')
                                                                (str identifier))
                                                              v)))
                                            tla-edn/to-tla-value
                                            str)
                                       ;; Remove quote strings from generated symbols so we can
                                       ;; use in a TLA+ mapping later.
                                       (str/replace #"\"G__\d+\"" (fn [s] (subs s 1 (dec (count s))))))
        formatted-collected-ranges (->> @collected-ranges
                                        ;; Sort it so it's more deterministic.
                                        (sort-by ::identifier)
                                        (mapv (fn [{:keys [::identifier ::possible-values]}]
                                                (format "%s \\in {%s}"
                                                        identifier
                                                        (->> possible-values
                                                             (mapv tla-edn/to-tla-value)
                                                             (str/join ", ")))))
                                        (str/join " /\\ "))
        formatted-init (if (seq @collected-ranges)
                         (format "%s /\\ main_var = %s" formatted-collected-ranges formatted-init-expressions)
                         (format "main_var = %s" formatted-init-expressions))
        formatted-invariants (->> operators
                                  (filter (comp #{:invariant} :recife.operator/type))
                                  (mapv #(format "%s\n\n%s ==\n  %s" (:identifier-2 %) (:identifier %) (:form %)))
                                  (str/join "\n\n"))
        formatted-constraints (->> operators
                                   (filter (comp #{:state-constraint} :recife.operator/type))
                                   (mapv #(format "%s\n\n%s ==\n  %s" (:identifier-2 %) (:identifier %) (:form %)))
                                   (str/join "\n\n"))
        config-invariants (or (some->> operators
                                       (filter (comp #{:invariant} :recife.operator/type))
                                       seq
                                       (mapv #(format "  %s" (:identifier %)))
                                       (str/join "\n")
                                       (str "INVARIANTS\n"))
                              "")
        config-constraints (or (some->> operators
                                        (filter (comp #{:state-constraint} :recife.operator/type))
                                        seq
                                        (mapv #(format "  %s" (:identifier %)))
                                        (str/join "\n")
                                        (str "CONSTRAINT\n"))
                               "")
        formatted-temporal-properties (->> operators
                                           (filter (comp #{:temporal-property} :recife.operator/type))
                                           (mapv #(format "%s\n\n%s ==\n  %s"
                                                          (->> (:identifiers %)
                                                               (str/join "\n\n"))
                                                          (:identifier %)
                                                          (:form %)))
                                           (str/join "\n\n"))
        config-temporal-properties (or (some->> operators
                                                (filter (comp #{:temporal-property} :recife.operator/type))
                                                seq
                                                (mapv #(format "  %s" (:identifier %)))
                                                (str/join "\n")
                                                (str "PROPERTY\n"))
                                       "")
        unchanged-helper-variables (or (some->> @collected-ranges
                                                seq
                                                (mapv (fn [{::keys [identifier]}] (format "%s' = %s" identifier identifier)))
                                                (str/join " /\\ ")
                                                (str " /\\ "))
                                       "")
        formatted-operators (->> (concat (->> operators
                                              (filter (comp #{:operator} :recife.operator/type))
                                              (mapv #(format "%s ==\n  %s%s"
                                                             (:identifier %)
                                                             (:form %)
                                                             (if (seq unchanged-helper-variables)
                                                               (str " /\\ " "(" unchanged-helper-variables ")")
                                                               ""))))
                                         (->> operators
                                              (filter (comp #{:tla-only} :recife.operator/type))
                                              (mapv #(format "%s ==\n  %s"
                                                             (:identifier %)
                                                             (:form %)))))
                                 (str/join "\n\n"))
        ;; `terminating` prevents deadlock
        terminating (format "(\\A self \\in DOMAIN main_var[%s]: main_var[%s][self][\"pc\"] = %s /\\ UNCHANGED vars)"
                            (tla-edn/to-tla-value ::procs)
                            (tla-edn/to-tla-value ::procs)
                            (tla-edn/to-tla-value ::done))
        helper-variables (or (some->> @collected-ranges
                                      seq
                                      (mapv (fn [{::keys [identifier]}] (format "%s" identifier)))
                                      (str/join ", ")
                                      (str ", "))
                             "")
        formatted-fairness (or
                            (some->> operators
                                     (filter :recife/fairness)
                                     seq
                                     (mapv (juxt :recife/fairness :recife.operator/name))
                                     (mapv (fn [[fairness name]]
                                             [:raw (format "\\A self \\in %s: %s_vars(%s(self, main_var) %s)"
                                                           (->> init ::procs keys
                                                                (mapv clojure.core/name) set tla-edn/to-tla-value)
                                                           (case fairness
                                                             :recife.fairness/weakly-fair "WF"
                                                             :recife.fairness/strongly-fair "SF")
                                                           name
                                                           (if (seq unchanged-helper-variables)
                                                             (format "/\\ (%s)" unchanged-helper-variables)
                                                             ""))]))
                                     (into [:and])
                                     parse)
                            "TRUE")]
    (int/i "
-------------------------------- MODULE #{spec-name} --------------------------------

EXTENDS Integers, TLC

VARIABLES main_var #{helper-variables}

vars == << main_var #{helper-variables} >>

__init ==
   #{formatted-init}

#{formatted-operators}

#{(:identifier next)} ==
   #{(:form next)}

Init == __init

Next == (#{(:identifier next)}) \\/ #{terminating}

#{formatted-invariants}

#{formatted-constraints}

#{formatted-temporal-properties}

Fairness ==
   #{formatted-fairness}

Spec == /\\ Init
        /\\ [][Next]_vars
        /\\ Fairness

MyView == << [x \\in DOMAIN main_var \\ {\"recife_SLASH_metadata\"} |-> main_var[x]] >>

=============================================================================

-------------------------------- CONFIG #{spec-name} --------------------------------

SPECIFICATION
   Spec

VIEW
   MyView

#{config-constraints}

#{config-invariants}

#{config-temporal-properties}

=============================================================================
")))

(defn- private-field
  ([obj fn-name-string]
   (let [m (.. obj getClass (getDeclaredField fn-name-string))]
     (. m (setAccessible true))
     (. m (get obj))))
  ([obj super-klass fn-name-string]
   (let [m (.. super-klass (getDeclaredField fn-name-string))]
     (. m (setAccessible true))
     (. m (get obj)))))

(def ^:private states-atom (atom {}))

(defn- save-state
  [tlc-state]
  (let [edn-state (->> tlc-state
                       .getVals
                       (some #(when (= (str (key %)) "main_var")
                                (val %)))
                       tla-edn/to-edn)]
    (swap! states-atom assoc-in
           [:states (.fingerPrint tlc-state)]
           {:state (dissoc edn-state :recife/metadata)
            :successors #{}})))

(defn- rank-state
  [tlc-state]
  (swap! states-atom update-in
         [:ranks (dec (.getLevel tlc-state))]
         (fnil conj #{}) (.fingerPrint tlc-state)))

(defn- save-successor
  [tlc-state tlc-successor]
  (let [successor-state (->> tlc-successor
                             .getVals
                             (some #(when (= (str (key %)) "main_var")
                                      (val %)))
                             tla-edn/to-edn)]
    (swap! states-atom update-in
           [:states (.fingerPrint tlc-state) :successors]
           conj
           [(.fingerPrint tlc-successor) (get-in successor-state [:recife/metadata :context])])))

(defn- write-state
  [_state-writer state successor _action-checks _from _length successor-state-new? _visualization _action]
  (when successor-state-new?
    (save-state successor))
  (rank-state state)
  (save-successor state successor))

#_ (clojure.edn/read-string (slurp "states-atom.edn"))

(defrecord EdnStateWriter [#_states-atom]
  tlc2.util.IStateWriter
  (writeState [_ state]
    (save-state state)
    (rank-state state))

  (writeState [this state successor successor-state-new?]
    (.writeState this state successor successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT))

  (^void writeState [this
                     ^tlc2.tool.TLCState state
                     ^tlc2.tool.TLCState successor
                     ^boolean successor-state-new?
                     ^tlc2.tool.Action action]
   (write-state this state successor nil 0 0 successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT action))

  (^void writeState [this
                     ^tlc2.tool.TLCState state
                     ^tlc2.tool.TLCState successor
                     ^boolean successor-state-new?
                     ^tlc2.util.IStateWriter$Visualization visualization]
   (write-state this state successor nil 0 0 successor-state-new? visualization nil))

  (writeState [this state successor action-checks from length successor-state-new?]
    (write-state this state successor action-checks from length successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT nil))

  (writeState [this state successor action-checks from length successor-state-new? visualization]
    (write-state this state successor action-checks from length successor-state-new? visualization nil))

  (close [_]
    (spit "states-atom.edn" @states-atom))

  (getDumpFileName [_])

  (isNoop [_] false)

  (isDot [_] false)

  (snapshot [_]))

(defn tlc-result-handler
  [tlc-runner]
  (let [recorder-atom (atom {:others []})
        record #(swap! recorder-atom merge %)
        recorder (reify IMessagePrinterRecorder
                   (record [_ code objects]
                     (condp = code
                       EC/TLC_INVARIANT_VIOLATED_BEHAVIOR
                       (record {:violation (merge
                                            {:type :invariant
                                             :name (to-edn-string (str/replace (first objects) #"_COLON_" ""))}
                                            (when (.exists (io/as-file invariant-data-filename))
                                              {:data (deserialize-obj invariant-data-filename)}))})

                       EC/TLC_MODULE_VALUE_JAVA_METHOD_OVERRIDE
                       (record {:error-messages (str/split (last objects) #"\n")})

                       EC/TLC_INVARIANT_VIOLATED_INITIAL
                       (record {:violation (merge
                                            {:type :invariant
                                             :name (to-edn-string (str/replace (first objects) #"_COLON_" ""))
                                             :initial-state? true}
                                            (when (.exists (io/as-file invariant-data-filename))
                                              {:data (deserialize-obj invariant-data-filename)}))})

                       EC/TLC_DEADLOCK_REACHED
                       (record {:violation {:type :deadlock}})

                       EC/TLC_PARSING_FAILED
                       (record {:error :parsing-failure})

                       EC/GENERAL
                       (record {:error :general
                                :error-messages (str/split (first objects) #"\n")})

                       (swap! recorder-atom update :others conj
                              [code objects]))))]
    (try
      (let [tlc (do (MP/setRecorder recorder)
                    (tlc-runner))
            _ (doto tlc
                #_(.setStateWriter (->EdnStateWriter))
                .process)
            recorder-info @recorder-atom
            _ (do (def tlc tlc)
                  (def recorder-info recorder-info))
            {:keys [:trace
                    :info]} (if-some [error-trace (-> (private-field tlc "recorder")
                                                      .getMCErrorTrace
                                                      (.orElse nil))]
                              (let [states (->> (.getStates error-trace)
                                                (mapv bean))
                                    stuttering-state (some #(when (:stuttering %) %) states)
                                    back-to-state (some #(when (:backToState %) %) states)]
                                {:trace (->> states
                                             ;; We don't want to return the state which
                                             ;; is stuttering, it's always equals to the
                                             ;; anterior.
                                             (remove :stuttering)
                                             ;; Same for backToState.
                                             (remove :backToState)
                                             (mapv (fn [state]
                                                     (->> state
                                                          :variables
                                                          (some #(when (= (.getName %) "main_var")
                                                                   (.getTLCValue %))))))
                                             (mapv tla-edn/to-edn)
                                             (map-indexed (fn [idx v] [idx v]))
                                             (into []))
                                 :info (merge (dissoc recorder-info :others)
                                              (cond
                                                stuttering-state
                                                {:violation {:type :stuttering
                                                             :state-number (:stateNumber stuttering-state)}}

                                                back-to-state
                                                {:violation {:type :back-to-state
                                                             :state-number (dec (:stateNumber back-to-state))}}))})
                              (let [initial-state (when (-> recorder-info :violation :initial-state?)
                                                    ;; If we had a initial state violation, `MCError` is `nil`,
                                                    ;; but the state is stored in the main checker.
                                                    (-> (private-field tlc2.TLCGlobals/mainChecker
                                                                       tlc2.tool.AbstractChecker
                                                                       "errState")
                                                        bean
                                                        :vals
                                                        (.get (UniqueString/uniqueStringOf "main_var"))
                                                        tla-edn/to-edn))]
                                (cond
                                  initial-state
                                  {:trace [[0 initial-state]]
                                   :info (dissoc recorder-info :others)}

                                  (:error recorder-info)
                                  {:trace :error
                                   :info (dissoc recorder-info :others)}

                                  :else
                                  {:trace :ok})))]
        {:trace (if (some-> tlc2.TLCGlobals/mainChecker .theFPSet .size)
                  trace
                  :error)
         :trace-info (if (-> info :violation :name (= ::show-example-invariant))
                       ;; If the user didn't ask for some check (through an
                       ;; invariant or a temporal property) and there was some
                       ;; violation (which in reality is just a example of a trace
                       ;; which satisfies the spec), then we get here.
                       (-> info
                           (dissoc :violation)
                           (assoc :trace-example? true))
                       info)
         :distinct-states (some-> tlc2.TLCGlobals/mainChecker .theFPSet .size)
         :generated-states (some-> tlc2.TLCGlobals/mainChecker .getStatesGenerated)
         :seed (private-field tlc "seed")
         :fp (private-field tlc "fpIndex")})
      (catch Exception ex
        (serialize-obj ex exception-filename)
        {:trace :error
         :trace-info (.getMessage ex)
         :distinct-states (some-> tlc2.TLCGlobals/mainChecker .theFPSet .size)
         :generated-states (some-> tlc2.TLCGlobals/mainChecker .getStatesGenerated)})
      (finally
        (MP/unsubscribeRecorder recorder)))))

(defn tlc-result-printer-handler
  [tlc-runner]
  (prn (tlc-result-handler tlc-runner)))

(defn- run-model*
  ([init-state next-operator operators]
   (run-model* init-state next-operator operators {}))
  ([init-state next-operator operators {:keys [:seed :fp :workers :tlc-args
                                               :raw-output? :run-local? :debug?
                                               :complete-response?]
                                        :or {workers :auto}}]
   ;; Do some validation.
   (some->> (m/explain schema/Operator next-operator)
            me/humanize
            (hash-map :error)
            (ex-info "Next operator is invalid")
            throw)
   (some->> (m/explain [:set schema/Operator] operators)
            me/humanize
            (hash-map :error)
            (ex-info "Some operator is invalid")
            throw)
   ;; Run model.
   (let [file (doto (File/createTempFile "eita" ".tla") .delete) ; we are interested in the random name of the folder
         abs-path (-> file .getAbsolutePath (str/split #"\.") first (str "/spec.tla"))
         _ (io/make-parents abs-path)
         [file-name file-type] (-> abs-path (str/split #"/") last (str/split #"\."))
         all-operators (if (->> operators
                                (filter (comp #{:temporal-property :invariant} :recife.operator/type))
                                seq)
                         operators
                         (conj operators show-example-invariant))
         module-contents (module-template
                          {:init init-state
                           :next next-operator
                           :operators all-operators
                           :spec-name file-name})
         _ (spit abs-path module-contents)
         tlc-opts (->> (cond-> []
                         seed (conj "-seed" seed)
                         fp (conj "-fp" fp)
                         workers (conj "-workers" (if (keyword? workers)
                                                    (name workers)
                                                    workers))
                         (seq tlc-args) (concat tlc-args))
                       (mapv str))
         ;; Load only the namespaces of the classes which are necessary
         ;; so we don't have interference from other namespaces.
         ;; It's also faster!
         loaded-classes (->> all-operators
                             (mapv (juxt :op-ns :op-nss))
                             flatten
                             (remove nil?)
                             vec)]
     (when debug? (println module-contents))
     ;; Delete any serialization file.
     (io/delete-file exception-filename true)
     (io/delete-file invariant-data-filename true)
     (cond
       run-local?
       (let [result (tlc-result-handler #(spec/run-spec abs-path
                                                        (str file-name "." file-type)
                                                        tlc-opts
                                                        {:run? false}))]
         (if (.exists (io/as-file exception-filename))
           (throw (deserialize-obj exception-filename))
           result))

       raw-output?
       (let [result (spec/run abs-path
                      (str file-name "." file-type)
                      tlc-opts
                      {:tlc-result-handler #'tlc-result-printer-handler
                       :complete-response? complete-response?
                       :loaded-classes loaded-classes})]
         (if (.exists (io/as-file exception-filename))
           (throw (deserialize-obj exception-filename))
           result))

       :else
       (let [result (spec/run abs-path
                      (str file-name "." file-type)
                      tlc-opts
                      {:tlc-result-handler #'tlc-result-printer-handler
                       :loaded-classes loaded-classes
                       :complete-response? true
                       :raw-args ["-DTLCCustomHandler=hillel.ch5_cache_2.Abc"]})
             output (atom [])]
         ;; Read line by line so we can stream the output to the user.
         (with-open [rdr (io/reader (:out result))]
           (binding [*in* rdr]
             (loop []
               (when-let [line (read-line)]
                 ;; If it's a EDN hashmap, do not print it.
                 (when-not (str/starts-with? line "{:")
                   (println line))
                 (swap! output conj line)
                 (recur)))))
         ;; Wait until the process finishes.
         @result
         ;; Throw exception or return EDN result.
         (if (.exists (io/as-file exception-filename))
           (throw (deserialize-obj exception-filename))
           (-> @output last edn/read-string)))))))

(defn timeline-diff
  [result-map]
  (update result-map :trace
          (fn [result]
            (if (vector? result)
              (->> result
                   (mapv last)
                   (partition 2 1)
                   (mapv #(ddiff/diff (first %) (second %)))
                   (mapv (fn [step]
                           (->> step
                                (filter (fn [[k v]]
                                          ;; If the key or the value contains a diff
                                          ;; instance, keep it.
                                          (or (instance? Mismatch k)
                                              (instance? Deletion k)
                                              (instance? Insertion k)
                                              (let [result-atom (atom false)]
                                                (walk/prewalk (fn [form]
                                                                (when (or (instance? Mismatch form)
                                                                          (instance? Deletion form)
                                                                          (instance? Insertion form))
                                                                  (reset! result-atom true))
                                                                form)
                                                              v)
                                                @result-atom))))
                                (into {}))))
                   (cons (last (first result)))
                   (map-indexed (fn [idx step] [idx step]))
                   vec)
              result))))

(defn print-timeline-diff
  [result-map]
  (if (vector? (:trace result-map))
    (-> result-map
        timeline-diff
        (update :trace ddiff/pretty-print))
    result-map))

(defn run-model
  ([init-global-state components]
   (run-model init-global-state components {}))
  ([init-global-state components opts]
   (schema/explain-humanized schema/RunModelComponents components "Invalid components")
   (let [processes (filter #(= (type %) ::Proc) components)
         invariants (filter #(= (type %) ::Invariant) components)
         constraints (filter #(= (type %) ::Constraint) components)
         properties (filter #(= (type %) ::Property) components)
         db-init (merge init-global-state
                        {::procs (->> processes
                                      (mapv :procs)
                                      (apply merge))})
         next' (tla ::next
                    (->> processes
                         (mapv (fn [{:keys [:steps-keys :procs]}]
                                 [:exists {'self (set (keys procs))}
                                  (into [:or]
                                        (->> steps-keys
                                             sort
                                             (mapv #(vector % 'self))))]))
                         (cons :or)
                         vec))
         operators (set (concat (mapcat :operators processes)
                                (map :operator invariants)
                                (map :operator constraints)
                                (map :operator properties)))]
     (run-model* db-init next' operators opts))))

(defmacro defproc
  [name params steps]
  `(def ~name
     (let [steps# ~steps
           params# ~params]
       (schema/explain-humanized schema/DefProc ['~name params# steps#] "Invalid `defproc` args")
       ^{:type ::Proc}
       {:name (keyword (str *ns*) ~(str name))
        :steps-keys (->> steps#
                         keys
                         (mapv #(if (vector? %)
                                  ;; If it's a vector, we are only interested
                                  ;; in the identifier.
                                  (first %)
                                  %)))
        :procs (->> (:procs params#)
                    (mapv (fn [proc#]
                            [proc# (:local params#)]))
                    (into {}))
        :operators (->> steps#
                        (mapv #(let [[k# opts#] (if (vector? (key %))
                                                  [(first (key %))
                                                   (or (second (key %)) {})]
                                                  [(key %)
                                                   {}])]
                                 (reg k#
                                   (with-meta opts#
                                     ~(meta name))
                                   (val %)))))})))

(defmacro definvariant
  [name f]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           f# ~f]
       ^{:type ::Invariant}
       {:name name#
        :invariant f#
        :operator (invariant name# f#)})))

(defmacro defproperty
  [name expr]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           expr# ~expr]
       ^{:type ::Property}
       {:name name#
        :property expr#
        :operator (temporal-property ::no-livelocks expr#)})))

(defmacro defconstraint
  [name f]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           f# ~f]
       ^{:type ::Constraint}
       {:name name#
        :constraint f#
        :operator (state-constraint name# f#)})))

(comment

  ;; We want to answer the question "Is RX programming using re-frame good
  ;; to model systems?"

  ;; We want it to be dynamic where you are able to create processes in
  ;; "runtime".

  ;; Everything is reified so you can manipulate anything that
  ;; it's hidden under the Recife functions. All the functions are just
  ;; normal Clojure code.

  ;; It's just an convention, but use namespaced keywords for "global" variables
  ;; and unamespaced keywords for "local" ones.

  ;; To mutate local variable, maybe you have to use some Recife function,
  ;; so the self is still hidden from you.

  ;; We can make it very flexible in the beginning, but we can constraint
  ;; later so it less overhead for simple models.

  ;; Different of TLA+/Pluscal, Recife requires you to explicitily define
  ;; sources of nondeterminism using `::r/or` or `::r/some` (TBD: `implies`
  ;; and `:in` should also be considered, see page 255, section 14.2.6
  ;; of https://lamport.azurewebsites.net/tla/book-02-08-08.pdf).

  ;; Another thing that you can do is to use the metadata (in `:recife/metadata`
  ;; for each step) to invoke your function with the same arguments as they were
  ;; invoked, allowing you to recreate all the steps. Just use the previous state
  ;; with a `[:recife/metadata :context]` step value.

  ;; TODO:
  ;; - Check if the arguments of `::r/or` could be simple functions (could
  ;;   naming be confusing for these cases?).
  ;; - Make (de)mangling of namespaces keywords (from)to tla values.
  ;; - One idea to make local variables work is just to override the
  ;;   implementattion of a map for `db`, see
  ;;   https://blog.wsscode.com/guide-to-custom-map-types/.
  ;; - [x] Make tla-edn accept keyword for a hashmap value (just use `str`).
  ;; - [x] Load dependencies correctly... `recife.core` does not want to load.

  ;; Recife TODO:
  ;; - [x] `Init` expression.
  ;; - [x] `reg` expression.
  ;; - [x] `Next` expression.
  ;; - [ ] Solve dependency problem (maybe by using vars instead of keywords?).
  ;; - [x] Define range for variables (one-of).
  ;; - [x] Add `init` properly.
  ;; - [x] Return output in EDN format (possibly with enhanced information about
  ;;       processes).
  ;; - [x] Temporal property.
  ;; - [ ] Add helper macros.
  ;; - [x] Encode back to state, stuttering, invariant which failed.
  ;; - [ ] Visualization of processes.
  ;; - [x] Check a way to avoid the error `Error: In evaluation, the identifier
  ;;       main_var is either undefined or not an operator.` when trying to
  ;;       override a operator called from a temporal property.
  ;; - [ ] It would be beneficial if side effects (e.g. `defop`) would be done
  ;;       when calling `r/run-model`.
  ;; - [x] Fix `Attempted to construct a set with too many elements (>1000000)."`.
  ;; - [ ] Profile parsing, things are slow in comparison with usual TLA+.
  ;; - [ ] (?) For performance reasons, make `db` be a custom map type.
  ;; - [x] Need to pass a seed so examples give us the same output.
  ;; - [ ] Keep track of which process is acting so we know who is doing what.
  ;; - [x] Show `fp`, `seed`.
  ;; - [ ] Create source map from TLA+ to clojure expression?
  ;; - [ ] Print logs to stdout.
  ;; - [ ] Warn about invalid `::r/procs` (e.g. no `:pc`, (use malli)).
  ;; - [ ] Add functions to add/remove/use/map processes.
  ;; - [ ] Make init value reference each other (see `ch5-cache4`). Maybe make
  ;;       it able to receive a function which creates a anonymous
  ;;       operator override?
  ;; - [x] Add fairness.
  ;; - [ ] Probably don't need identifier for `next`, remove it.
  ;; - [ ] Don't make the user use `::r`, make it work with non-namespaced
  ;;       keywords.
  ;; - [ ] Maybe create some `:dispatch-n` where the mutations happen
  ;;       sequentially so you don't need to be too much verbose.
  ;; - [ ] Maybe add some extension point for parallelization, e.g. divide
  ;;       initial states and trigger `n` aws lambda executions. Maybe it's not
  ;;       worth it as for real specs the majority of states is not from initial?
  ;; - [ ] Maybe start a JVM preemptively so startup time is amortized? It would require
  ;;       some smartness regarding state of the code and when to load things,
  ;;       maybe it's not worth it.
  ;; - [ ] Maybe start TLC in the same JVM by reloading its classes.
  ;; - [ ] Fix `Error: Found a Java class for module spec, but unable to read"`
  ;;       error.
  ;; - [ ] Maybe give names to anonymous functions in `defproc`?
  ;; - [ ] Check coverage (e.g. if some operator is never enabled or if it's
  ;;       never enabled in some context).

  ;; PRIORITIES:
  ;; - [x] Better show which process caused which changes, it's still confusing.
  ;; - [x] Create TypeOkInvariant with `malli`.
  ;; - [x] Stream log so user doesn't stay in the dark.
  ;; - [x] For `ch6-threads-1` and `ch5-cache-3`, make it more convenient to
  ;;       create and input operators into `run-model`.
  ;; - [x] Make everyone use new `run-model`.
  ;; - [x] Maybe use `Elle` (https://github.com/jepsen-io/elle) to check for powerful
  ;;       invariants (linearizability, serializability etc). It shows that you
  ;;       can combine different libraries to augment your model checking.
  ;; - [x] `::/procs` could be created from some DSL.
  ;; - [x] Add some form to visualize the generated trace.
  ;; - [x] Throw exception if the params of `r/defproc` are invalid. Also check
  ;;       that initial `:pc` corresponds to one of the declared steps.
  ;; - [x] When running the model, check if there are any globally repeated
  ;;       `proc`s.
  ;; - [x] Add Hillel license.
  ;; - [x] When using `Elle`, some java process get focus from OSX,
  ;;       this is annoying.
  ;; - [x] Allow user to pass a function which will be used as a `CONSTRAINT`.
  ;; - [x] Maybe let the user pass custom messages from the invariant to the
  ;;       output.

  ())
