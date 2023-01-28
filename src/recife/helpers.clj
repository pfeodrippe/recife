(ns recife.helpers
  (:require
   [clojure.math.combinatorics :as comb]
   [recife.core :as r]
   [tla-edn-2.core :as tla-edn])
  (:import
   (tlc2 TLCGlobals)
   (tlc2.util IdThread)
   (recife RecifeEdnValue)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)
#_(set! *unchecked-math* :warn-on-boxed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Trace
(defn get-level
  "Get current TLC level (depth in a trace)."
  []
  (or (some-> (IdThread/getCurrentState) .getLevel)
      ;; Initial state.
      0))

(defonce ^:private *trace-state
  (atom {:keyword->id {}
         :counter 1}))

(defn set-trace-value!
  "Set data for a trace (a hash map). You may use it to set data that you want
  to use for external purposes (e.g. statistics)."
  [k v]
  (let [thread (Thread/currentThread)
        value (RecifeEdnValue. v)
        {:keys [keyword->id]} (swap! *trace-state
                                     (fn [{:keys [keyword->id counter]
                                           :as v}]
                                       (if (contains? keyword->id k)
                                         v
                                         (-> v
                                             (update :counter inc)
                                             (update :keyword->id assoc k counter)))))
        id (get keyword->id k)]
    (cond
      (instance? IdThread thread)
      (.setLocalValue ^IdThread thread id value)

      (some? TLCGlobals/mainChecker)
      (.setAllValues TLCGlobals/mainChecker id value)

      :else
      (.setAllValues TLCGlobals/simulator id value))
    true))

(defn get-trace-value
  "Get data from a trace."
  [k]
  (when-let [id (get-in @*trace-state [:keyword->id k])]
    (let [thread (Thread/currentThread)]
      (some-> (cond
                (instance? IdThread thread)
                (.getLocalValue ^IdThread thread id)

                (some? TLCGlobals/mainChecker)
                (.getValue TLCGlobals/mainChecker 0 id)

                :else
                (.getLocalValue TLCGlobals/simulator id))
              tla-edn/to-edn))))

;;;;;;;;;;;;;;;;;;;;;; Helper functions for operators
(def ^:dynamic *env* {:global-vars [] :local-vars []})

(defn- add-global-vars
  [vs]
  (update *env* :global-vars
          #(apply conj % vs)))

(defn- add-local-vars
  [vs]
  (update *env* :local-vars
          #(apply conj % vs)))

(defn- invoke
  [form]
  (let [local-vars (->> (:local-vars *env*)
                        (mapv (fn [l]
                                (if (seq? l)
                                  (last l)
                                  l))))
        global-vars (->> (:global-vars *env*)
                         (mapv (fn [l]
                                 (if (seq? l)
                                   (last l)
                                   l))))]
    (if (or (::raw-form (meta form))
            (and (seq? form)
                 (not (keyword? (first form)))
                 (resolve (first form))
                 (or (::operator? (meta (resolve (first form))))
                     (= (namespace (symbol (resolve (first form)))) "recife.helpers"))))
      form
      (if (= (count global-vars) 2)
        `[:invoke (quote ~(->> local-vars
                               (mapv (fn [l] [(keyword l) l]))
                               (into {})))
          (fn [{:keys ~(vec local-vars)
                :as db#}
               db'#]
            (let [~(first global-vars) db#
                  ~(second global-vars) db'#
                  ~'--db db#
                  ~'--db' db'#]
              ~form))]
        `[:invoke (quote ~(->> local-vars
                               (mapv (fn [l] [(keyword l) l]))
                               (into {})))
          (fn [{:keys ~(vec local-vars)
                :as db#}]
            (let [~(first global-vars) db#]
              ~form))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quantifiers and functions
(defn- for-all*
  [bindings body]
  (let [actual-bindings {`'~(first bindings) (second bindings)}]
    `(binding [*env* (quote ~(add-local-vars (keys actual-bindings)))]
       (eval '[:for-all ~actual-bindings
               ~(binding [*env* (add-local-vars (keys actual-bindings))]
                  (if (> (count bindings) 2)
                    (for-all* (drop 2 bindings) body)
                    `(do ~(invoke body))))]))))

(defmacro for-all
  [bindings body]
  (for-all* bindings body))

(defn- for-some*
  [bindings body]
  (let [actual-bindings {`'~(first bindings) (second bindings)}]
    `(binding [*env* (quote ~(add-local-vars (keys actual-bindings)))]
       (eval '[:exists ~actual-bindings
               ~(binding [*env* (add-local-vars (keys actual-bindings))]
                  (if (> (count bindings) 2)
                    (for-some* (drop 2 bindings) body)
                    `(do ~(invoke body))))]))))

(defmacro for-some
  [bindings body]
  (for-some* bindings body))

(defmacro leads-to
  [source target]
  `[:leads-to
    ~(invoke source)
    ~(invoke target)])

(defmacro implies*
  "Note that this `implies` works in the TLA+ level, it should be used only
  in temporal properties."
  [source target]
  `[:implies
    ~(invoke source)
    ~(invoke target)])

(defmacro always
  [body]
  `[:always
    ~(invoke body)])

(defmacro eventually
  [body]
  `[:eventually
    ~(invoke body)])

(defmacro box
  [body]
  `[:box
    ~(invoke body)])

(defmacro fair
  [body]
  `[:fair
    ~(invoke body)])

(defmacro fair+
  [body]
  `[:fair+
    ~(invoke body)])

(defmacro and*
  [& body]
  `[:and ~@(mapv invoke body)])

(defmacro or*
  [& body]
  `[:or ~@(mapv invoke body)])

(defmacro not*
  [body]
  `[:not ~body])

(defmacro current-state
  []
  `[:current-state])

(defmacro next-state
  []
  `[:next-state])

(defmacro =*
  [v1 v2]
  `[:=
    ~(invoke v1)
    ~(invoke v2)])

(defmacro call
  "Invoke process step."
  [proc body]
  (let [proc' (eval proc)]
    (when-not (= (type proc') ::r/Proc)
      (throw (ex-info "First argument of `call` should be a proc (return of `recife.core/defproc`)"
                      {:non-proc proc})))
    (into [:or]
          (for [step-key (:steps-keys proc')]
            `(for-all [~'self (keys (:procs ~proc))]
               ^::raw-form
               [:call ~step-key '~'self
                ~(invoke body)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Defs
(defn- parse-decl
  [decl]
  (if (or (= (count decl) 2)
          (and (> (count decl) 2)
               (not (string? (first decl)))))
    [nil (first decl) (drop 1 decl)]
    [(first decl) (second decl) (drop 2 decl)]))

(defn -save-temporal-property-violation
  [name]
  (when (not (get-trace-value :recife/trace-violated?))
    (let [current-state (tlc2.util.IdThread/getCurrentState)]
      (r/save! :recife/violation
               {:trace (->> (->> (tlc2.module.TLCExt/getTrace nil nil nil
                                                              current-state
                                                              nil 0 nil)
                                 tla-edn-2.core/to-edn
                                 (mapv :main-var))
                            (map-indexed (fn [idx step] [idx step]))
                            vec)
                :type :temporal-property
                :name name})
      (set-trace-value! :recive/trace-violated? true)))
  false)

(defmacro defproperty
  {:arglists '([name doc-string? [db] body])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)
        #_ #_body (list
                   (concat
                    (cons `or*
                          body)
                    (list `(boolean (println :aaaa)))
                    #_(list (list 'eval (list '-save-temporal-property-violation
                                              (list 'symbol (str *ns*)
                                                    (str (quote name))))))))]
    `(r/-defproperty ~name
       (binding [*env* (quote ~(add-global-vars
                                (mapv (fn [p#] `(quote ~p#))
                                      params)))]
         (eval '~@body)))))

(defn -save-action-property-violation
  [name db']
  (when (not (get-trace-value :recife/trace-violated?))
    (let [current-state (tlc2.util.IdThread/getCurrentState)]
      (r/save! :recife/violation
               {:trace (->> (conj (->> (tlc2.module.TLCExt/getTrace nil nil nil
                                                                    current-state
                                                                    nil 0 nil)
                                       tla-edn-2.core/to-edn
                                       (mapv :main-var))
                                  ;; Append next state to trace.
                                  db')
                            (map-indexed (fn [idx step] [idx step]))
                            vec)
                :type :action-property
                :name name})
      (set-trace-value! :recive/trace-violated? true)))
  false)

(defmacro defaction-property
  {:arglists '([name doc-string? [db db'] body])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)
        _ (when-not (= (count params) 2)
            (throw (ex-info "Action constraints requires two arguments"
                            {:params params
                             :expected '[db db']})))
        body (list (cons `box (list (list 'or (cons 'do body)
                                          `(-save-action-property-violation
                                            (symbol ~(str *ns*) (str '~name))
                                            ~'--db')))))]
    `(r/-defaction-property ~name
       (binding [*env* (quote ~(add-global-vars
                                (mapv (fn [p#] `(quote ~p#))
                                      params)))]
         (eval '~@body)))))

(defn -save-invariant-violation
  [name]
  (when (not (get-trace-value :recife/trace-violated?))
    (let [current-state (tlc2.util.IdThread/getCurrentState)]
      (r/save! :recife/violation
               {:trace (->> (->> (tlc2.module.TLCExt/getTrace nil nil nil
                                                              current-state
                                                              nil 0 nil)
                                 tla-edn-2.core/to-edn
                                 (mapv :main-var))
                            (map-indexed (fn [idx step] [idx step]))
                            vec)
                :type :invariant
                :name name})
      (set-trace-value! :recive/trace-violated? true)))
  false)

(defmacro definvariant
  {:arglists '([name doc-string? [db] body])}
  [name & decl]
  (let [[doc-string params body] (parse-decl decl)]
    `(r/-definvariant ~name
       ~doc-string
       (fn ~params
         (or ~@body
             (-save-invariant-violation (symbol ~(str *ns*) (str '~name))))))))

(defmacro deffairness
  {:arglists '([name doc-string? [db] body])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)]
    `(r/-deffairness ~name
       (binding [*env* (quote ~(add-global-vars
                                (mapv (fn [p#] `(quote ~p#))
                                      params)))]
         (eval '~@body)))))

(defmacro defconstraint
  {:arglists '([name doc-string? [db] body])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)]
    `(r/-defconstraint ~name
       (fn ~params
         ~@body))))

(defmacro defaction-constraint
  {:arglists '([name doc-string? [db db'] body])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)]
    (when-not (= (count params) 2)
      (throw (ex-info "Action constraints requires two arguments"
                      {:params params
                       :expected '[db db']})))
    `(r/-defaction-constraint ~name
       (fn ~params
         ~@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Combine
(defn combine
  "Get all the possible combinations of the domain with the values.

  See https://www.learntla.com/core/functions.html#function-sets and
  `-maps` at https://github.com/Viasat/salt#maps.

  Returns a lazy value so things don't get stuck when the combination
  has many elements, values are randomized."
  [domain values]
  #_(def domain domain)
  #_(def values values)
  (let [domain->pairs (->> (for [a (shuffle (set domain))
                                 b (shuffle (set values))]
                             [a b])
                           (group-by first)
                           (into [])
                           shuffle)
        #_ #__ (def domain->pairs domain->pairs)
        domain-seq? (when (every? number? domain)
                      (let [domain-max (apply max domain)
                            domain-range (range (inc domain-max))]
                        (= (set domain-range)
                           (set domain))))]
    #_(def aaa (apply comb/cartesian-product (vals domain->pairs)))
    (->> (apply comb/cartesian-product (vals domain->pairs))
         (map #(if domain-seq?
                 (mapv last (sort-by first %))
                 (into {} %))))))

#_(take 10 (combine #{:a :b :c :d :e}
                    #{10 20 }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc
(defmacro with-features
  "Just some helper so feature flags are applied in compilation time.

  `features` is a set, only the first match is applied. The non-matching case
  is required (last element just like in a `case`)."
  [features & body]
  (when-not (odd? (count body))
    (throw (ex-info "Body count for `with-features` should always be odd (matches + default)"
                    {:body body})))

  (let [pairs (->> (drop-last body)
                   (partition-all 2 2))
        default (last body)]
    (loop [[[k v] & others] pairs]
      (cond
        (nil? k) default
        (contains? (set (eval features)) k) v
        :else (recur others)))))

(defmacro with-features-m
  "It's like `with-features`, but it does not evaluates the arguments, it just
  macroexpands it.

  Helpful for debugging."
  [features & body]
  `(macroexpand-1
    (quote
     (with-features ~features
       ~@body))))

(defn get-trace
  "Get trace (without index) from a result."
  [result]
  (mapv second (:trace result)))
