(ns recife.core
  (:require
   [alandipert.interpol8 :as int]
   [babashka.process :as p]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.repl :as repl]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [cognitect.transit :as t]
   [lambdaisland.deep-diff2 :as ddiff]
   [malli.core :as m]
   [malli.error :as me]
   [medley.core :as medley]
   [potemkin :refer [def-map-type]]
   [recife.buffer :as r.buf]
   [recife.schema :as schema]
   [recife.util :as u :refer [p*]]
   [taoensso.tufte :as tufte :refer [defnp p defnp-]]
   [tla-edn-2.core :as tla-edn]
   [tla-edn.spec :as spec]
   recife.class.recife-edn-value)
  (:import
   (java.io File)
   (lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion)
   (tlc2.output IMessagePrinterRecorder MP EC)
   (tlc2.value.impl Value StringValue ModelValue RecordValue FcnRcdValue IntValue
                    TupleValue SetEnumValue BoolValue IntervalValue)
   (util UniqueString)
   (recife RecifeEdnValue)))

(set! *warn-on-reflection* false)
#_(set! *warn-on-reflection* true)

(set! *unchecked-math* false)
#_(set! *unchecked-math* :warn-on-boxed)

(defn- custom-munge
  [v]
  (str/replace (munge v) #"\." "___"))

(defn- to-edn-string
  [v]
  (let [s (str/replace (str v)  #"___" ".")]
    (keyword (repl/demunge s))))

(defonce ^:private string-cache (atom {}))
(defonce ^:private keyword-cache (atom {}))
(defonce ^:private cache (atom {}))
(defonce ^:private values-cache (atom {}))
(defonce ^:private keys-cache (atom {}))

(defn- record-info-from-record
  [record]
  {:names (p* ::record-info-from-record--to-edn
              (mapv tla-edn/to-edn (.-names ^RecordValue record)))})

(declare build-record-map)

;; TODO: Maybe also keep a companion Clojure map?
;; TODO: We may store the index for a given k.
(def-map-type TlaRecordMap [record record-info]
  (get [_ k default-value]
       (p* ::tla-record-map--get
           (let [val-tla (or (get @keyword-cache k)
                             (let [result (UniqueString/of (custom-munge (symbol k)))]
                               (swap! keyword-cache assoc k result)
                               result))

                 ^{:tag "[Lutil.UniqueString;"}
                 names (.-names ^RecordValue record)

                 length (alength names)]
             (loop [idx 0]
               (if (< idx length)
                 (let [name' (aget names idx)]
                   (if (= (hash val-tla) (hash name'))
                     (tla-edn/to-edn
                      (aget ^{:tag "[Ltlc2.value.impl.Value;"} (.-values ^RecordValue record)
                            idx))
                     (recur (unchecked-inc idx))))
                 default-value)))))

  (assoc [this k v]
         (p* ::tla-record-map--assoc-1
             (let [val-tla (p* ::val-tla-unique
                               (or (get @keyword-cache k)
                                   (let [result (UniqueString/of (custom-munge (symbol k)))]
                                     (swap! keyword-cache assoc k result)
                                     result)))
                   ;; This is a much work solution (~5x) as strings are not
                   ;; interned, while unique strings are.
                   #_(p* ::val-tla
                         (.val (tla-edn/to-tla-value k)))

                   ^{:tag "[Lutil.UniqueString;"}
                   names (.-names ^RecordValue record)

                   ^{:tag "[Ltlc2.value.impl.Value;"}
                   values (.-values ^RecordValue record)

                   *new-key? (volatile! true)
                   length (alength names)

                   ^{:tag "[Lutil.UniqueString;"}
                   new-names (make-array UniqueString length)

                   ^{:tag "[Ltlc2.value.impl.Value;"}
                   new-values (make-array Value length)]
               (loop [idx 0]
                 (if (< idx length)
                   (do
                     (let [name' (aget names idx)]
                       (if (p ::hash
                              (= (hash val-tla) (hash name')))
                         (do (vreset! *new-key? false)
                             (aset new-names idx name')
                             (aset new-values idx (tla-edn/to-tla-value v)))
                         (do (aset new-names idx name')
                             (aset new-values idx (aget values idx)))))
                     (recur (unchecked-inc idx)))
                   (if @*new-key?
                     (build-record-map
                      (conj (into [] names)
                            val-tla)
                      (conj (into [] values)
                            (tla-edn/to-tla-value v)))
                     (build-record-map new-names new-values))))))
         ;; Old code, just for maintained for reference.
         #_(if (contains? (set @(:names @record-info)) k)
             (p* ::tla-record-map--assoc-1
                 (let [record'
                       (p* ::tla-record-map--assoc-1-dissoc
                           (.record ^TlaRecordMap (dissoc this k)))]
                   (build-record-map
                    (conj (into [] (.-names ^RecordValue record'))
                          (UniqueString/of (custom-munge (symbol k))))
                    (conj (into [] (.-values ^RecordValue record'))
                          (tla-edn/to-tla-value v)))))
             (p* ::tla-record-map--assoc-2
                 (build-record-map
                  (conj (into [] (.-names ^RecordValue record))
                        (UniqueString/of (custom-munge (symbol k))))
                  (conj (into [] (.-values ^RecordValue record))
                        (tla-edn/to-tla-value v))))))

  (dissoc [_ k]
          (p* ::tla-record-map--dissoc
              (let [names (transient [])
                    values (transient [])]
                (loop [record-names (.-names ^RecordValue record)
                       record-values (.-values ^RecordValue record)]
                  (let [n (first record-names)]
                    (when n
                      (when-not (= (tla-edn/to-edn n) k)
                        (conj! names n)
                        (conj! values (first record-values)))
                      (recur (rest record-names) (rest record-values)))))
                (build-record-map (persistent! names)
                                  (persistent! values)))))

  (keys [_]
        (p* ::tla-record-map--keys
            (:names record-info)))

  (empty [_]
         (p* ::tla-record-map--empty
             (build-record-map [] []))))

(defn- build-record-map
  ([record]
   (TlaRecordMap. record (record-info-from-record record)))
  ([names values]
   (let [record (RecordValue.
                 (tla-edn/typed-array UniqueString names)
                 (tla-edn/typed-array Value values)
                 false)]
     (TlaRecordMap. record (record-info-from-record record)))))

(defn- record-keys
  [v]
  (mapv #_#(or (get @keys-cache (str %))
               (p* ::k-no-cache
                   (let [result (tla-edn/-to-edn %)]
                     (swap! keys-cache assoc (str %) result)
                     result)))
        tla-edn/-to-edn
        (.-names ^RecordValue v)))

(defn- record-values
  [v]
  (mapv #(or (get @values-cache (str %))
             (p* ::v-no-cache
                 (let [result (tla-edn/-to-edn %)]
                   (swap! values-cache assoc (str %) result)
                   result)))
        #_tla-edn/-to-edn
        (.-values ^RecordValue v)))

(defmacro use-cache
  [v op]
  `(let [v# ~v]
     (or (get @cache v#)
         (let [result# ~op]
           (swap! cache assoc v# result#)
           result#))))

(defn- class-method
  [^Class klass ^String method-name]
  (let [m (.. klass (getDeclaredMethod method-name nil))]
    (.. m (setAccessible true))
    m))

(def ^:private fcn-rcd-is-tuple-method
  (class-method FcnRcdValue "isTuple"))

(defn ^:private fcn-rcd-is-tuple?
  [v]
  (boolean (.invoke ^java.lang.reflect.Method fcn-rcd-is-tuple-method v nil)))

(defn- private-field
  ([^Object obj ^String fn-name-string]
   (let [m (.. obj getClass (getDeclaredField fn-name-string))]
     (.. m (setAccessible true))
     (.. m (get obj))))
  ([^Object obj ^Class super-klass ^String fn-name-string]
   (let [m (.. super-klass (getDeclaredField fn-name-string))]
     (.. m (setAccessible true))
     (.. m (get obj)))))

(defrecord RecifeIntervalValue [low high])

(extend-protocol tla-edn/TLAPlusEdn
  RecordValue
  #_(-to-edn [v]
      (p* ::tla-edn--record
          (build-record-map v)))
  (-to-edn [v]
    (p* ::tla-edn--record
        (let [name->value (p* ::tla-edn--zipmap
                              (zipmap (p* ::tla-edn--zipmap-keys
                                          (record-keys v))
                                      (p* ::tla-edn--zipmap-values
                                          (record-values v))))]
          (if (= name->value {:tla-edn.record/empty? true})
            {}
            name->value))))

  IntValue
  (-to-edn [v]
    (p* ::tla-edn--int
        (.val v)))

  StringValue
  (-to-edn [v']
    (p* ::tla-edn--string
        (let [v (.val v')]
          (or (get @string-cache v)
              (let [s (str/replace (str v) #"___" ".")
                    k (keyword (repl/demunge s))
                    result (if (= k :recife/null)
                             nil
                             k)]
                (swap! string-cache assoc v result)
                result)))))

  UniqueString
  (-to-edn [v]
    (p* ::tla-edn--unique-string
        (or (get @string-cache v)
            (let [result (to-edn-string v)]
              (swap! string-cache assoc v result)
              result))))

  BoolValue
  (-to-edn [v]
    (p* ::tla-edn--bool
        (.getVal v)))

  FcnRcdValue
  (-to-edn [v]
    (p* ::tla-edn--fcn
        (if (fcn-rcd-is-tuple? v)
          (mapv tla-edn/-to-edn (.-values v))
          (zipmap (mapv tla-edn/-to-edn (.-domain v))
                  (mapv tla-edn/-to-edn (.-values v))))))

  RecifeEdnValue
  (-to-edn [v]
    (p* ::tla-edn--recife-value
        (.-state v)))

  IntervalValue
  (-to-edn [v]
    (p* ::tla-edn--interval-value
        (RecifeIntervalValue. (.-low v) (.-high v))))

  SetEnumValue
  (-to-edn [v]
    (p* ::tla-edn--set
        (set (mapv tla-edn/-to-edn (.toArray (.-elems v))))))

  TupleValue
  (-to-edn [v]
    (p* ::tla-edn--tuple
        (mapv tla-edn/-to-edn (.getElems v)))))

(extend-protocol tla-edn/EdnToTla
  RecifeIntervalValue
  (tla-edn/-to-tla-value
    [v]
    (p* ::to-tla--interval-value
        (IntervalValue. (:low v) (:high v))))

  clojure.lang.Keyword
  (tla-edn/-to-tla-value
    [v]
    (p* ::to-tla--keyword
        (RecifeEdnValue. v)
        #_(use-cache v (RecifeEdnValue. v))
        #_(or (get @cache v)
              (let [result (RecifeEdnValue. v)]
                (swap! cache assoc v result)
                result)))
    #_(StringValue. ^String (custom-munge (symbol v))))

  nil
  (tla-edn/-to-tla-value [_]
    (p* ::to-tla--null
        (RecifeEdnValue. nil)
        #_(tla-edn/to-tla-value :recife/null)))

  clojure.lang.Seqable
  (tla-edn/-to-tla-value [v]
    (p* ::to-tla--seqable
        (tla-edn/to-tla-value (into [] v))))

  clojure.lang.Symbol
  (tla-edn/-to-tla-value [v]
    (p* ::to-tla--symbol
        (ModelValue/make (str v))))

  TlaRecordMap
  (tla-edn/-to-tla-value [v]
    (p* ::to-tla--tla-record-map
        #_(.record v)
        (RecifeEdnValue. v)))

  clojure.lang.Ratio
  (tla-edn/-to-tla-value [v]
    (p* ::to-tla--ratio
        (RecifeEdnValue. v)))

  BigInteger
  (tla-edn/-to-tla-value [v]
    (p* ::to-tla--big-int
        (IntValue/gen v)))

  clojure.lang.APersistentMap
  (-to-tla-value [coll]
    (p* ::to-tla--map
        (RecifeEdnValue. coll)
        #_(cond
            (empty? coll)
            (tla-edn/-to-tla-value {:tla-edn.record/empty? true})

            (every? keyword? (keys coll))
            (RecordValue.
             (tla-edn/typed-array UniqueString (mapv #(-> % key ^StringValue tla-edn/to-tla-value .getVal) coll))
             (tla-edn/typed-array Value (mapv #(-> % val tla-edn/to-tla-value) coll))
             false)

            :else
            (FcnRcdValue.
             (tla-edn/typed-array Value (mapv #(-> % key tla-edn/to-tla-value) coll))
             (tla-edn/typed-array Value (mapv #(-> % val tla-edn/to-tla-value) coll))
             false))))

  clojure.lang.PersistentVector
  (-to-tla-value [coll]
    (p* ::to-tla--vector
        (TupleValue.
         (tla-edn/typed-array Value (mapv #(-> % tla-edn/-to-tla-value) coll)))))

  clojure.lang.PersistentList
  (-to-tla-value [coll]
    (p* ::to-tla--list
        (TupleValue.
         (tla-edn/typed-array Value (mapv #(-> % tla-edn/-to-tla-value) coll)))))

  clojure.lang.PersistentHashSet
  (-to-tla-value [coll]
    (p* ::to-tla--hash-set
        #_(RecifeEdnValue. coll)
        (SetEnumValue.
         (tla-edn/typed-array Value (mapv #(-> % tla-edn/-to-tla-value) coll))
         false)))

  String
  (-to-tla-value [v]
    (p* ::to-tla--string
        (throw (ex-info "We don't support strings for now, use a keyword instead."
                        {:string v}))
        #_(StringValue. v)))

  Boolean
  (-to-tla-value [v]
    (p* ::to-tla--boolean
        #_(RecifeEdnValue. v)
        (BoolValue. v)))

  Integer
  (-to-tla-value [v]
    (p* ::to-tla--integer
        (IntValue/gen v)))

  Long
  (-to-tla-value [v]
    (p* ::to-tla--long
        (IntValue/gen v))))

;; TODO: For serialized objecs, make it a custom random filename
;; so exceptions from concurrent executions do not mess with each other.
(def ^:private exception-filename
  ".recife-exception.ser")

(def ^:private invariant-data-filename
  ".recife-invariant-data.ser")

(def ^:private Lock (Object.))

(defn- serialize-obj [object ^String filename]
  (p* ::serialize-obj
      (locking Lock
        (when-not (.exists (io/as-file filename))
          (with-open [outp (-> (java.io.File. filename) java.io.FileOutputStream. java.io.ObjectOutputStream.)]
            (.writeObject outp object))))))

(defn- deserialize-obj [^String filename]
  (p* ::deserialize-obj
      (with-open [inp (-> (java.io.File. filename) java.io.FileInputStream. java.io.ObjectInputStream.)]
        (.readObject inp))))

(defn- process-operator*
  "Created so we can use the same \"meat\" when invoking the function
  in a context outside TLC."
  [identifier f self main-var]
  (p* ::process-operator*
      (let [global (dissoc main-var ::extra-args)
            #_ #__ (when (::extra-args main-var)
                     (throw (ex-info (str "po* -- " self " ==== " (::extra-args main-var) " ==== " (keys main-var)) {})))
            local (get-in main-var [::procs self])
            extra-args (::extra-args main-var)
            result (p ::result (f (p* ::result--merge
                                      (merge {:self self} global extra-args local))))
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
                              {:context [identifier (merge {:self self} extra-args)]})
                       {:context [identifier (merge {:self self} extra-args)]})]
        (p* ::process-operator*--return
            (if (nil? result)
              (dissoc (assoc main-var :recife/metadata metadata) ::extra-args)
              (merge
               ;; All namespaced keywords are global.
               (dissoc result-global :recife/metadata)
               ;; While unamespaced keywords are part of the
               ;; process as local variables.
               {::procs (merge (::procs result-global)
                               {self
                                ;; Remove `extra-args` and `self`
                                ;; so they don't become stateful.
                                (apply dissoc result-local :self (keys extra-args))})
                :recife/metadata metadata}))))))

(defn process-operator
  [identifier f self-tla ^RecordValue main-var-tla ^Value extra-args]
  (p* ::process-operator
      (try
        (let [self (p ::process-operator--self
                      (tla-edn/to-edn self-tla))
              main-var (p ::process-operator--main-var
                          (tla-edn/to-edn main-var-tla))
              extra-args (p ::process-operator--extra-args
                            (tla-edn/to-edn extra-args))
              result (p ::process-operator--result
                        (process-operator* identifier f self
                                           (if (set? extra-args)
                                             main-var
                                             (assoc main-var ::extra-args extra-args))))]
          (p ::process-operator--to-tla-value
             (tla-edn/to-tla-value result)))
        (catch Exception e
          (serialize-obj e exception-filename)
          (throw e)))))

(defn- process-operator-local*
  [f self main-var]
  (p* ::deserialize-obj
      (let [global main-var
            local (get-in main-var [::procs self])]
        (f (merge {:self self} global local)))))

(defn- process-operator-local
  [f self-tla ^Value main-var-tla]
  (try
    (let [self (tla-edn/to-edn self-tla)
          main-var (tla-edn/to-edn main-var-tla)
          result (process-operator-local* f self main-var)]
      (tla-edn/to-tla-value result))
    (catch Exception e
      (serialize-obj e exception-filename)
      (throw e))))

(defn process-config-operator
  [f ^Value main-var-tla]
  (p* ::process-config-operator
      (try
        (let [main-var (p* ::process-config-operator--main-var
                           (tla-edn/to-edn main-var-tla))
              result (p* ::process-config-operator--result
                         (f main-var))]
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
          (throw e)))))

(defn process-config-operator--primed
  [f ^Value main-var-tla ^Value main-var-tla']
  (p* ::process-config-operator--primed
      (try
        (let [main-var (p* ::process-config-operator--primed-main-var
                           (tla-edn/to-edn main-var-tla))
              main-var' (p* ::process-config-operator--primed-main-var
                            (tla-edn/to-edn main-var-tla'))
              result (p* ::process-config-operator--primed-result
                         (f main-var main-var'))]
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
          (throw e)))))

(defn process-local-operator
  [f ^Value main-var-tla]
  (p* ::process-local-operator
      (try
        (let [main-var (p ::local-op-to-edn-main-var-tla (tla-edn/to-edn main-var-tla))
              result (p ::local-op-result (f main-var))]
          (p ::local-op-to-tla (tla-edn/to-tla-value result)))
        (catch Exception e
          (serialize-obj e exception-filename)
          (throw e)))))

(defn parse
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
                                           (format "%s \\in (%s)"
                                                   (parse (custom-munge k) f)
                                                   (parse v f))))
                                   (str/join ", "))
                              (parse (last args) f))

                      :for-all
                      (format "\\A %s : (%s)"
                              (->> (first args)
                                   (mapv (fn [[k v]]
                                           (format "%s \\in (%s)"
                                                   (parse (custom-munge k) f)
                                                   (parse v f))))
                                   (str/join ", "))
                              (parse (last args) f))

                      :eventually
                      (format "<>(%s)" (parse (first args) f))

                      :always
                      (format "[](%s)" (parse (first args) f))

                      :leads-to
                      (format "(%s) ~> (%s)"
                              (parse (first args) f)
                              (parse (second args) f))

                      :invoke
                      (parse {:env (->> (first args)
                                        (mapv (fn [[k v]]
                                                [k (symbol (custom-munge v))]))
                                        (into {}))
                              :fn (last args)}
                             f)

                      :fair
                      (format "WF_vars(%s)"
                              (parse (first args) f))

                      :fair+
                      (format "SF_vars(%s)"
                              (parse (first args) f))

                      :call
                      (format "%s(%s%s)"
                              (custom-munge (first args))
                              (parse (second args))
                              (if-let [call-args (seq (drop 2 args))]
                                (str ", "
                                     (->> call-args
                                          (mapv #(parse % f))
                                          (str/join ", ")))
                                ""))

                      :or
                      (->> args
                           (mapv (comp #(str "(" % ")") #(parse % f)))
                           (str/join " \\/ "))

                      :and
                      (->> args
                           (mapv (comp #(str "(" % ")") #(parse % f)))
                           (str/join " /\\ "))

                      :if
                      (format "IF (%s) THEN (%s) ELSE (%s)"
                              (parse (first args) f)
                              (parse (second args) f)
                              (parse (last args) f))

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

                  (sequential? expr)
                  (tla-edn/to-tla-value (set expr))

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

(defn tla-repl
  [tla-expr]
  (let [tmp (java.nio.file.Files/createTempDirectory "repltest"
                                                     (into-array java.nio.file.attribute.FileAttribute []))]
    (-> (tlc2.REPL. tmp)
        (.processValue tla-expr)
        tla-edn/to-edn)))

#_(tla-repl "RandomSubset(2, [1..43 -> {TRUE, FALSE}])")
#_(tla-repl "RandomSubset(2, [2..5 -> {TRUE, FALSE}])")
#_(count (tla-repl "[1..43 -> {TRUE, FALSE}]"))
#_(tla-repl "CHOOSE x \\in RandomSubset(2, [2..43 -> {TRUE, FALSE}]): TRUE")

(defn context-from-state
  [state]
  (if (vector? state)
    (get-in state [1 :recife/metadata :context])
    (get-in state [:recife/metadata :context])))

(defmulti simulate
  "Simulate trace."
  (fn [context _state]
    (first context)))

(defmulti operator-local
  "Used for helper functions (e.g. to help with non-determinism).
  Instead of creating a new operator, we create a unique one,
  `recife-operator-local`, and dispatch it based on `:step` and
  `:key` fields."
  (fn [args]
    (select-keys args [:step :key])))

(spec/defop recife_operator_local {:module "spec"}
  [self ^Value params main-var]
  (p* ::recife_operator_local
      (let [{:keys [:f]} (operator-local (tla-edn/to-edn params))]
        (process-operator-local f self main-var))))

(spec/defop recife_check_extra_args {:module "spec"}
  [^Value main-var]
  (p* ::recife_check_extra_args
      #_(when (contains? (set (keys (tla-edn/to-edn main-var)))
                         ::extra-args)

          (throw (ex-info (str "extra --- " (tla-edn/to-tla-value
                                             (contains? (set (keys (tla-edn/to-edn main-var)))
                                                        ::extra-args))
                               " ==== "
                               (::extra-args (tla-edn/to-edn main-var))) {})))
      (tla-edn/to-tla-value
       (contains? (set (keys (tla-edn/to-edn main-var)))
                  ::extra-args))))

(spec/defop recife_check_inequality {:module "spec"}
  [^Value main-var ^Value main-var']
  (p* ::recife_check_inequality
      (tla-edn/to-tla-value
       (not= (dissoc (tla-edn/to-edn main-var) :recife/metadata)
             (dissoc (tla-edn/to-edn main-var') :recife/metadata)))))

(spec/defop recife_my_view {:module "spec"}
  [^Value main-var]
  (p* ::recife_my_view
      (tla-edn/to-tla-value (dissoc (tla-edn/to-edn main-var) :recife/metadata))))

;; \\A self \\in DOMAIN main_var[procs]: main_var[procs][self][\"pc\"] = done
(spec/defop recife_check_done {:module "spec"}
  [^Value main-var]
  (p* ::recife_check_done
      (tla-edn/to-tla-value
       (->> (::procs (tla-edn/to-edn main-var))
            vals
            (every? #(= (:pc %) ::done))))))

(spec/defop recife_check_pc {:module "spec"}
  [^Value main-var ^Value self ^Value identifier]
  #_[^Value main-var ^StringValue self ^StringValue identifier]
  (p* ::recife_check_pc
      (tla-edn/to-tla-value
       (= (get-in (tla-edn/to-edn main-var)
                  [::procs (tla-edn/to-edn self) :pc])
          (tla-edn/to-edn identifier)))))

(declare temporal-property)

(defn reg
  ([identifier expr]
   (reg identifier {} expr))
  ([identifier opts expr]
   (let [op (eval
             `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
                [~'self ~'main-var ~'extra-args]
                (process-operator ~identifier ~expr ~'self ~'main-var ~'extra-args)))]
     (defmethod simulate identifier
       [context state]
       (let [self (get-in context [1 :self])
             identifier (first context)
             context' (second context)]
         ;; This is the same function that we use at `process-operator`.
         (process-operator* identifier expr self (assoc (if (vector? state)
                                                          (second state)
                                                          state)
                                                        ::extra-args context'))))
     (with-meta
       {:identifier (str (symbol (str (custom-munge identifier) "2"))
                         "(self, _main_var, _extra_args) == self = self /\\ _main_var = _main_var /\\ _extra_args = _extra_args\n\n"
                         (symbol (custom-munge identifier))
                         "(self, _main_var)")
        :op-ns (-> op meta :op-ns)
        :recife.operator/name (str (symbol (custom-munge identifier)))
        :recife/fairness (cond
                           (or (-> expr meta :fair)
                               (-> opts meta :fair)) :recife.fairness/weakly-fair
                           (or (-> expr meta :fair+)
                               (-> opts meta :fair+)) :recife.fairness/strongly-fair)
        :recife/fairness-map (some->> (or (-> expr meta :fairness) (-> opts meta :fairness))
                                      (temporal-property (keyword (str (symbol identifier) "-fairness"))))
        :form (parse [:if [:raw "recife_check_extra_args(_main_var)"]
                      [:and
                       [:raw (format "recife_check_pc(_main_var, self, %s)"
                                     (str (tla-edn/to-tla-value identifier)))]
                       [:raw (str "main_var' = "
                                  (symbol (str (custom-munge identifier) "2"))
                                  "(self, _main_var, {})")]
                       [:raw "recife_check_inequality(_main_var, main_var')"]]
                      [:and
                       [:raw (format "recife_check_pc(_main_var, self, %s)"
                                     (str (tla-edn/to-tla-value identifier)))]
                       (if (seq opts)
                         [:raw (parse [:exists (->> opts
                                                    (mapv (fn [[k v]]
                                                            ;; Here we want to achieve non determinism.
                                                            [(symbol (str (custom-munge k)))
                                                             ;; If the value is empty, we return
                                                             ;; a set with a `nil` (`#{nil}`) so
                                                             ;; we don't have bogus deadlocks.
                                                             (cond
                                                               ;; If we have a coll here, then we want
                                                               ;; to get one of these hardcoded elements.
                                                               (coll? v)
                                                               (if (seq v)
                                                                 (parse (set v))
                                                                 #{nil})

                                                               ;; A keyword means that the user wants
                                                               ;; to use one of the global variables
                                                               ;; as a source of non determinism.
                                                               ;; TODO: Maybe if the user passes a
                                                               ;; unamespaced keyword we can use a
                                                               ;; local variable?
                                                               #_(keyword? v)
                                                               #_[:raw
                                                                  (let [mv (str " _main_var[" (parse v) "] ")]
                                                                    (format " IF %s = {} THEN %s ELSE %s"
                                                                            mv
                                                                            (parse #{nil})
                                                                            mv))]

                                                               ;; For functions, instead of creating a
                                                               ;; new operator, we use a defmethod used
                                                               ;; by a hardcoded operator (`recife-operator-local`).
                                                               (or (keyword? v)
                                                                   (fn? v))
                                                               (do (defmethod operator-local {:step identifier
                                                                                              :key k}
                                                                     [_]
                                                                     {:step identifier
                                                                      :key k
                                                                      :f (comp (fn [result]
                                                                                 (if (seq result)
                                                                                   (set result)
                                                                                   #{nil}))
                                                                               v)})
                                                                   [:raw (str " recife_operator_local"
                                                                              (format "(self, %s, _main_var)"
                                                                                      (tla-edn/to-tla-value
                                                                                       {:step identifier
                                                                                        :key k})))])

                                                               :else
                                                               (throw (ex-info "Unsupported type"
                                                                               {:step identifier
                                                                                :opts opts
                                                                                :value [k v]})))])))
                                       [:raw (str "\nmain_var' = "
                                                  (symbol (str (custom-munge identifier) "2"))
                                                  (format "(self, _main_var, %s)"
                                                          (->> (keys opts)
                                                               (mapv #(hash-map % (symbol (custom-munge %))))
                                                               (into {})
                                                               tla-edn/to-tla-value)))]])]
                         [:raw (str "main_var' = "
                                    (symbol (str (custom-munge identifier) "2"))
                                    "(self, _main_var, {})")])
                       ;; With it we can test deadlock.
                       [:raw "recife_check_inequality(_main_var, main_var')"]]])
        :recife.operator/type :operator}
       (merge (meta expr) (meta opts))))))

(defn invariant
  ([identifier expr]
   (invariant identifier nil expr))
  ([identifier doc-string expr]
   (let [op (eval
             `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
                [^Value ~'main-var]
                (process-config-operator ~expr ~'main-var)))]
     {:identifier (symbol (custom-munge identifier))
      :op-ns (-> op meta :op-ns)
      :identifier-2 (str (symbol (str (custom-munge identifier) "2")) "(_main_var) == _main_var = _main_var")
      :form (str (symbol (str (custom-munge identifier) "2"))
                 "(main_var)")
      :recife.operator/type :invariant
      :doc-string doc-string})))

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

(defn action-constraint
  "`expr` is a function of two arguments, the first one is the main var and the
  next one is the main var in the next state (primed)."
  [identifier expr]
  (let [op (eval
            `(spec/defop ~(symbol (str (custom-munge identifier) "2")) {:module "spec"}
               [^Value ~'main-var ^Value ~'main-var']
               (process-config-operator--primed ~expr ~'main-var ~'main-var')))]
    {:identifier (symbol (custom-munge identifier))
     :op-ns (-> op meta :op-ns)
     :identifier-2 (str (symbol (str (custom-munge identifier) "2")) "(_main_var, _main_var_2) == _main_var = _main_var /\\ _main_var_2 = _main_var_2")
     :form (str (symbol (str (custom-munge identifier) "2"))
                "(main_var, main_var')")
     :recife.operator/type :action-constraint}))

(defn- compile-temporal-property
  [collector identifier]
  (let [
        ;; With `counter` we are able to use many functions for the same
        ;; temporal property.
        counter (atom 100)]
    (fn [id-and-args]
      (parse id-and-args (fn [[result expr]]
                           (cond
                             (fn? expr)
                             (let [op (eval
                                       `(spec/defop ~(symbol (str (custom-munge identifier)
                                                                  @counter))
                                          {:module "spec"}
                                          [^Value ~'main-var]
                                          (process-local-operator ~expr ~'main-var)))]
                               (swap! collector conj {:identifier (str (symbol (str (custom-munge identifier)
                                                                                    @counter))
                                                                       "(_main_var) == _main_var = _main_var")
                                                      :op-ns (-> op meta :op-ns)})
                               (try
                                 (str (symbol (str (custom-munge identifier)
                                                   @counter))
                                      "(main_var)")
                                 (finally
                                   (swap! counter inc))))

                             ;; This means that we called invoke and we are calling a native
                             ;; function.
                             (map? expr)
                             (let [{:keys [:env] f :fn} expr
                                   op (eval
                                       `(spec/defop ~(symbol (str (custom-munge identifier)
                                                                  @counter))
                                          {:module "spec"}
                                          [^Value ~'main-var]
                                          (process-local-operator ~f ~'main-var)))]
                               (swap! collector conj {:identifier (str (symbol (str (custom-munge identifier)
                                                                                    @counter))
                                                                       "(_main_var) == _main_var = _main_var")
                                                      :op-ns (-> op meta :op-ns)})
                               (try
                                 (str (symbol (str (custom-munge identifier)
                                                   @counter))
                                      (if (seq env)
                                        (format "(main_var @@ %s)"
                                                (tla-edn/to-tla-value env))
                                        "(main_var)"))
                                 (finally
                                   (swap! counter inc))))

                             :else
                             result))))))

(defn temporal-property
  [identifier expr]
  (let [collector (atom [])
        form ((compile-temporal-property collector identifier) expr)]
    {:identifier (symbol (custom-munge identifier))
     :identifiers (mapv :identifier @collector)
     :op-nss (mapv :ns @collector)
     :form form
     :recife.operator/type :temporal-property}))

(defn fairness
  [identifier expr]
  (merge (temporal-property identifier expr)
         {:recife.operator/type :fairness}))

(defn goto
  [db identifier]
  (assoc db :pc identifier))

(defn done
  "Finishes step so deadlock is not triggered for this step."
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
    ::identifier (when-let [v (some->> identifier hash)]
                   (->> (Math/abs ^Integer v)
                        (str "G__")
                        symbol))}))

(defn- module-template
  [{:keys [:init :next :spec-name :operators]}]
  (let [collected-ranges (atom #{})
        formatted-init-expressions (-> (->> init
                                            (walk/prewalk (fn [v]
                                                            (if (= (::type v) ::one-of)
                                                              (let [identifier (or (::identifier v)
                                                                                   (gensym))
                                                                    v' (assoc v ::identifier identifier)]
                                                                (swap! collected-ranges conj v')
                                                                (keyword identifier))
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
        formatted-action-constraints (->> operators
                                          (filter (comp #{:action-constraint} :recife.operator/type))
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
        config-action-constraints (or (some->> operators
                                               (filter (comp #{:action-constraint} :recife.operator/type))
                                               seq
                                               (mapv #(format "  %s" (:identifier %)))
                                               (str/join "\n")
                                               (str "ACTION_CONSTRAINT\n"))
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
        formatted-fairness-operators (or (some->> operators
                                                  (filter (comp #{:fairness} :recife.operator/type))
                                                  seq
                                                  (mapv #(format "%s\n\n%s ==\n  %s"
                                                                 (->> (:identifiers %)
                                                                      (str/join "\n\n"))
                                                                 (:identifier %)
                                                                 (:form %)))
                                                  (str/join "\n\n"))
                                         "")
        #_ #_ _ (def operators operators)
        fairness-identifiers (or (some->> operators
                                          (filter (comp #{:fairness} :recife.operator/type))
                                          seq
                                          (mapv :identifier)
                                          (str/join " /\\ ")
                                          (str " /\\ "))
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
        terminating  "(recife_check_done(main_var) /\\ UNCHANGED vars)"
        helper-variables (or (some->> @collected-ranges
                                      seq
                                      (mapv (fn [{::keys [identifier]}] (format "%s" identifier)))
                                      (str/join ", ")
                                      (str ", "))
                             "")
        fairness-operators (some->> operators
                                    (filter :recife/fairness)
                                    seq
                                    (mapv (juxt :recife/fairness :recife.operator/name))
                                    (mapv (fn [[fairness name]]
                                            [:raw (format "\\A self \\in %s: %s_vars(%s(self, main_var) %s)"
                                                          (->> (keys (::procs init))
                                                               set
                                                               tla-edn/to-tla-value)
                                                          (case fairness
                                                            :recife.fairness/weakly-fair "WF"
                                                            :recife.fairness/strongly-fair "SF")
                                                          name
                                                          (if (seq unchanged-helper-variables)
                                                            (format "/\\ (%s)" unchanged-helper-variables)
                                                            ""))])))
        formatted-fairness (or
                            (some->> (concat fairness-operators
                                             (some->> operators
                                                      (filter :recife/fairness-map)
                                                      seq
                                                      (mapv (fn [{:keys [:recife/fairness-map]}]
                                                              [:raw (:form fairness-map)]))))
                                     seq
                                     (into [:and])
                                     parse)
                            "TRUE")
        other-identifiers (or (some->> operators
                                       (filter :recife/fairness-map)
                                       seq
                                       (mapcat (fn [{:keys [:recife/fairness-map]}]
                                                 (:identifiers fairness-map)))
                                       (str/join "\n\n"))
                              "")]
    (int/i "
-------------------------------- MODULE #{spec-name} --------------------------------

EXTENDS Integers, TLC

VARIABLES main_var #{helper-variables}

vars == << main_var #{helper-variables} >>

recife_operator_local(_self, _params, _main_var) == _self = _self /\\ _params = _params /\\ _main_var = _main_var

recife_check_extra_args(_main_var) == _main_var = main_var

recife_check_inequality(_main_var, _main_var_2) == _main_var = main_var /\\ _main_var_2 = _main_var_2

recife_my_view(_main_var) == _main_var = main_var

recife_check_done(_main_var) == _main_var = main_var

recife_check_pc(_main_var, self, identifier) == _main_var = main_var /\\ self = self /\\ identifier = identifier

#{other-identifiers}

__init ==
   #{formatted-init}

#{formatted-operators}

#{(:identifier next)} ==
   #{(:form next)}

Init == __init

Next == (#{(:identifier next)}) \\/ #{terminating}

#{formatted-invariants}

#{formatted-constraints}

#{formatted-action-constraints}

#{formatted-temporal-properties}

#{formatted-fairness-operators}

Fairness ==
   #{formatted-fairness}#{fairness-identifiers}

Spec == /\\ Init
        /\\ [][Next]_vars
        /\\ Fairness

MyView == << recife_my_view(main_var) >>

=============================================================================

-------------------------------- CONFIG #{spec-name} --------------------------------

SPECIFICATION
   Spec

VIEW
   MyView

#{config-constraints}

#{config-action-constraints}

#{config-invariants}

#{config-temporal-properties}

=============================================================================
")))

;; EdnStateWriter
(def ^:private edn-states-atom (atom {}))

(defn- edn-save-state
  [^tlc2.tool.TLCState tlc-state]
  (let [edn-state (->> tlc-state
                       .getVals
                       (some #(when (= (str (key %)) "main_var")
                                (val %)))
                       tla-edn/to-edn)]
    (swap! edn-states-atom assoc-in
           [:states (.fingerPrint tlc-state)]
           {:state (dissoc edn-state :recife/metadata)
            :successors #{}})))

(defn- edn-rank-state
  [^tlc2.tool.TLCState tlc-state]
  (swap! edn-states-atom update-in
         [:ranks (dec (.getLevel tlc-state))]
         (fnil conj #{}) (.fingerPrint tlc-state)))

(defn- edn-save-successor
  [^tlc2.tool.TLCState tlc-state ^tlc2.tool.TLCState tlc-successor]
  (let [successor-state (->> tlc-successor
                             .getVals
                             (some #(when (= (str (key %)) "main_var")
                                      (val %)))
                             tla-edn/to-edn)]
    (swap! edn-states-atom update-in
           [:states (.fingerPrint tlc-state) :successors]
           conj
           [(.fingerPrint tlc-successor) (get-in successor-state [:recife/metadata :context])])))

(defn- edn-write-state
  [_state-writer state successor _action-checks _from _length successor-state-new? _visualization _action]
  (when successor-state-new?
    (edn-save-state successor))
  (edn-rank-state state)
  (edn-save-successor state successor))

#_ (clojure.edn/read-string (slurp "edn-states-atom.edn"))

(defrecord EdnStateWriter [#_edn-states-atom]
  tlc2.util.IStateWriter
  (writeState [_ state]
    (edn-save-state state)
    (edn-rank-state state))

  (writeState [this state successor successor-state-new?]
    (.writeState this state successor successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT))

  (^void writeState [this
                     ^tlc2.tool.TLCState state
                     ^tlc2.tool.TLCState successor
                     ^boolean successor-state-new?
                     ^tlc2.tool.Action action]
   (edn-write-state this state successor nil 0 0 successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT action))

  (^void writeState [this
                     ^tlc2.tool.TLCState state
                     ^tlc2.tool.TLCState successor
                     ^boolean successor-state-new?
                     ^tlc2.util.IStateWriter$Visualization visualization]
   (edn-write-state this state successor nil 0 0 successor-state-new? visualization nil))

  (writeState [this state successor action-checks from length successor-state-new?]
    (edn-write-state this state successor action-checks from length successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT nil))

  (writeState [this state successor action-checks from length successor-state-new? visualization]
    (edn-write-state this state successor action-checks from length successor-state-new? visualization nil))

  (close [_]
    (spit "edn-states-atom.edn" @edn-states-atom))

  (getDumpFileName [_])

  (isNoop [_] false)

  (isDot [_] false)

  (snapshot [_]))

;; FileStateWriter
(defn- file-sw-save-state
  [{:keys [:edn-states-atom]} ^tlc2.tool.TLCState tlc-state]
  (let [edn-state (->> tlc-state
                       .getVals
                       (some #(when (= (str (key %)) "main_var")
                                (val %)))
                       tla-edn/to-edn)]
    (swap! edn-states-atom assoc-in
           [:states (.fingerPrint tlc-state)]
           {:state (dissoc edn-state :recife/metadata)
            :successors #{}})))

(defn- file-sw-rank-state
  [{:keys [:edn-states-atom]} ^tlc2.tool.TLCState tlc-state]
  (swap! edn-states-atom update-in
         [:ranks (dec (.getLevel tlc-state))]
         (fnil conj #{}) (.fingerPrint tlc-state)))

(defn- file-sw-save-successor
  [{:keys [:edn-states-atom]} ^tlc2.tool.TLCState tlc-state ^tlc2.tool.TLCState tlc-successor]
  (let [successor-state (->> tlc-successor
                             .getVals
                             (some #(when (= (str (key %)) "main_var")
                                      (val %)))
                             tla-edn/to-edn)]
    (swap! edn-states-atom update-in
           [:states (.fingerPrint tlc-state) :successors]
           conj
           [(.fingerPrint tlc-successor) (get-in successor-state [:recife/metadata :context])])))

(defn- file-sw-write-state
  [this state successor _action-checks _from _length successor-state-new? visualization _action]
  ;; If it's stuttering, we don't put it as a successor.
  (when-not (= visualization tlc2.util.IStateWriter$Visualization/STUTTERING)
    (when successor-state-new?
      (file-sw-save-state this successor))
    (file-sw-rank-state this state)
    (file-sw-save-successor this state successor)))

;; TODO: Move the state writers to a `state-writers` ns.
(defrecord FileStateWriter [writer output-stream edn-states-atom file-path]
  tlc2.util.IStateWriter
  (writeState [this state]
    (file-sw-save-state this state)
    (file-sw-rank-state this state))

  (writeState [this state successor successor-state-new?]
    (.writeState this state successor successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT))

  (^void writeState [this
                     ^tlc2.tool.TLCState state
                     ^tlc2.tool.TLCState successor
                     ^boolean successor-state-new?
                     ^tlc2.tool.Action action]
   (file-sw-write-state this state successor nil 0 0 successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT action))

  (^void writeState [this
                     ^tlc2.tool.TLCState state
                     ^tlc2.tool.TLCState successor
                     ^boolean successor-state-new?
                     ^tlc2.util.IStateWriter$Visualization visualization]
   (file-sw-write-state this state successor nil 0 0 successor-state-new? visualization nil))

  (writeState [this state successor action-checks from length successor-state-new?]
    (file-sw-write-state this state successor action-checks from length successor-state-new? tlc2.util.IStateWriter$Visualization/DEFAULT nil))

  (writeState [this state successor action-checks from length successor-state-new? visualization]
    (file-sw-write-state this state successor action-checks from length successor-state-new? visualization nil))

  (close [_]
    (t/write writer @edn-states-atom)
    (reset! edn-states-atom nil)
    (.close ^java.io.OutputStream output-stream))

  (getDumpFileName [_])

  (isNoop [_] false)

  (isDot [_] false)

  (snapshot [_]))

(defn- states-from-file
  [file-path]
  (with-open [is (io/input-stream file-path)]
    (let [reader (t/reader is :msgpack)]
      (t/read reader))))

(defn states-from-result
  [{:keys [:recife/transit-states-file-path]}]
  (states-from-file transit-states-file-path))

(defn random-traces-from-states
  ([states]
   (random-traces-from-states states 10))
  ([states max-number-of-paths]
   (if (< max-number-of-paths 1)
     []
     (let [initial-states (get-in states [:ranks 0])]
       (loop [[current-state] [(rand-nth (vec initial-states)) nil]
              ;; `visited` is used to avoid loops.
              visited #{}
              ;; TODO: It can be improved to check for visited state successor.
              ;; Currently it does not return all possible paths (I wonder if
              ;; this is something we want), but it's fine for now.
              removed #{}
              paths [[[current-state nil]]]
              counter 0]
         (let [visited' (conj visited current-state)
               successor-state (some->> (get-in states [:states current-state :successors])
                                        (remove #(contains? visited' (first %)))
                                        vec
                                        seq
                                        rand-nth)
               ;; If there is no successor state and the current path is a prefix
               ;; of some existing path, we can get rid of this path.
               paths' (if (and (nil? successor-state)
                               (some #(= (take (count (last paths)) %)
                                         (last paths))
                                     (drop-last paths)))
                        (vec (drop-last paths))
                        paths)
               ;; If all the successors are visited or removed, then we don't
               ;; need to check this state again.
               removed' (if (->> (get-in states [:states current-state :successors])
                                 (mapv first)
                                 (every? #(contains? (set/union removed visited') %)))
                          (conj removed current-state)
                          removed)]
           (cond
             (some? successor-state)
             (recur successor-state
                    visited'
                    removed'
                    (update paths' (dec (count paths')) (comp vec conj) successor-state)
                    (inc counter))

             ;; Stop if there are no more paths to see or if we have the desired
             ;; number of paths.
             (or (= (count paths') max-number-of-paths)
                 (= (count removed') (count (:states states)))
                 (not (->> (vec initial-states) (remove #(contains? removed' %)) seq)))
             ;; Return maps instead of positional data.
             ;; We make the output the same form as `:trace` when we have some
             ;; violation.
             (->> paths'
                  (mapv #(->> %
                              (map-indexed (fn [idx [state-fp context]]
                                             [idx (-> (get-in states [:states state-fp :state])
                                                      (merge (when (some? context)
                                                               {:recife/metadata {:context context}}))
                                                      (with-meta {:fingerprint state-fp}))]))
                              vec)))

             :else
             ;; Start a new path.
             (let [state (->> (vec initial-states)
                              (remove #(contains? removed' %))
                              rand-nth)]
               (recur [state nil]
                      #{}
                      removed'
                      (assoc paths' (count paths') [[state nil]])
                      (inc counter))))))))))

(defn tlc-result-handler
  "This function is a implementation detail, you should not use it.
  It handles the TLC object and its result, generating the output we see when
  calling `run-model`."
  [tlc-runner]
  (let [recorder-atom (atom {:others []})
        record #(swap! recorder-atom merge %)
        recorder (reify IMessagePrinterRecorder
                   (record [_ code objects]
                     (p* ::tlc-result--recorder
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
                                  [code objects])))))]
    (try
      (let [tlc (do (MP/setRecorder recorder)
                    (tlc-runner))
            ;; Read opts file from JVM property.
            opts (some-> (System/getProperty "RECIFE_OPTS_FILE_PATH") slurp edn/read-string)
            _ (when-let [channel-path (::channel-file-path opts)]
                (println "Creating channel path at" channel-path)
                (r.buf/start-client-loop!)
                (r.buf/set-buf (r.buf/buf-create {:file (io/file channel-path)
                                                  :truncate true})))
            state-writer (when (or (:dump-states? opts)
                                   ;; If we want to show a trace example (if no
                                   ;; violation is found), then we have to
                                   ;; generate the states file.
                                   (:trace-example? opts))
                           (let [file-path (.getAbsolutePath (File/createTempFile "transit-output" ".msgpack"))
                                 os (io/output-stream file-path)
                                 state-writer (->FileStateWriter (t/writer os :msgpack) os (atom {}) file-path)]
                             (.setStateWriter ^tlc2.TLC tlc state-writer)
                             state-writer))
            *closed-properly? (atom false)
            _ (.addShutdownHook (Runtime/getRuntime)
                                (Thread. ^Runnable (fn []
                                                     #_(when-let [ch-buf (some-> @r.buf/*client-channel .buf)]
                                                         (println "Remaining data size in buffer:" (count ch-buf)))
                                                     (when-not @*closed-properly?
                                                       (println "\n---- Closing child Recife process ----\n")
                                                       (let [pstats @@u/pd]
                                                         (when (:stats pstats)
                                                           (println (str "\n\n" (tufte/format-pstats pstats)))))
                                                       (println {:unfinished? true})))))
            _ (do (p* ::process
                      (doto ^tlc2.TLC tlc
                        .process))
                  (reset! *closed-properly? true)
                  (let [pstats @@u/pd]
                    (when (:stats pstats)
                      (println (str "\n\n" (tufte/format-pstats pstats))))))
            recorder-info @recorder-atom
            _ (do (def tlc tlc)
                  (def recorder-info recorder-info))

            simulator tlc2.TLCGlobals/simulator

            {:keys [:trace
                    :info]} (if-some [error-trace (-> ^tlc2.output.ErrorTraceMessagePrinterRecorder
                                                      (private-field tlc "recorder")
                                                      .getMCErrorTrace
                                                      (.orElse nil))]
                              (let [states (->> (.getStates ^tlc2.model.MCError error-trace)
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
                                                          (some #(when (= (.getName ^tlc2.model.MCVariable %)
                                                                          "main_var")
                                                                   (.getTLCValue ^tlc2.model.MCVariable %))))))
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

        ;; `do` for debugging.
        #_(do (def recorder-value @recorder-atom)
              (def simulator simulator))

        (-> {:trace (cond
                      (and (nil? (some-> ^tlc2.tool.AbstractChecker tlc2.TLCGlobals/mainChecker .theFPSet .size))
                           (nil? simulator))
                      :error

                      (and (= trace :ok) (:trace-example? opts))
                      (-> (:file-path state-writer)
                          states-from-file
                          random-traces-from-states
                          rand-nth)

                      :else
                      trace)
             :trace-info (if (and (nil? info) (:trace-example? opts))
                           {:trace-example? true}
                           info)
             :distinct-states (some-> tlc2.TLCGlobals/mainChecker .theFPSet .size)
             :generated-states (some-> tlc2.TLCGlobals/mainChecker .getStatesGenerated)
             :seed (private-field tlc "seed")
             :fp (private-field tlc "fpIndex")}
            (merge (when simulator
                     {:simulation
                      {:states-count (long (private-field simulator "numOfGenStates"))
                       :traces-count (long (private-field simulator "numOfGenTraces"))}}))
            (medley/assoc-some :recife/transit-states-file-path (:file-path state-writer))))

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

;; We create a record just so we can use `simple-dispatch`.
(defrecord RecifeModel [v]
  java.io.Closeable
  (close [_]
    (.close v))

  clojure.lang.IDeref
  (deref [_]
    @v)

  Object
  (toString [_]
    "#RecifeModel {}"))

(defmethod print-method RecifeModel
  [_o ^java.io.Writer w]
  (.write w "#RecifeModel {}"))

(defmethod pp/simple-dispatch RecifeModel
  [^RecifeModel _]
  (pr {:type `RecifeModel}))

(defonce ^:private *current-model-run (atom nil))

(defn halt!
  "Halt model run. If no arg is passed, it halts the existing or last run (if
  existing).

  Returns @model-run."
  ([]
   (halt! @*current-model-run))
  ([model-run]
   (some-> model-run .close)
   (when (some? model-run)
     @model-run)))

(defn get-result
  "Wait for model run result and return it. If no arg is passed, it returns
  the last model run."
  ([]
   (get-result @*current-model-run))
  ([model-run]
   @model-run))

(defn- run-model*
  "Run model. Model run is async by default, use `halt!` to interrupt it."
  ([init-state next-operator operators]
   (run-model* init-state next-operator operators {}))
  ([init-state next-operator operators {:keys [seed fp workers tlc-args
                                               raw-output? run-local? debug?
                                               complete-response?
                                               no-deadlock
                                               depth async simulate generate
                                               use-buffer
                                               ;; Below opts are used in the child
                                               ;; process.
                                               trace-example? dump-states?]
                                        :as opts
                                        :or {workers :auto
                                             async true}}]
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
   (when-let [simple-keywords (seq (remove qualified-keyword? (keys init-state)))]
     (throw (ex-info "For the initial state, all the keywords should be namespaced. Recife uses the convention in which namespaced keywords are global ones, while other keywords are process-local."
                     {:keywords-without-namespace simple-keywords})))

   (try
     (halt!)
     (catch Exception _))

   ;; Run model.
   (let [use-buffer (if (contains? opts :use-buffer)
                      use-buffer
                      generate)

         file (doto (File/createTempFile "eita" ".tla") .delete) ; we are interested in the random name of the folder
         abs-path (-> file .getAbsolutePath (str/split #"\.") first (str "/spec.tla"))
         opts-file-path (-> file .getAbsolutePath (str/split #"\.") first (str "/opts.edn"))
         _ (io/make-parents abs-path)
         [file-name file-type] (-> abs-path (str/split #"/") last (str/split #"\."))
         all-operators operators
         module-contents (module-template
                          {:init init-state
                           :next next-operator
                           :operators all-operators
                           :spec-name file-name})
         _ (spit abs-path module-contents)
         _ (when use-buffer
             (r.buf/sync!) ; sync so we can make sure that the writer can write
             (reset! r.buf/*contents [])
             (r.buf/reset-buf!)
             (r.buf/start-sync-loop!))
         ;; Also put a file with opts in the same folder so we can read configuration
         ;; in the tlc-handler function.
         _ (spit opts-file-path (merge opts
                                       (when use-buffer
                                         {::channel-file-path (str @r.buf/*channel-file)})))
         tlc-opts (->> (cond-> ["-noTE" "-nowarning"]
                         simulate (conj "-simulate")
                         generate (cond->
                                      true (conj "-generate")
                                      (:num generate) (conj (str "num=" (:num generate))))
                         depth (conj "-depth" depth)
                         seed (conj "-seed" seed)
                         fp (conj "-fp" fp)
                         no-deadlock (conj "-deadlock")
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
     (when debug?
       (println (->> (str/split-lines module-contents)
                     (map-indexed (fn [idx line]
                                    (str (inc idx) " " line)))
                     (str/join "\n"))))
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
                       :raw-args [(str "-DRECIFE_OPTS_FILE_PATH=" opts-file-path)]})
             output (atom [])
             t0 (System/nanoTime)
             *destroyed? (atom false)
             process (RecifeModel.
                      (reify
                        java.io.Closeable
                        (close [_]
                          (when-not @*destroyed?
                            (reset! *destroyed? true)
                            #_(p/destroy result)
                            (p/sh (format "kill -15 %s" (.pid (:proc result))))
                            (when use-buffer
                              (r.buf/stop-sync-loop!))
                            (println (format "\n\n------- Recife process destroyed after %s seconds ------\n\n"
                                             (Math/round (/ (- (System/nanoTime) t0) 1E9))))))

                        clojure.lang.IDeref
                        (deref [_]
                          ;; Wait until the process finishes.
                          @result
                          (reset! *destroyed? true)
                          (when use-buffer
                            (r.buf/stop-sync-loop!))
                          ;; Throw exception or return EDN result.
                          (if (.exists (io/as-file exception-filename))
                            (throw (deserialize-obj exception-filename))
                            (try
                              (let [edn (-> @output last edn/read-string)]
                                (if (map? edn)
                                  edn
                                  (println (-> @output last))))
                              (catch Exception _
                                (println (-> @output last))))))))
             _output-streaming (future
                                 (with-open [rdr (io/reader (:out result))]
                                   (binding [*in* rdr]
                                     (loop []
                                       (when-let [line (read-line)]
                                         ;; If it's a EDN hashmap, do not print it.
                                         (when-let [last-line (last @output)]
                                           (println last-line))
                                         (swap! output conj line)
                                         (recur)))))
                                 @process)]
         (reset! *current-model-run process)
         ;; Read line by line so we can stream the output to the user.
         (if async
           process
           @process))))))

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
  "Check `opts` in `run-model*`."
  ([init-global-state components]
   (run-model init-global-state components {}))
  ([init-global-state components opts]
   (schema/explain-humanized schema/RunModelComponents components "Invalid components")
   (let [components (set (flatten (seq components)))
         processes (filter #(= (type %) ::Proc) components)
         invariants (filter #(= (type %) ::Invariant) components)
         constraints (filter #(= (type %) ::Constraint) components)
         action-constraints (filter #(= (type %) ::ActionConstraint) components)
         properties (filter #(= (type %) ::Property) components)
         fairness (filter #(= (type %) ::Fairness) components)
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
                                (map :operator action-constraints)
                                (map :operator properties)
                                (map :operator fairness)))]
     (run-model* db-init next' operators opts))))

(defmacro defproc
  "Defines a process and its multiple instances (`:procs`).

  `name` is a symbol for this var.

  `params` is optional a map of:
     `:procs` - set of arbitrary idenfiers (keywords);
     `:local` - map with local variables, `:pc` is required.

  `:pc` - initial step from the `steps` key (keyword).

  `steps` is a map of keywords (or vector) to functions. Each of
  these functions receives one argument (`db`) which contains the
  global state (namespaced keywords) plus any local state (unamespaced
  keywords) merged. E.g. in the example below, the function of `::check-funds`
  will have `:amount` available in it's input (besides all global state, which
  it uses only `:account/alice`).

  The keys in `steps` also can be a vector with two elements, the first one
  is the step identifier (keyword) and the second is some non-deterministic
  source (Recife model checker checks all the possibilities), e.g. if we want
  to model a failure, we can have `[::read {:notify-failure? #{true false}}]`
  instead of only `::read`, `:notify-failure` will be assoc'ed to into `db`
  of the associated function.


;; Example
(r/defproc wire {:procs #{:x :y}
                 :local {:amount (r/one-of (range 5))
                         :pc ::check-funds}}
  {::check-funds
   (fn [{:keys [:amount :account/alice] :as db}]
     (if (< amount alice)
       (r/goto db ::withdraw)
       (r/done db)))

   ::withdraw
   (fn [db]
     (-> db
         (update :account/alice - (:amount db))
         (r/goto ::deposit)))

   ::deposit
   (fn [db]
     (-> db
         (update :account/bob + (:amount db))
         r/done))})"
  {:arglists '([name params? steps])}
  [name & [params' steps']]
  (let [params (if (some? steps')
                 params'
                 {})
        steps (if (some? steps')
                steps'
                params')]
    `(def ~name
       (let [keywordized-name# (keyword (str *ns*) ~(str name))
             temp-steps# ~steps
             ;; If you pass a function to `steps`, it means that you
             ;; have only one step and the name of this will be derived
             ;; from `name`.
             steps# (if (fn? temp-steps#)
                      {keywordized-name# temp-steps#}
                      temp-steps#)
             temp-params# ~params
             ;; If we don't have `:procs`, use the name of the symbol as a proc name.
             procs# (or (:procs temp-params#)
                        #{keywordized-name#})
             ;; If we don't have `:local`, just use the first step (it should have
             ;; one only).
             local-variables# (or (:local temp-params#)
                                  {:pc (if (vector? (key (first steps#)))
                                         (first (key (first steps#)))
                                         (key (first steps#)))})
             params# {:procs procs#
                      :local local-variables#}]
         (schema/explain-humanized schema/DefProc ['~name params# steps#] "Invalid `defproc` args")
         ^{:type ::Proc}
         {:name keywordized-name#
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
                                     (val %)))))}))))

(defmacro definvariant
  [name & opts]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           [doc-string# f#] ~(if (= (count opts) 1)
                               [nil (first opts)]
                               [(first opts) (last opts)])]
       ^{:type ::Invariant}
       {:name name#
        :invariant f#
        :operator (invariant name# doc-string# f#)})))

(defmacro defproperty
  [name expr]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           expr# ~expr]
       ^{:type ::Property}
       {:name name#
        :property expr#
        :operator (temporal-property name# expr#)})))

(defmacro deffairness
  [name expr]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           expr# ~expr]
       ^{:type ::Fairness}
       {:name name#
        :property expr#
        :operator (fairness name# expr#)})))

(defmacro defconstraint
  [name f]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           f# ~f]
       ^{:type ::Constraint}
       {:name name#
        :constraint f#
        :operator (state-constraint name# f#)})))

(defmacro defaction-constraint
  [name f]
  `(def ~name
     (let [name# (keyword (str *ns*) ~(str name))
           f# ~f]
       ^{:type ::ActionConstraint}
       {:name name#
        :constraint f#
        :operator (action-constraint name# f#)})))

(def save!
  "Save data that can be fetched in real time, it can be used for statistics."
  r.buf/save!)

(def read-saved-data
  "Read saved data, see `save!`."
  r.buf/read-contents)

(defn read-saved-data-if-new-content
  "Reads saved data if there is new content, otherwise, returns `nil`.
  See `save!`."
  []
  (when (r.buf/has-new-contents?)
    (r.buf/read-contents)))

(comment

  ;; TODO (from 2022-12-17):
  ;; - [x] Use Clerk for for visualization
  ;; - [ ] Add action property support
  ;; - [ ] Ability to  query for lineage
  ;;   - [ ] E.g. how could I have an entity with such and such characteristics?
  ;;   - This would help us to ask questions about the system
  ;;   - [-] Use datalog?
  ;;     - I guess not
  ;; - [ ] How to improve observability over what's being tested?
  ;;   - [ ] Gather real-time info about the running spec
  ;;   - [ ] Ability to check traces while running
  ;;     - [ ] Show it with clerk
  ;; - [ ] Create a def that can abstract a state machine
  ;; - [ ] Add `defchecker` (complement of `definvariant`)
  ;; - [ ] Add a way to use locks without having the user having to reinvent the
  ;;       wheel every time
  ;; - [ ] Add `not*` to rh
  ;; - [ ] Add `next*` to rh
  ;; - [ ] Add a way to tell which property was violated
  ;;   - https://github.com/tlaplus/tlaplus/issues/641
  ;; - [ ] Check a trace using properties and invariants without running the
  ;;       model
  ;;   - [ ] With it, we can create visualizations
  ;; - [ ] Can we build a spec without using the TLA+ compiler?
  ;;   - For perf purposes so we can avoid as many conversions as possible
  ;;   - [ ] Maybe we can leverage TLCGet to retrieve the initial values?
  ;; - [ ] Create var which holds state info
  ;; - [ ] Add a way to override default Spec expression

  ())

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
  ;; - [x] Keep track of which process is acting so we know who is doing what.
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
  ;; - [ ] Create CHANGELOG file.
  ;; - [ ] Visualize states file.
  ;; - [ ] Convert `nil` values in Clojure to `:recife/null` in TLA+ and
  ;;       vice-versa.

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
  ;; - [ ] Show action which took it to a back to state violation.
  ;; - [ ] Use DFS and `arrudeia` to check implementations. Well, it appears
  ;;       that TLC DFS is not reliable, so we will have to use BFS, maybe
  ;;       generating all the possible traces upfront (with a `StateWriter`
  ;;       instance) and using it. Or just use BFS and require lots of memory
  ;;       for the user, it may be viable given the small scope hypothesis.
  ;; - [ ] Return `:error` in `:trace`, check for some kind of error code.
  ;; - [x] Remove bogus `-DTLCCustomHandler` JVM property.
  ;; - [ ] Make `r/run-model` ignore other `r/run-model` calls by checking a JVM
  ;;       property.
  ;; - [ ] Better documentation for fairness and `:exists` local variables.
  ;; - [ ] Create serialized files in a unique folder so it does not clash
  ;;       with concurrent runs of Recife.
  ;; - [ ] Add flag to return an trace example if no violation is found.
  ;; - [ ] Check that all initial global variables are namespaced.
  ;; - [ ] Use Pathom3, I don't want to be bothered how to get data (e.g.
  ;;       trace from the result map).
  ;; - [x] Fix a problem with the usage of empty maps.
  ;; - [ ] Return better error when some bogus thing happens in the user-provided
  ;;       functions.
  ;; - [ ] Add `tap` support.

  "
== Interaction with implementation

To test an implementation, we have to make our system deterministic by
instrumenting it (maybe with `arrudeia`), also we have to keep track of
which trace we are at (maybe by appending the traces prefixes?) (maybe it's
not needed). Besides being deterministic and as we will use BFS, we have to
differentiate between the concurrent traaces through our systems (maybe by
mocking).

The combination function + state should give you the same result everytime.

Thinking a little bit more, we could use use DFS with the one worker limitation so
we don't have concurrency issues (which is what we are trying to test in a real
implementation anyway). DFS also does not let us test liveness stuff, but we could
probably use something from TLC (or somewhere else) which would check the stories
for us ad hoc.

We can do it in real time while TLA+ is running or after we have our traces from
the state writer file. Doing it after appears to be much more easier, it can be
said that testing it in the implementation it's kind of a refinement of the
specification. Doing it in real time means that we can drive TLA+ from the
implementation, but is this useful?

Two types of implementation model checking:
- Using arrudeia and Recife in the application itself.
- Sending command through headers so the application know when to \"freeze\".

Just using state writer is how we can know the action, so let's try to use it.

For the implementation project, also see https://github.com/pfeodrippe/recife/projects/2.

"

  ())
