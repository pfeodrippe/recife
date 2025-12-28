(ns recife.class.recife-edn-value
  (:gen-class)
  (:require
   [clojure.edn :as edn]
   [kaocha.classpath :as cp]
   [tla-edn-2.core :as tla-edn]
   [recife.util :refer [p*]]
   [clojure.string :as str]
   [clojure.repl :as repl]
   [taoensso.nippy :as nippy]
   recife.class)
  (:import
   (tlc2.value IValueOutputStream IValueInputStream ValueInputStream)
   (tlc2.value.impl Value StringValue RecordValue FcnRcdValue BoolValue IntValue
                    TupleValue)
   (util UniqueString)
   (tlc2.util FP64)))

(when-not (try
            (Class/forName "recife.RecifeEdnValue")
            (catch Error _))
  (compile 'recife.class)
  (cp/add-classpath "classes"))
(import '(recife RecifeEdnValue))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private edn-value-byte
  (+ tlc2.value.ValueConstants/DUMMYVALUE 1))

(defmacro ^:private debug
  [identifier & body]
  `(do
     (println ~identifier (do ~@body))
     (throw (ex-info "Boom from DEBUG" {}))))

#_(compile 'recife.class)

#_(do
    (import '(recife RecifeEdnValue))
    (bean (type (RecifeEdnValue. {}))))

#_(.-state (RecifeEdnValue. {:a 1}))

(def kind "RecifeEdnValue")

;; Pre-compile regex patterns for performance
(def ^:private dot-pattern (re-pattern "\\."))
(def ^:private triple-underscore-pattern (re-pattern "___"))

;; Cache for munge operations
(defonce ^:private munge-cache (atom {}))

(defn- custom-munge
  [v]
  (let [v-str (str v)]
    (or (get @munge-cache v-str)
        (let [result (str/replace (munge v) dot-pattern "___")]
          (swap! munge-cache assoc v-str result)
          result))))

(defn ->tla
  [v]
  (p* ::->tla
      (cond
        (nil? v)
        nil

        (keyword? v)
        (p* ::->tla--keyword
            (StringValue. ^String (custom-munge (symbol v))))

        (boolean? v)
        (p* ::->tla--boolean
            (BoolValue. v))

        (int? v)
        (p* ::->tla--int
            (IntValue/gen v))

        (vector? v)
        (p* ::->tla--vector
            (let [n (count v)
                  ^"[Ltlc2.value.impl.Value;" arr (make-array Value n)]
              (loop [i 0]
                (when (< i n)
                  (aset arr i ^Value (->tla (nth v i)))
                  (recur (unchecked-inc i))))
              (TupleValue. arr)))

        :else
        (let [coll v]
          (cond
            (empty? coll)
            (p* ::->tla--empty
                (tla-edn/-to-tla-value {:tla-edn.record/empty? true}))

            (every? keyword? (keys coll))
            (p* ::->tla--record
                (let [entries (seq coll)
                      n (count entries)
                      ^"[Lutil.UniqueString;" names (make-array UniqueString n)
                      ^"[Ltlc2.value.impl.Value;" values (make-array Value n)]
                  (loop [i 0, es entries]
                    (when es
                      (let [[k val] (first es)]
                        (aset names i (.getUniqueString ^RecifeEdnValue (tla-edn/to-tla-value k)))
                        (aset values i ^Value (tla-edn/to-tla-value val))
                        (recur (unchecked-inc i) (next es)))))
                  (RecordValue. names values false)))

            :else
            (p* ::->tla--fcn-rcd
                (let [entries (seq coll)
                      n (count entries)
                      ^"[Ltlc2.value.impl.Value;" keys-arr (make-array Value n)
                      ^"[Ltlc2.value.impl.Value;" values-arr (make-array Value n)]
                  (loop [i 0, es entries]
                    (when es
                      (let [[k val] (first es)]
                        (aset keys-arr i ^Value (tla-edn/to-tla-value k))
                        (aset values-arr i ^Value (tla-edn/to-tla-value val))
                        (recur (unchecked-inc i) (next es)))))
                  (FcnRcdValue. keys-arr values-arr false))))))))

(defn edn-getKindString
  [^RecifeEdnValue _]
  kind)

(defn edn-getUniqueString
  [^RecifeEdnValue this]
  (UniqueString/of (custom-munge (symbol (.-state this)))))

(def *cache (atom {}))

(defn- tlc-string->keyword
  [v]
  (or (get @*cache v)
      (let [s (str/replace v triple-underscore-pattern ".")
            res (keyword (repl/demunge s))]
        (swap! *cache assoc v res)
        res)))

(defn edn-createMap
  [^{:tag "[Ltlc2.value.impl.Value;"} names
   ^{:tag "[Ltlc2.value.impl.Value;"} values
   ^tlc2.tool.coverage.CostModel _cm]
  (p* ::createMap
      (let [n (alength names)]
        (RecifeEdnValue.
         (loop [i 0, acc (transient {})]
           (if (< i n)
             (recur (unchecked-inc i)
                    (assoc! acc
                            (tla-edn/to-edn ^RecifeEdnValue (aget names i))
                            (tla-edn/to-edn ^Value (aget values i))))
             (persistent! acc)))))))

(defn edn-createTuple
  [^{:tag "[Ltlc2.value.impl.Value;"} values
   ^tlc2.tool.coverage.CostModel _cm]
  (p* ::createTuple
      (let [n (alength values)]
        (RecifeEdnValue.
         (loop [i 0, acc (transient [])]
           (if (< i n)
             (recur (unchecked-inc i) (conj! acc (tla-edn/to-edn (aget values i))))
             (persistent! acc)))))))

(defn edn-createInt
  [i]
  (p* ::createInt
      (RecifeEdnValue. i)))

(defn edn-createString
  [^String str]
  (p* ::createString
      (RecifeEdnValue. (tlc-string->keyword str))))

(defn edn-getDomain
  [^RecifeEdnValue this]
  (p* ::getDomain
      (tla-edn/to-tla-value (set (keys (.-state this))))))

#_(.getDomain (RecifeEdnValue. {:a 1}))

(defn edn-apply-Value<>-int
  [^RecifeEdnValue this ^{:tag "[Ltlc2.value.impl.Value;"} args ^Integer control]
  (p* ::apply-V-list
      (.apply this ^Value (aget args 0) ^Integer control)))

#_(.apply (RecifeEdnValue. {:a 1})
          (into-array Value [(tla-edn/to-tla-value :a)])
          0)

(defn edn-apply-Value-int
  [^RecifeEdnValue this ^Value arg ^Integer _control]
  (p* ::apply-V
      (tla-edn/to-tla-value (get (.-state this)
                                 (p* ::apply-V--arg
                                     (tla-edn/to-edn arg))))))

#_(.apply (RecifeEdnValue. {:a 1}) (tla-edn/to-tla-value :a) 0)

(defn edn-select
  [^RecifeEdnValue this ^Value arg]
  (p* ::select
      (tla-edn/to-tla-value (get (.-state this) (tla-edn/to-edn arg)))))

#_(.select (RecifeEdnValue. {:a 1}) (tla-edn/to-tla-value :a))

(defn edn-init
  [v]
  (p* ::init
      [[] v]))

(defn edn-toString
  [^RecifeEdnValue this ^StringBuffer sb ^Integer _offset ^Boolean _swallow]
  (p* ::toString
      (.append sb
               (str (->tla (.-state this)))
               #_(str kind " " (pr-str (.-state this))))))

(defn edn-compareTo
  [^RecifeEdnValue this obj]
  (p* ::compareTo
      (if (instance? RecordValue obj)
        (compare (->tla (.-state this)) obj)
        (compare (hash (.-state this)) (hash (.-state ^RecifeEdnValue obj)))
        #_(compare (->tla (.-state this)) (->tla (.-state ^RecifeEdnValue obj))))))

(defn edn-equals
  [^RecifeEdnValue this obj]
  (p* ::equals
      (if (or (instance? RecordValue obj)
              (instance? StringValue obj))
        (= (->tla (.-state this)) obj)
        (= (.-state this) (.-state ^RecifeEdnValue obj)))))

#_(= (RecifeEdnValue. {:a 3})
     (RecifeEdnValue. {:a 3}))

#_(= (RecifeEdnValue. {:a 3})
     (->tla {:a 3}))

(defn edn-size
  [^RecifeEdnValue this]
  (p* ::size
      (count (.-state this))))

#_(= (.size (RecifeEdnValue. {:a 3 :b 54}))
     2)

(defn edn-toFcnRcd
  [^RecifeEdnValue this]
  (p* ::toFcnRcd
      (.toFcnRcd ^RecordValue (->tla (.-state this)))))

#_(.toFcnRcd (RecifeEdnValue. {:a 3 :b 54}))

(defn edn-toRcd
  [^RecifeEdnValue this]
  (p* ::toRcd
      (.toRcd ^Value (->tla (.-state this)))))

#_(.toRcd (RecifeEdnValue. {:a 3 :b 54}))

(defn edn-write
  [^RecifeEdnValue this ^IValueOutputStream vos]
  (p* ::write
      (let [idx ^int (.put vos this)]
        (if (= idx -1)
          (let [os ^util.BufferedDataOutputStream (.getOutputStream vos)
                ^{:tag "[B"} freezed (nippy/fast-freeze (.-state this))]
            (.writeByte vos edn-value-byte)
            (.writeInt os (alength freezed))
            (.write os freezed))
          (do (.writeByte vos tlc2.value.ValueConstants/DUMMYVALUE)
              (.writeNat vos idx))))))

(defn edn-createFrom-IValueInputStream
  [^RecifeEdnValue _this ^IValueInputStream vis]
  (p* ::createFrom-arity-2
      (let [idx (.getIndex vis)
            dis ^util.BufferedDataInputStream (.getInputStream vis)
            len (.readInt dis)
            data-bytes (byte-array len)
            _ (.read dis data-bytes 0 len)
            res (RecifeEdnValue. (nippy/fast-thaw data-bytes))]
        (.assign vis res idx)
        res)))

(defn edn-createFrom-ValueInputStream-Map
  [^RecifeEdnValue _this ^IValueInputStream _vis ^java.util.Map _tbl]
  (p* ::createFrom-arity-3
      (println :>>BEGK_TBL)))

(defn edn-fingerPrint
  [^RecifeEdnValue this ^long fp]
  (p* ::fingerPrint
      (let [fp (FP64/Extend fp ^String kind)
            fp (FP64/Extend fp ^long (hash (.-state this)))]
        fp)))

(defn edn-permute
  [^RecifeEdnValue this _perm]
  this)

(defn edn-empty
  [_]
  (p* ::empty
      (RecifeEdnValue. {})))

(.remove ValueInputStream/customValues (byte edn-value-byte))
(.put ValueInputStream/customValues (byte edn-value-byte) (RecifeEdnValue. {}))

#_(cp/add-classpath "classes")

#_(compile 'recife.class)

;; TODO:
;; - [x] Add support for other types at `ValueInputStream.java`
;; - [x] See if we can change TLC to accept our Edn class directly
;; - [x] Try to improve perf even more
;;   - [x] Check if we can use nippy for faster (de)serialization
;; - [ ] Try to add some defrecord to the main var
;; - [ ] See how things are going when using the library
;; - [ ] How to use `cm`?
;; - [ ] Can we make something for TLC?
;;   - [ ] Parsing is an issue for new IDEs
;;   - [ ] Visualization
;;   - [ ] Override for saving data for statistics
;;   - [ ] Deal with Jepsen (Maelstrom?)
;;   - [ ] Search TLA+ libraries
;;     - [ ] Including tladeps?
;;   - [ ] Call external commands in parallel (e.o. IOEnvExec)

(comment

  ())
