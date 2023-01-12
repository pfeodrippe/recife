(ns recife.class.recife-edn-value
  (:gen-class)
  (:require
   [clojure.edn :as edn]
   [kaocha.classpath :as cp]
   [tla-edn.core :as tla-edn]
   [recife.util :refer [p*]]
   [clojure.string :as str]
   [clojure.repl :as repl])
  (:import
   (tlc2.value IValueOutputStream IValueInputStream ValueInputStream)
   (tlc2.value.impl Value StringValue RecordValue FcnRcdValue)
   (util UniqueString)
   (tlc2.util FP64)))

(when-not (try
            (Class/forName "recife.RecifeEdnValue")
            (catch Error _))
  (compile 'recife.class)
  (cp/add-classpath "classes"))
(import '(recife RecifeEdnValue))

(set! *warn-on-reflection* true)

(def ^:private edn-value-byte
  (+ tlc2.value.ValueConstants/DUMMYVALUE 1))

#_(compile 'recife.class)

#_(do
    (import '(recife RecifeEdnValue))
    (bean (type (RecifeEdnValue. {}))))

#_(.-state (RecifeEdnValue. {:a 1}))

(def kind "RecifeEdnValue")

(defn- custom-munge
  [v]
  (str/replace (munge v) #"\." "___"))

(defn ->tla
  [coll]
  (p* ::->tla
      (if (keyword? coll)
        (StringValue. ^String (custom-munge (symbol coll)))
        (cond
          (empty? coll)
          (tla-edn/-to-tla-value {:tla-edn.record/empty? true})

          (every? keyword? (keys coll))
          (RecordValue.
           (tla-edn/typed-array UniqueString (mapv #(-> % key ^RecifeEdnValue tla-edn/to-tla-value .getUniqueString)
                                                   coll))
           (tla-edn/typed-array Value (mapv #(-> % val tla-edn/to-tla-value) coll))
           false)

          :else
          (FcnRcdValue.
           (tla-edn/typed-array Value (mapv #(-> % key tla-edn/to-tla-value) coll))
           (tla-edn/typed-array Value (mapv #(-> % val tla-edn/to-tla-value) coll))
           false)))))

(defn edn-getKindString
  [^RecifeEdnValue _]
  kind)

(defn edn-getUniqueString
  [^RecifeEdnValue this]
  (UniqueString/of (custom-munge (symbol (.-state this)))))

(defn- tlc-string->keyword
  [v]
  (let [s (str/replace v #"___" ".")]
    (keyword (repl/demunge s))))

(defn edn-createMap
  [^{:tag "[Ljava.lang.String;"} names
   ^{:tag "[Ltlc2.value.impl.Value;"} values
   ^tlc2.tool.coverage.CostModel _cm]
  (p* ::createMap
      (RecifeEdnValue. (zipmap (mapv tlc-string->keyword names)
                               (mapv tla-edn/to-edn values)))))

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
      (let [idx (.put vos this)]
        (if (= idx -1)
          (let [os (.getOutputStream vos)
                s (pr-str (.-state this))]
            (.writeByte vos edn-value-byte)
            (.writeInt os (count s))
            (.writeString os s))
          (do (.writeByte vos tlc2.value.ValueConstants/DUMMYVALUE)
              (.writeNat vos idx))))))

(defn edn-createFrom-IValueInputStream
  [^RecifeEdnValue _this ^IValueInputStream vis]
  (p* ::createFrom-simple
      (let [idx (.getIndex vis)
            dis (.getInputStream vis)
            len (.readInt dis)
            edn-str (.readString dis len)
            res (RecifeEdnValue. (edn/read-string edn-str))]
        (.assign vis res idx)
        res)))

(defn edn-createFrom-ValueInputStream-Map
  [^RecifeEdnValue _this ^IValueInputStream _vis ^java.util.Map _tbl]
  (p* ::createFrom-tbl
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

(comment

  ())
