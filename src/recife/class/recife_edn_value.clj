(ns recife.class.recife-edn-value
  (:gen-class)
  (:require
   [clojure.edn :as edn]
   [kaocha.classpath :as cp]
   [tla-edn.core :as tla-edn])
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

(defn ->tla
  [coll]
  (cond
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
     false)))

(defn edn-getKindString
  [^RecifeEdnValue _]
  kind)

(defn edn-getDomain
  [^RecifeEdnValue this]
  (tla-edn/to-tla-value (set (keys (.-state this)))))

#_(.getDomain (RecifeEdnValue. {:a 1}))

(defn edn-apply-Value<>-int
  [^RecifeEdnValue this ^{:tag "[Ltlc2.value.impl.Value;"} args ^Integer control]
  (.apply this ^Value (aget args 0) ^Integer control))

#_(.apply (RecifeEdnValue. {:a 1})
          (into-array Value [(tla-edn/to-tla-value :a)])
          0)

(defn edn-apply-Value-int
  [^RecifeEdnValue this ^Value arg ^Integer _control]
  (tla-edn/to-tla-value (get (.-state this) (tla-edn/to-edn arg))))

(defn edn-apply
  [^RecifeEdnValue this ^Value arg ^Integer _control]
  (tla-edn/to-tla-value (get (.-state this) (tla-edn/to-edn arg))))

#_(.apply (RecifeEdnValue. {:a 1}) (tla-edn/to-tla-value :a) 0)

(defn edn-select
  [^RecifeEdnValue this ^Value arg]
  (tla-edn/to-tla-value (get (.-state this) (tla-edn/to-edn arg))))

#_(.select (RecifeEdnValue. {:a 1}) (tla-edn/to-tla-value :a))

(defn edn-init
  [v]
  [[] v])

(defn edn-toString
  [^RecifeEdnValue this ^StringBuffer sb ^Integer _offset ^Boolean _swallow]
  (.append sb (str (->tla (.-state this))) #_(str kind " " (pr-str (.-state this)))))

(defn edn-compareTo
  [^RecifeEdnValue this obj]
  (compare obj (->tla (.-state this))))

(defn edn-equals
  [^RecifeEdnValue this that]
  (if (instance? RecordValue that)
    (= (->tla (.-state this)) that)
    (= (.-state this) (.-state ^RecifeEdnValue that))))

#_(= (RecifeEdnValue. {:a 3})
     (RecifeEdnValue. {:a 3}))

#_(= (RecifeEdnValue. {:a 3})
     (->tla {:a 3}))

(defn edn-size
  [^RecifeEdnValue this]
  (count (.-state this)))

#_(= (.size (RecifeEdnValue. {:a 3 :b 54}))
     2)

(defn edn-toFcnRcd
  [^RecifeEdnValue this]
  (.toFcnRcd ^RecordValue (->tla (.-state this))))

#_(.toFcnRcd (RecifeEdnValue. {:a 3 :b 54}))

(defn edn-write
  [^RecifeEdnValue this ^IValueOutputStream vos]
  (let [idx (.put vos this)]
    (if (= idx -1)
      (let [os (.getOutputStream vos)
            s (pr-str (.-state this))]
        (.writeByte vos edn-value-byte)
        (.writeInt os (count s))
        (.writeString os s))
      (do (.writeByte vos tlc2.value.ValueConstants/DUMMYVALUE)
          (.writeNat vos idx)))))

(defn edn-createFrom-IValueInputStream
  [^RecifeEdnValue this ^IValueInputStream vis]
  #_(println :>>BEGK)
  (let [idx (.getIndex vis)
        dis (.getInputStream vis)
        #_ #_ _ (println :>>11 1)
        len (.readInt dis)
        #_ #__ (println :>>>readInt len)
        edn-str (.readString dis len)
        res (RecifeEdnValue. (edn/read-string edn-str))]
    (.assign vis res idx)
    res
    #_(println :>>>EDN edn-str)
    #_(println {:aa 10})
    #_(println :>>>idx idx)
    #_(println (count edn-str)))
  #_(println :>>>ss)
  #_(let [idx (.put vos this)]
      (if (= idx -1)
        (do (.writeByte vos (+ tlc2.value.ValueConstants/DUMMYVALUE 1))
            (.writeString (.getOutputStream vos) (pr-str (.-state this))))
        (do (.writeByte vos tlc2.value.ValueConstants/DUMMYVALUE)
            (.writeNat vos idx)))))

(defn edn-createFrom-ValueInputStream-Map
  [^RecifeEdnValue this ^IValueInputStream vis ^java.util.Map tbl]
  (println :>>BEGK_TBL)
  #_(try (let [dis (.getInputStream vis)
             _ (println :>>11 1)
             len (.readInt dis)
             _ (println :>>>readInt len)
             edn-str (.readString dis len)]
         (println :>>>EDN edn-str)
         (println {:aa 10})
         #_(println :>>>idx idx))
       (catch Exception e
         (println e)
         (throw e)))
  #_(println :>>>ss)
  #_(let [idx (.put vos this)]
      (if (= idx -1)
        (do (.writeByte vos (+ tlc2.value.ValueConstants/DUMMYVALUE 1))
            (.writeString (.getOutputStream vos) (pr-str (.-state this))))
        (do (.writeByte vos tlc2.value.ValueConstants/DUMMYVALUE)
            (.writeNat vos idx)))))

(defn edn-fingerPrint
  [^RecifeEdnValue this ^long fp]
  (let [fp (FP64/Extend fp ^String kind)
        fp (FP64/Extend fp ^long (hash (.-state this)))]
    fp))

(defn edn-permute
  [^RecifeEdnValue this _perm]
  this)

(defn edn-empty
  [_]
  (RecifeEdnValue. {}))

(.remove ValueInputStream/customValues (byte edn-value-byte))
(.put ValueInputStream/customValues (byte edn-value-byte) (RecifeEdnValue. {}))

#_(cp/add-classpath "classes")

#_(compile 'recife.class)

;; TODO:
;; - [x] Add support for other types at `ValueInputStream.java`
