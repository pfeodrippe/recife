(ns tla-edn.core
  (:require
   [clojure.string :as str]
   [recife.util :refer [p*]])
  (:import
   (tlc2.value.impl Value IntValue RecordValue BoolValue FcnRcdValue
                    StringValue TupleValue SetEnumValue BoolValue)
   (util UniqueString)))

(def ^:private ^:dynamic *string-to-keyword?* false)

(defprotocol TLAPlusEdn
  (-to-edn [this]))

(defn to-edn
  ([this]
   (to-edn this {}))
  ([this {:keys [:string-to-keyword?]
          :or   {string-to-keyword? false}}]
   (binding [*string-to-keyword?* string-to-keyword?]
     (-to-edn this))))

(extend-protocol TLAPlusEdn
  tlc2.value.impl.RecordValue
  (-to-edn [v]
    (p* ::record
        (let [name->value (zipmap (mapv -to-edn (.-names v))
                                  (mapv -to-edn (.-values v)))]
          (if (= name->value {:tla-edn.record/empty? true})
            {}
            name->value))))

  tlc2.value.impl.FcnRcdValue
  (-to-edn [v]
    (p* ::fcn
        (zipmap (mapv #(-to-edn (.val %)) (.-domain v))
                (mapv -to-edn (.-values v)))))

  tlc2.value.impl.TupleValue
  (-to-edn [v]
    (p* ::tuple
        (mapv -to-edn (.getElems v))))

  tlc2.value.impl.SetEnumValue
  (-to-edn [v]
    (p* ::set
        (set (mapv -to-edn (.toArray (.-elems v))))))

  tlc2.value.impl.IntValue
  (-to-edn [v]
    (p* ::int
        (.val v)))

  tlc2.value.impl.StringValue
  (-to-edn [v]
    (p* ::string
        (let [s (str (.val v))]
          (if (str/includes? s "__")
            (let [[nmsp n] (str/split s #"__")]
              (keyword nmsp n))
            (if *string-to-keyword?*
              (keyword s)
              s)))))

  UniqueString
  (-to-edn [v]
    (p* ::unique-string
        (let [s (str v)]
          (if (str/includes? s "__")
            (let [[nmsp n] (str/split s #"__")]
              (keyword nmsp n))
            (if *string-to-keyword?*
              (keyword s)
              s)))))

  tlc2.value.impl.BoolValue
  (-to-edn [v]
    (p* ::bool
        (.getVal v))))

(defprotocol EdnToTla
  (-to-tla-value [this]))

(defn to-tla-value
  [v]
  (-to-tla-value v))

(defmacro typed-array
  [klass vals]
  (let [^Class resolved (resolve klass)]
    (with-meta
      (list 'into-array resolved vals)
      {:tag (str "[L" (.getName resolved) ";")})))

(extend-protocol EdnToTla
  clojure.lang.APersistentMap
  (-to-tla-value [coll]
    (if (empty? coll)
      (-to-tla-value {:tla-edn.record/empty? true})
      (RecordValue.
       (typed-array UniqueString (mapv #(-> % key ^String -to-tla-value .getVal) coll))
       (typed-array Value (mapv #(-> % val -to-tla-value) coll))
       false)))

  clojure.lang.PersistentVector
  (-to-tla-value [coll]
    (TupleValue.
     (typed-array Value (mapv #(-> % -to-tla-value) coll))))

  clojure.lang.PersistentList
  (-to-tla-value [coll]
    (TupleValue.
     (typed-array Value (mapv #(-> % -to-tla-value) coll))))

  clojure.lang.PersistentHashSet
  (-to-tla-value [coll]
    (SetEnumValue.
     (typed-array Value (mapv #(-> % -to-tla-value) coll))
     false))

  Integer
  (-to-tla-value [v]
    (IntValue/gen v))

  Long
  (-to-tla-value [v]
    (IntValue/gen v))

  BigDecimal
  (-to-tla-value [v]
    (IntValue/gen v))

  String
  (-to-tla-value [v]
    (StringValue. v))

  clojure.lang.Keyword
  (-to-tla-value [v]
    (StringValue. (if (namespace v)
                    (str (namespace v) "__" (name v))
                    (name v))))

  Boolean
  (-to-tla-value [v]
    (BoolValue. v)))
