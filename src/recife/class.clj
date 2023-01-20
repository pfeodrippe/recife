(ns recife.class
  (:gen-class))

(gen-class
 :name recife.RecifeEdnValue
 :extends tlc2.value.impl.Value
 :constructors {[Object] []}
 :implements [tlc2.value.impl.Applicable tlc2.value.IValueRead tlc2.value.IStringValue]
 :init init
 :state state
 :impl-ns recife.class.recife-edn-value
 :methods [^:static [createMap
                     ["[Ltlc2.value.impl.Value;"
                      "[Ltlc2.value.impl.Value;"
                      tlc2.tool.coverage.CostModel]
                     tlc2.value.impl.Value]

           ^:static [createInt
                     [int]
                     tlc2.value.impl.Value]

           ^:static [createString
                     [String]
                     tlc2.value.impl.Value]

           ^:static [createTuple
                     ["[Ltlc2.value.impl.Value;"
                      tlc2.tool.coverage.CostModel]
                     tlc2.value.impl.Value]]
 :prefix "edn-")

#_(gen-class
   :name recife.RecifeEdnSetValue
   :constructors {[Object] []}
   :implements [tlc2.value.impl.Enumerable tlc2.value.impl.Reducible]
   :extends tlc2.value.impl.SetEnumValue
   :init init
   :state state
   :impl-ns recife.class.recife-edn-set-value
   :prefix "edn-")

#_(compile 'recife.class)
#_(cp/add-classpath "classes")
