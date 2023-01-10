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
 :prefix "edn-")

#_(compile 'recife.class)
#_(cp/add-classpath "classes")
