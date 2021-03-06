(ns recife.util.ordering
  (:refer-clojure :exclude [first last next])
  (:require
   [clojure.string :as str]
   recife.alloy)
  (:import
   (recife.alloy AlloySignature)))

(defn- build-fun
  [sig fun args]
  (let [sig-map (.-sig-map sig)]
    ;; TODO: Make a more generic function at recife.core for it.
    (AlloySignature. (assoc sig-map
                            :rec.alloy/result-cache (atom nil)
                            :rec.alloy/type :rec.type/relation
                            :rec.alloy/name
                            (format "%s_ordering/%s[%s]"
                                    (:rec.alloy/name sig-map)
                                    (name fun)
                                    (if (seq args)
                                      (->> args
                                           (mapv #(:rec.alloy/name (.-sig-map %)))
                                           (str/join ", " ))
                                      ""))))))

(defn ^:rec.alloy/module-required first
  [sig & args]
  (build-fun sig :first args))

(defn ^:rec.alloy/module-required last
  [sig & args]
  (build-fun sig :last args))

(defn ^:rec.alloy/module-required next
  [sig & args]
  (build-fun sig :next args))
