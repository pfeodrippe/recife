(ns recife.model
  (:require
   [recife.core :as r]
   [recife.protocol :as proto]))

(defn model-state
  "Get model state."
  ([]
   (model-state (r/get-model)))
  ([model]
   (when model
     (proto/-model-state model))))
