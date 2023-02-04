(ns recife.records
  (:import
   (clojure.lang IFn)))

(defrecord RecifeProc [name steps-keys procs operators]
  IFn
  (invoke [this db]
    (this db {}))
  (invoke [_ db extra-args]
    (when-not (= (count operators) 1)
      (throw (ex-info "You have more than one operator defined for this proc, this feature is only available for procs with one operator"
                      {:operators (mapv :recife.operator/name operators)})))

    (let [{:keys [expr]} (first operators)
          result (expr (merge db extra-args))]
      (->> (or result db)
           (filter (comp qualified-keyword? key))
           (into {})))))
