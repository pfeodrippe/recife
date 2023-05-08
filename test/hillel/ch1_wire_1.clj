(ns hillel.ch1-wire-1
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {:account/alice 5
   :account/bob 5})

(r/defproc wire {:procs #{:x :y}
                 :local {:amount (r/one-of (mapv inc (range 5)))
                         :pc ::check-funds}}
  {::check-funds
   (fn [{:keys [:amount :account/alice] :as db}]
     (if (<= amount alice)
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
         r/done))})

(rh/definvariant invariant
  [db]
  (and (>= (:account/alice db) 0)
       (>= (:account/bob db) 0)))

(comment

  (-> @(r/run-model global #{wire invariant} {#_ #_:raw-output true
                                              #_ #_:debug true
                                              #_ #_:run-local true})
      r/timeline-diff)

  (r/read-saved-data :recife/violation)
  (r/get-result)

  ())
