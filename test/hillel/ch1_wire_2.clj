(ns hillel.ch1-wire-2
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {:account/alice 5
   :account/bob 5})

(r/defproc wire {:procs #{:x :y}
                 :local {:amount (r/one-of (mapv inc (range 5)))
                         :pc ::check-and-withdraw}}
  {::check-and-withdraw
   (fn [{:keys [:amount :account/alice] :as db}]
     (if (<= amount alice)
       (-> db
           (update :account/alice - (:amount db))
           (r/goto ::deposit))
       (r/done db)))

   ::deposit
   (fn [db]
     (-> db
         (update :account/bob + (:amount db))
         r/done))})

(rh/definvariant invariant
  [{:keys [:account/alice :account/bob]}]
  (and (>= alice 0)
       (>= bob 0)))

(rh/defproperty eventually-consistent
  [{:keys [:account/alice :account/bob]}]
  (rh/eventually
   (rh/always
    (= (+ alice bob)
       10))))

(comment

  (-> @(r/run-model global #{wire invariant eventually-consistent} #_{:raw-output? true})
      r/timeline-diff)

  ())
