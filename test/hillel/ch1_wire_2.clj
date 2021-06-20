(ns hillel.ch1-wire-2
  (:require
   [recife.core :as r]))

(def global
  {:account/alice 5
   :account/bob 5})

(r/defproc wire {:procs #{:x :y}
                 :local {:amount (r/one-of (range 5))
                         :pc ::check-and-withdraw}}
  {::check-and-withdraw
   (fn [{:keys [:amount :account/alice] :as db}]
     (if (< amount alice)
       (-> db
           (update :account/alice - (:amount db))
           (r/goto ::deposit))
       (r/done db)))

   ::deposit
   (fn [db]
     (-> db
         (update :account/bob + (:amount db))
         r/done))})

(r/definvariant invariant
  (fn [db]
    (and (>= (:account/alice db) 0)
         (>= (:account/bob db) 0))))

(r/defproperty eventually-consistent
  [:eventually
   [:always
    (fn [db]
      (= (+ (:account/alice db)
            (:account/bob db))
         10))]])

(comment

  (-> (r/run-model global #{wire invariant eventually-consistent} #_{:raw-output? true})
      r/timeline-diff)

  ())
