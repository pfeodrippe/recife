(ns hillel.ch5-cache-2
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def resource-cap 6)

(def max-consumer-req 2)

(def global
  {:cache/resources-left resource-cap})

(r/defproc actor {:procs #{:a1 :a2}
                  :local {:resources-needed max-consumer-req
                          :pc ::wait-for-resources}}
  {::wait-for-resources
   (fn [{:keys [:cache/resources-left :resources-needed] :as db}]
     (when (>= resources-left resources-needed)
       (r/goto db ::use-resources)))

   [::use-resources
    {:x (range max-consumer-req)}]
   (fn [{:keys [:resources-needed :x] :as db}]
     (if (pos? resources-needed)
       (-> db
           (update :cache/resources-left dec)
           (update :resources-needed dec))
       (-> db
           (assoc :resources-needed x)
           (r/goto ::wait-for-resources))))})

(r/defproc time' {:procs #{:time}
                  :local {:pc ::tick}}
  {::tick
   (fn [db]
     (assoc db :cache/resources-left resource-cap))})

(rh/definvariant invariant
  [{:keys [:cache/resources-left]}]
  (>= resources-left 0))

(comment

  ;; invariant ~15 steps.
  (r/run-model global #{actor time' invariant} #_{:raw-output? true})

  ())
