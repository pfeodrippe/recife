(ns hillel.ch5-cache-4
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def max-consumer-req 1)

(def global
  {:cache/resource-cap (r/one-of :resource (map inc (range 6)))
   :cache/resources-left (r/one-of :resource (map inc (range 6)))
   :cache/reserved 0})

(r/defproc actor {:procs #{:a1 :a2}
                  :local {:resources-needed max-consumer-req
                          :pc ::wait-for-resources}}
  {::wait-for-resources
   (fn [{:keys [:cache/resources-left :cache/reserved :resources-needed] :as db}]
     (when (>= resources-left (+ reserved resources-needed))
       (-> db
           (update :cache/reserved + resources-needed)
           (r/goto ::use-resources))))

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
   (fn [{:keys [:cache/resource-cap] :as db}]
     (assoc db :cache/resources-left resource-cap))})

(rh/definvariant invariant
  [{:keys [:cache/resources-left]}]
  (>= resources-left 0))

(comment

  ;; ok.
  (def states
    (-> @(r/run-model global #{actor time' invariant} {:dump-states true})
        r/states-from-result))

  (r/random-traces-from-states states 3)

  ())
