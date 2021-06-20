(ns hillel.ch5-cache-1
  (:require
   [recife.core :as r]))

(def resource-cap 6)

(def global
  {:cache/resources-left resource-cap})

(r/defproc actor {:procs #{:actor}
                          :local {:resources-needed 2
                                  :pc ::use-resources}}
  {::use-resources
   (fn [{:keys [:cache/resources-left :resources-needed] :as db}]
     (when (>= resources-left resources-needed)
       (update db :cache/resources-left - resources-needed)))})

(r/defproc time' {:procs #{:time}
                 :local {:pc ::tick}}
  {::tick
   (fn [db]
     (assoc db :cache/resources-left resource-cap))})

(r/definvariant invariant
  (fn [{:keys [:cache/resources-left]}]
    (>= resources-left 0)))

(comment

  ;; ok.
  (r/run-model global #{actor time' invariant} #_{:raw-output? true})

  ())
