(ns hillel.ch5-cache-1
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

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

(rh/definvariant invariant
  [{:keys [:cache/resources-left]}]
  (>= resources-left 0))

(comment

  ;; ok.
  (def states
    @(-> (r/run-model global #{actor time' invariant} {:dump-states true})
        r/states-from-result))
  ;; `:dump-states` creates a file with states and ranks (the level where a
  ;; state appears first). It adds `:recife/transit-states-file-path` to the
  ;; result of `run-model`, it's a implementation details that it's transit.

  (r/random-traces-from-states states)

  ())
