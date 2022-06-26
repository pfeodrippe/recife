(ns hillel.ch5-server-1
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def max-queue-size 2)

(def global
  {:server/queue []})

(r/defproc writer {:procs #{:writer}
                   :local {:pc ::write}}
  {::write
   (fn [db]
     (update db :server/queue conj :msg))})

(r/defproc reader {:procs #{:reader}
                   :local {:pc ::read}}
  {::read
   (fn [{:keys [:server/queue] :as db}]
     ;; Check if `queue` is not empty, `nil` is not a valid value
     ;; for TLC.
     (when (seq queue)
       (-> db
           (update :server/queue rest)
           (assoc :current-message (first queue)))))})

(rh/definvariant bounded-queue
  [{:keys [:server/queue]}]
  (<= (count queue) max-queue-size))

(comment

  ;; invariant, 3 steps.
  (r/run-model global #{writer reader bounded-queue})

  ())
