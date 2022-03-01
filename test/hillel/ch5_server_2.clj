(ns hillel.ch5-server-2
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def max-queue-size 2)

(def global
  {:server/queue []
   ::r/procs {:writer {:pc ::write}
              :reader {:pc ::read}}})

(r/defproc writer {:procs #{:writer}
                   :local {:pc ::write}}
  {::write
   (fn [{:keys [:server/queue] :as db}]
     (when (< (count queue) max-queue-size)
       (update db :server/queue conj :msg)))})

(r/defproc reader {:procs #{:reader}
                   :local {:pc ::read}}
  {::read
   (fn [{:keys [:server/queue] :as db}]
     (when (seq queue)
       (-> db
           (update :server/queue rest)
           (assoc :current-message (first queue)))))})

(rh/definvariant bounded-queue
  [{:keys [:server/queue]}]
  (<= (count queue) max-queue-size))

(comment

  ;; ok.
  (r/run-model global #{writer reader bounded-queue})

  ())
