(ns hillel.ch5-server-3
  (:require
   [recife.anim :as ra]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def max-queue-size 2)

(def global
  {:server/queue []})

(r/defproc writer {:procs #{:writer}
                   :local {:pc ::write}}
  {::write
   (fn [{:keys [:server/queue] :as db}]
     (when (< (count queue) max-queue-size)
       (update db :server/queue conj :msg)))})

(r/defproc reader {:procs #{:reader}
                   :local {:pc ::read}}
  {[::read
    {:notify-failure? #{true false}}]
   (fn [{:keys [:server/queue :notify-failure?] :as db}]
     (when (seq queue)
       (cond-> (-> db
                   (update :server/queue rest)
                   (assoc :current-message (first queue)))
         notify-failure? (r/goto ::notify-failure))))

   ::notify-failure
   ;; Maybe do some guards with operator overriding?
   (fn [{:keys [:server/queue] :as db}]
     (when (< (count queue) max-queue-size)
       (-> db
           (dissoc :current-message)
           (update :server/queue conj :error)
           (r/goto ::read))))})

(rh/definvariant bounded-queue
  [{:keys [:server/queue]}]
  (<= (count queue) max-queue-size))

(comment

  ;; Should deadlock.
  (def result @ (r/run-model global #{writer reader bounded-queue}))

  (ra/visualize-result result)

  ())
