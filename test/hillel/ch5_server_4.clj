(ns hillel.ch5-server-4
  (:require
   [recife.core :as r]))

(def max-queue-size 2)

(def global
  {:server/queue []})

(r/defproc writer {:procs #{:writer}
                   :local {:pc ::write}}
  {::write
   (fn [{:keys [:server/queue] :as db}]
     (when (< (count queue) max-queue-size)
       (update db :server/queue conj :msg)))})

(r/defproc reader {:procs #{:r1 :r2}
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
   ;; `:self` is a special variable indicating which
   ;; process this call is part of. It's not stateful.
   (fn [{:keys [:server/queue :self] :as db}]
     (when (< (count queue) max-queue-size)
       (-> db
           (dissoc :current-message)
           (update :server/queue conj self)
           (r/goto ::read))))})

(r/definvariant bounded-queue
  (fn [{:keys [:server/queue]}]
    (<= (count queue) max-queue-size)))

(comment

  ;; Should deadlock (~ 6 states).
  (r/run-model global #{writer reader bounded-queue})

  ())
