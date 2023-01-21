(ns hillel.ch2-telephone-1
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {:tel/to-send [1 2 3]
   :tel/received []
   :tel/in-transit #{}})

(r/defproc sender {:procs #{:s}
                   :local {:pc ::sender}}
  {::sender
   (fn [{:keys [:tel/to-send] :as db}]
     (if-let [v (first to-send)]
       (-> db
           (update :tel/to-send rest)
           (update :tel/in-transit conj v))
       (r/done db)))})

(r/defproc receiver {:procs #{:r}
                     :local {:pc ::receiver}}
  {[::receiver
    {:msg :tel/in-transit}]
   (fn [{:keys [:msg] :as db}]
     (let [new-db (-> db
                      (update :tel/received conj msg)
                      (update :tel/in-transit disj msg))]
       (cond-> new-db
         ;; If we already have 3 messages, stop the count!
         ;; Otherwise a deadlock will happen as we will not have
         ;; more messages in `:tel/in-transit` in the next step.
         (= (count (:tel/received new-db)) 3)
         r/done)))})

(rh/defproperty eventually-always-ordered
  [{:keys [:tel/received] :as db}]
  (rh/eventually
   (rh/always
    (r/implies (r/all-done? db)
               (= received [1 2 3])))))

(comment

  ;; stuttering at state 8.
  (-> @(r/run-model global #{sender receiver eventually-always-ordered} {:fp 0 :seed 1})
      r/timeline-diff)

  ())
