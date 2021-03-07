(ns hillel.ch2-telephone-2
  (:require
   [recife.core :as r]
   [recife.anim :as ra]))

(def global
  {:tel/to-send [1 2 3]
   :tel/received []
   :tel/in-transit #{}
   :tel/can-send? true})

(r/defproc sender {:procs #{:s}
                   :local {:pc ::sender}}
  {::sender
   (fn [{:keys [:tel/to-send :tel/can-send?] :as db}]
     (when can-send?
       (if-let [v (first to-send)]
         (-> db
             (update :tel/to-send rest)
             (update :tel/in-transit conj v)
             (assoc :tel/can-send? false))
         (r/done db))))})

(r/defproc receiver {:procs #{:r}
                     :local {:pc ::receiver}}
  {[::receiver
    {:msg :tel/in-transit}]
   (fn [{:keys [:msg] :as db}]
     (let [new-db (-> db
                      (update :tel/received conj msg)
                      (update :tel/in-transit disj msg)
                      (assoc :tel/can-send? true))]
       (cond-> new-db
         (= (count (:tel/received new-db)) 3)
         r/done)))})

(r/defproperty eventually-always-ordered
  [:eventually
   [:always
    (fn [{:keys [:tel/received] :as db}]
      (r/implies (r/all-done? db)
                 (= received [1 2 3])))]])

(comment

  ;; ok.
  (def result (r/run-model global #{sender receiver eventually-always-ordered}))

  (ra/visualize-result result)

  ())
