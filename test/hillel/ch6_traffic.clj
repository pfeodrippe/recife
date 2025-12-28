(ns hillel.ch6-traffic
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {:traffic/at-light? true
   :traffic/light :red})

(r/defproc car {:procs #{:car}
                :local {:pc ::drive}}
  {::drive ^:fair+
   (fn [{:keys [:traffic/light] :as db}]
     (when (= light :green)
       (-> db
           (assoc :traffic/at-light? false)
           r/done)))})

(r/defproc light {:procs #{:light}
                  :local {:pc ::cycle}}
  {::cycle ^:fair
   (fn [{:keys [:traffic/at-light?] :as db}]
     (if at-light?
       (update db :traffic/light {:red :green
                                  :green :red})
       (r/done db)))})

(rh/defproperty termination
  [db]
  (rh/eventually (r/all-done? db)))

(comment

  ;; ok (6 generated states).
  (r/run-model global #{car light termination}
               {#_ #_:raw-output true #_ #_:debug true})

  ())
