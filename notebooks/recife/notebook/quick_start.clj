;; ◊page-name[{:subtitle "Gimme some code"}]{quick start}

;; Run the spec below and see a temporal property violation.

(ns recife.notebook.quick-start
  {:nextjournal.clerk/visibility {:result :hide}
   :clerk/name "quick start"}
  (:require
   [recife.clerk :as rc]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {::paid? (r/one-of #{true false})})

(r/defproc ^:fair activate {:local {:pc ::try-activation}}
  {::try-activation
   (fn [{::keys [paid?] :as db}]
     (if paid?
       (-> db
           (assoc ::status :activated)
           r/done)
       (-> db
           (assoc ::status :requires-payment)
           (r/goto ::pay))))

   [::pay
    {:fail? #{true false}}]
   (fn [{:keys [fail?] :as db}]
     (-> db
         (assoc ::paid? (not fail?))
         (r/goto ::try-activation)))})

(rh/defproperty will-eventualy-activate
  [{::keys [status]}]
  (rh/eventually
   (rh/always
    (= status :activated))))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::quick}
(rc/run-model global #{activate will-eventualy-activate})

;; Ok, this is too much, if you want to go deeper and slower, there is
;; a series of articles about Recife, starting with
;; ◊xref{:doc/slow-start}.
