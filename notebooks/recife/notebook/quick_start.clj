^{:nextjournal.clerk/visibility {:code :hide}}
(ns recife.notebook.quick-start
  {:clerk/name "quick start"}
  (:require
   [recife.clerk :as rc]
   [recife.core :as r]
   [recife.helpers :as rh]))

;; ◊page-name[{:subtitle "Gimme some code"}]{quick start}

(def global
  {::paid? (r/one-of #{true false})})

(r/defproc ^:fai activate {:local {:pc ::try-activation}}
  {::try-activation
   (fn [{::keys [paid?] :as db}]
     (if paid?
       (-> db
           (assoc ::status :activated)
           r/done)
       (-> db
           (assoc ::status :requires-payment)
           (r/goto ::pay))))

   #_[::pay
    {:fail? #{true false}}]
   ::pay
   (fn [{:keys [fail? oops] :as db}]
     (when oops
       (println (keyword (str "___" fail?))))
     (if #_fail? false
         (r/done db)
         (-> db
             (assoc ::status :activated)
             r/done)))})

(rh/defproperty will-eventualy-activate
  [{::keys [status]}]
  (rh/eventually
   (rh/always
    (= status :activated))))

(rh/deffairness pay-fairness
  [{:keys [::r/procs] :as db}]
  (rh/fair
   (rh/and*
    #_(do (println :>>>procs (:pc (::activate procs)))
        (not= (:pc (::activate procs)) ::r/done))
    (rh/for-some [fail? #{true false}]
      (rh/call activate
        (assoc db ::r/extra-args {:fail? fail?
                                  :oops true}))))))

(comment

  @(r/run-model global
                #{activate will-eventualy-activate #_pay-fairness}
                {:trace-example true
                 :debug true})

  ;; TODO:
  ;; - [x] Make pay-fairness work
  ;; - [x] Fix helper variables when using rh/deffairness
  ;; - [ ] Make meta fair take into consideration the non-deterministic params
  ;; - [ ] Always go to ::try-activation from ::pay

  ())
;; graviePayActivationStatus\ =\

;; requires-payment
;; activated

;; . -> activated if user has already paid
;; . -> requires-payment if user didn't pay yet
;; requires-payment -> activated after payment

;; User may initiate paid or not

;; Making a payment requires talking to an unreliable vendor
