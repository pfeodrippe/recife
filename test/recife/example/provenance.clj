(ns recife.example.provenance
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

;; TODO:
;; - [ ] Employer
;;   - [ ] Model invoice to the portal website
;;   - [ ] Model partner id being set at worker for an employer
;; - [ ] Member
;;   - [ ] Member signs up for a payment method

(def global
  {::id 0})

(def ^:private choose-employer
  (comp keys ::employers))

(r/defproc add-employer
  (fn [{::keys [id] :as db}]
    (-> db
        (update ::id inc)
        (update ::employers assoc
                id {:id id}))))

(r/defproc add-parent-employer
  {[::set-parent-employer
    {:parent choose-employer
     :child choose-employer}]
   (fn [{::keys [employers] :keys [parent child] :as db}]
     (when (and (not= parent child)
                (not (:parent (get employers child)))
                (not (:parent (get employers parent)))
                (not (contains? (set (mapv :parent (vals employers)))
                                child)))
       (-> db
           (update-in [::employers child] assoc
                      :parent parent))))})

(r/defproc invoice
  {[::admin-uploads-invoice
    {:employer choose-employer}]
   (fn [{::keys [id] :keys [employer] :as db}]
     (when employer
       (-> db
           (assoc :choosen-employer employer)
           (update ::id inc)
           (update-in [::employers employer :invoices-uploaded]
                      conj id)
           (r/goto ::system-inserts-invoice))))

   ::system-inserts-invoice
   (fn [{::keys [employers] :keys [choosen-employer] :as db}]
     (-> db
         (update-in [::employers choosen-employer :invoices]
                    concat (get-in employers [choosen-employer :invoices-uploaded]))
         (assoc-in [::employers choosen-employer :invoices-uploaded] [])
         (r/goto ::admin-uploads-invoice)))})

(rh/defchecker check
  [{::keys [employers]}]
  (some (comp seq :invoices)
        (vals employers)))

(rh/definvariant no-grandparent
  [{::keys [employers]}]
  (not (some #(-> (:parent %) employers :parent)
             (vals employers))))

(rh/definvariant no-cyclical-parent
  [{::keys [employers]}]
  (not (some #(= (-> (:parent %) employers :parent)
                 (:id %))
             (vals employers))))

(rh/defconstraint constraint
  [{::keys [employers]}]
  (<= (count employers) 3))

(comment

  (r/run-model global #{add-employer add-parent-employer invoice
                        check
                        no-grandparent no-cyclical-parent
                        constraint}
               {:trace-example true})

  (r/get-result)
  (r/halt!)

  (->> (r/get-result)
       r/states-from-result
       r/random-traces-from-states
       #_(mapv #(mapv last %))
       #_(mapv last)
       #_(mapv ::employers))

  ;; You can use a proc as a function as if you were simulating a trace,
  ;; this is just a convenience so you can emulate some known scenario to harden
  ;; your code.
  ;; You can only call procs that have one operator (one step).
  (-> (add-employer global)
      add-employer
      add-employer
      ;; You can also pass extra args (which represent local variables or
      ;; non-deterministic ones),
      (add-parent-employer {:parent 1 :child 2}))

  ())
