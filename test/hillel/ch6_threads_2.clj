(ns hillel.ch6-threads-2
  (:require
   [recife.core :as r]))

(r/defproc ^:fair thread {:procs #{:t1 :t2}
                          :local {:pc ::p1
                                  :flag false}}
  {::p1
   (fn [db]
     (-> db
         (assoc :flag true)
         (r/goto ::p2)))

   ::p2
   (fn [{:keys [::r/procs :self] :as db}]
     (if (->> procs
              (remove (comp #{self} key))
              vals
              (some :flag))
       (r/goto db ::p2-1)
       (r/goto db ::cs)))

   ::p2-1
   #(-> % (assoc :flag false) (r/goto ::p2-2))

   ::p2-2
   #(-> % (assoc :flag true) (r/goto ::p2))

   ::cs
   #(r/goto % ::p3)

   ::p3
   (fn [db]
     (-> db
         (assoc :flag false)
         (r/goto ::p4)))

   ::p4
   (fn [db]
     (r/goto db ::p1))})

(r/definvariant at-most-one-critical
  (fn [{:keys [::r/procs]}]
    (<= (->> procs
             vals
             (filter (comp #{::cs} :pc))
             count)
        1)))

(r/defproperty no-livelocks
  [:for-all {'t #{:t1 :t2}}
   [:eventually
    [:invoke {:t 't}
     (fn [{:keys [:t ::r/procs]}]
       (= (get-in procs [t :pc]) ::cs))]]])

(comment

  ;; back-to-state
  (r/run-model {} #{thread at-most-one-critical no-livelocks})

  ())
