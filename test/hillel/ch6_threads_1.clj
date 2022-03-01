(ns hillel.ch6-threads-1
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(r/defproc thread {:procs #{:t1 :t2}
                   :local {:pc ::p1
                           :flag false}}
  {::p1
   (fn [db]
     (-> db
         (assoc :flag true)
         (r/goto ::p2)))

   ::p2
   (fn [{:keys [::r/procs :self] :as db}]
     (when (->> procs
                (remove #(= (key %) self))
                vals
                (every? (comp false? :flag)))
       (r/goto db ::cs)))

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

(rh/definvariant at-most-one-critical
  [{:keys [::r/procs]}]
  (<= (->> procs
           vals
           (filter (comp #{::cs} :pc))
           count)
      1))

(comment

  ;; deadlock.
  (r/run-model {} #{thread at-most-one-critical})

  ())
