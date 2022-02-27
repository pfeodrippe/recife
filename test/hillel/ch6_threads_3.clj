(ns hillel.ch6-threads-3
  (:require
   [recife.core :as r]
   [recife.anim :as ra]))

(def threads #{:t1 :t2 :t3})

(def global
  {:thread/next-thread (r/one-of threads)})

(r/defproc ^:fair thread {:procs threads
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
   (fn [{:keys [:thread/next-thread :self] :as db}]
     (if (not= next-thread self)
       (-> db
           (assoc :flag false)
           (r/goto ::p2-1-1))
       (r/goto db ::p2)))

   ::p2-1-1
   #(-> % (assoc :flag false) (r/goto ::p2-1-2))

   ::p2-1-2
   #(when (= (:thread/next-thread %) (:self %))
      (r/goto % ::p2-1-3))

   ::p2-1-3
   #(-> % (assoc :flag true) (r/goto ::p2))

   ::cs
   #(r/goto % ::p3)

   [::p3
    {:t threads}]
   (fn [{:keys [:t :self] :as db}]
     (when (not= t self)
       (-> db
           (assoc :thread/next-thread t)
           (r/goto ::p4))))

   ::p4
   (fn [db]
     (-> db
         (assoc :flag false)
         (r/goto ::p5)))

   ::p5
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
  [:for-all {'t threads}
   [:eventually
    [:invoke {:t 't}
     (fn [{:keys [:t ::r/procs]}]
       (= (get-in procs [t :pc]) ::cs))]]])

(comment

  ;; back-to-state at around 30-32 states (back to 2-4)
  (def result
    (r/run-model global #{thread at-most-one-critical no-livelocks}))

  (ra/visualize-result result)

  ())
