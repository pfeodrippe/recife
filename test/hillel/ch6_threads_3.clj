(ns hillel.ch6-threads-3
  (:require
   [recife.anim :as ra]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def threads #{:t1 :t2 :t3 :t4})

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

(rh/definvariant at-most-one-critical
  [{:keys [::r/procs]}]
  (<= (->> procs
           vals
           (filter (comp #{::cs} :pc))
           count)
      1))

(rh/defproperty no-livelocks
  [{:keys [::r/procs]}]
  (rh/for-all [t threads]
    (rh/eventually
     (= (get-in procs [t :pc]) ::cs))))

(comment

  ;; back-to-state at around 30-32 states (back to 2-4)
  (def result
    ;; When you have `:simulate` or `:generate` set as a truthy value,
    ;; the return will be async. You can close or deref (@).
    @(r/run-model global #{thread at-most-one-critical no-livelocks}
                  {#_ #_:workers 1
                   :fp 0
                   :seed 1
                   :async true
                   #_ #_:simulate true
                   #_ #_:run-local? true}))

  (ra/visualize-result result)

  ;; TODO:
  ;; - [x] Cache unique string
  ;;   - 14% to 4%
  ;; - [x] Cache string
  ;;   - 4% to 1%
  ;; - [x] Check if we can use potemkin defmap to create an abstraction over
  ;;       RecordValue
  ;; - [x] Add most type hints
  ;;   - 64s -> 45s for :t4
  ;; - [x] :recife.core/tla-record-map--assoc-1
  ;;   - 14.99uS to 6.17us
  ;; - [x] ch6_threads_3
  ;;   - 64s -> 34s for :t4
  ;; - [x] keyword cache
  ;;   - 34s -> 24s
  ;; - [x] Add support for simulation
  ;;   - [x] Read trace
  ;;   - [x] Get number of states checked and the number of traces
  ;;   - [x] For multiple workers, get first trace
  ;; - [ ] Check statistics http://conf.tlapl.us/2022/JackMarkusTLA+Statistics.pdf
  ;; - [ ] Create macro that sends data from chilren processes  to the current
  ;;       JVM
  ;; - [ ] Add Clerk

  ;; TLA+ comparison.
  "
Time	 Diameter	Found	Distinct	Queue
00:00:00	0	4	4	4
00:00:03	10	11 100	3 732	1 136
00:00:17	38	94 984	23 412	0")

;; CLJ
"Finished in 01min 04s at (2022-12-27 21:56:27)"

()
