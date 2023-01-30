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
    @(r/run-model global #{thread at-most-one-critical no-livelocks}
                  {:workers 1
                   :fp 0
                   :seed 1
                   #_ #_:simulate true
                   #_ #_:run-local? true}))

  (r/get-result)
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
  ;; - 19s achieved after removing TLA checks
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

"pId                                            nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total

:recife.class.recife-edn-value/apply-V      3,062,508     1.61μs     3.06μs     4.21μs     4.68μs    10.69μs   195.65ms     4.03μs  ±43%    12.34s     40%
:recife.class.recife-edn-value/getDomain    1,096,857     2.67μs     5.21μs     6.91μs     7.79μs    24.09μs   311.08ms     6.60μs  ±34%     7.24s     23%
:recife.core/to-tla--hash-set               1,096,883     1.35μs     3.76μs     5.11μs     5.48μs    19.17μs    30.15ms     4.30μs  ±21%     4.72s     15%
:recife.core/to-tla--map                    2,161,569   933.00ns     1.05μs     1.61μs     2.32μs     2.88μs   178.97ms     1.47μs  ±50%     3.17s     10%
:tufte/compaction                                  18    72.41ms   150.53ms   303.91ms   311.07ms   447.25ms   447.25ms   173.51ms  ±43%     3.12s     10%
:recife.core/process-operator                 144,846     7.63μs    10.99μs    18.01μs    24.52μs    44.15μs   303.95ms    18.38μs  ±65%     2.66s      9%
:recife.core/tla-edn--string                3,380,288   281.00ns   345.00ns   406.00ns   546.00ns     1.44μs   127.46ms   473.86ns  ±50%     1.60s      5%
:recife.core/process-operator*                144,846     2.49μs     4.90μs     7.36μs     8.81μs    25.87μs   119.91ms     6.65μs  ±45%   963.90ms     3%
:recife.core/process-local-operator            24,993     7.83μs     9.04μs    13.92μs    17.61μs    36.86μs   447.29ms    31.90μs ±133%   797.29ms     3%
:recife.core/local-op-to-edn-main-var-tla      24,993     6.74μs     7.70μs    11.81μs    13.66μs    32.43μs   447.28ms    30.17μs ±140%   754.12ms     2%
:recife.core/fcn                               24,993     5.95μs     6.71μs    10.11μs    11.51μs    30.04μs   447.28ms    28.99μs ±145%   724.49ms     2%
:recife.class.recife-edn-value/init         2,161,570   239.00ns   265.00ns   308.00ns   372.00ns     1.63μs   228.70μs   332.49ns  ±36%   718.71ms     2%
:recife.core/to-edn-self-tla                  144,846     1.04μs     1.17μs     1.58μs     2.57μs     3.25μs   303.91ms     3.50μs ±124%   506.89ms     2%
:recife.core/result                           144,846     1.45μs     1.84μs     3.28μs     3.93μs     9.83μs   165.61μs     2.38μs  ±36%   344.70ms     1%
:recife.class.recife-edn-value/toFcnRcd        24,975     8.41μs     9.60μs    14.35μs    17.36μs    35.91μs    49.64ms    13.38μs  ±45%   334.28ms     1%
:recife.class.recife-edn-value/->tla           25,951     1.92μs     8.70μs    13.15μs    18.50μs    35.66μs    49.64ms    12.48μs  ±48%   323.77ms     1%
:recife.core/to-tla-value                     144,846     1.45μs     1.74μs     2.93μs     3.18μs     4.83μs    96.47μs     2.06μs  ±26%   298.83ms     1%
:recife.class.recife-edn-value/equals         305,760   280.00ns   747.00ns     1.29μs     1.75μs     2.68μs   186.61μs   948.18ns  ±43%   289.92ms     1%
:recife.core/tla-edn--record                   23,215     4.65μs     5.30μs     8.91μs    20.78μs   107.81μs     1.52ms     8.90μs  ±65%   206.68ms     1%
:recife.core/result--merge                    144,846   922.00ns     1.10μs     1.60μs     2.39μs     3.54μs   129.52μs     1.34μs  ±30%   193.81ms     1%
:recife.core/tla-edn--zipmap                   23,215     3.87μs     4.43μs     7.59μs    17.97μs   104.73μs     1.51ms     7.79μs  ±70%   180.94ms     1%
:recife.core/to-edn-extra-args                 22,128     5.47μs     6.21μs     9.53μs    10.51μs    26.68μs   133.03μs     7.36μs  ±24%   162.84ms     1%
:recife.core/to-edn-main-var-tla              144,846   403.00ns   506.00ns   602.00ns   712.00ns     2.10μs   361.92μs   777.35ns  ±65%   112.60ms     0%
:recife.core/tla-edn--zipmap-values            23,215     1.29μs     1.46μs     2.84μs     4.29μs    90.71μs   550.21μs     3.66μs ±106%    84.97ms     0%
:recife.core/tla-edn--zipmap-keys              23,215     1.24μs     1.49μs     2.90μs     6.82μs    14.85μs   843.52μs     2.23μs  ±54%    51.69ms     0%
:recife.class.recife-edn-value/fingerPrint     24,341   376.00ns   505.00ns     1.53μs     2.07μs     4.29μs    71.36μs   845.11ns  ±60%    20.57ms     0%
:recife.core/tla-edn--unique-string            24,523   309.00ns   373.00ns   667.00ns     1.06μs     2.42μs    42.14μs   515.11ns  ±44%    12.63ms     0%
:recife.core/local-op-result                   24,993   221.00ns   339.00ns   610.00ns   942.00ns     2.13μs   229.77μs   500.04ns  ±50%    12.50ms     0%
:recife.class.recife-edn-value/toString           348     3.28μs    16.92μs    78.82μs   105.88μs   257.07μs   683.46μs    34.78μs  ±91%    12.10ms     0%
:recife.class.recife-edn-value/toRcd              189    26.44μs    32.25μs    42.90μs    52.06μs    84.96μs   110.36μs    35.47μs  ±18%     6.70ms     0%
:recife.core/local-op-to-tla                   24,993   109.00ns   155.00ns   308.00ns   443.00ns     1.11μs   173.21μs   233.57ns  ±53%     5.84ms     0%
:recife.core/to-tla--vector                        46     5.48μs     6.28μs     7.15μs     8.42μs   140.93μs   140.93μs     9.99μs  ±70%   459.33μs     0%
:recife.class.recife-edn-value/compareTo           79   741.00ns   814.00ns     1.59μs     2.09μs    18.22μs   290.77μs     5.13μs ±153%   405.36μs     0%

Accounted                                                                                                                                   41.97s    136%
Clock                                                                                                                                       30.89s    100%"

()
