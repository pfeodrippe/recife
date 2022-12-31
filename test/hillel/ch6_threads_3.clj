(ns hillel.ch6-threads-3
  (:require
   [recife.anim :as ra]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def threads #{:t1 :t2 :t3 #_:t4})

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
    (r/run-model global #{thread at-most-one-critical no-livelocks} #_{:run-local? true}
                 {:workers 1
                  :fp 0
                  :seed 1}))

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
  ;; - [ ] Assoc-1 (assoc with dissoc)
  ;;   - TBD
  ;; - [ ] Change tla+ code to create a Clojure map in RecordValue?
  ;; - [ ] Try to add helper functions that deal with TLC values directly
  ;; - [ ] Support TLC simulate option

  "
pId                                             nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total

:recife.core/process-operator                  144,662   154.68μs   207.08μs   379.56μs   421.33μs   533.22μs   201.72ms   266.45μs  ±38%    38.55s     83%
:recife.core/process-operator*                 144,662   133.38μs   168.46μs   315.73μs   350.66μs   445.32μs   201.64ms   219.21μs  ±39%    31.71s     68%
:recife.core/tla-record-map--get             1,173,982     4.14μs    10.02μs    17.68μs    22.67μs    36.35μs   201.41ms    12.61μs  ±47%    14.81s     32%
:recife.core/tla-record-map--assoc-1           278,053    28.13μs    39.62μs    77.68μs    91.44μs   127.88μs   129.02ms    51.91μs  ±44%    14.43s     31%
:recife.core/result                            144,662    45.17μs    54.82μs   100.35μs   119.88μs   173.56μs   201.49ms    75.54μs  ±45%    10.93s     23%
:recife.core/tla-record-map--assoc-1-dissoc    278,053    12.34μs    19.39μs    37.73μs    45.99μs    65.40μs   109.52ms    24.75μs  ±47%     6.88s     15%
:recife.core/tla-edn--unique-string          3,509,096   744.00ns     1.07μs     3.88μs     4.36μs     6.44μs    90.06ms     1.87μs  ±63%     6.57s     14%
:recife.core/tla-record-map--dissoc            278,053     9.22μs    16.19μs    31.47μs    37.35μs    55.98μs    87.47ms    19.20μs  ±44%     5.34s     11%
:recife.core/to-edn-main-var-tla               144,662    14.96μs    16.81μs    28.15μs    33.76μs    54.78μs   109.00ms    21.76μs  ±35%     3.15s      7%
:recife.core/to-tla-value                      144,662   940.00ns    13.28μs    25.55μs    28.62μs    47.13μs     3.08ms    15.39μs  ±32%     2.23s      5%
:recife.core/process-local-operator             24,957    56.24μs    63.86μs   120.98μs   135.32μs   178.72μs   128.30ms    82.72μs  ±35%     2.06s      4%
:recife.core/tla-edn--string                   559,383   835.00ns     1.22μs     6.51μs     7.53μs    14.01μs     4.20ms     3.04μs  ±82%     1.70s      4%
:tufte/compaction                                    9    87.34ms   131.53ms   176.15ms   201.40ms   201.40ms   201.40ms   135.70ms  ±25%     1.22s      3%
:recife.core/local-op-to-edn-main-var-tla       24,957    35.23μs    39.53μs    76.79μs    85.57μs   114.54μs     5.31ms    48.43μs  ±28%     1.21s      3%
:tla-edn.core/fcn                               24,957    34.33μs    38.33μs    74.70μs    83.04μs   111.05μs     5.30ms    47.03μs  ±28%     1.17s      3%
:recife.core/local-op-result                    24,957    19.78μs    22.73μs    41.97μs    48.81μs    74.89μs   128.26ms    32.74μs  ±49%   817.21ms     2%
:recife.core/to-edn-self-tla                   144,662     1.82μs     2.36μs     3.47μs     4.76μs     7.63μs     1.75ms     2.82μs  ±27%   408.28ms     1%
:recife.core/to-edn-extra-args                  22,131     3.51μs     3.94μs     8.83μs     9.93μs    20.51μs    53.49ms     8.32μs  ±89%   184.04ms     0%
:recife.core/tla-record-map--keys              368,683   313.00ns   394.00ns   504.00ns   618.00ns     2.05μs   458.17μs   474.85ns  ±28%   175.07ms     0%
:recife.core/to-tla--tla-record-map            278,053   279.00ns   372.00ns   488.00ns   572.00ns     2.03μs     2.00ms   443.80ns  ±33%   123.40ms     0%
:tla-edn.core/bool                             171,040   262.00ns   305.00ns   392.00ns   517.00ns     1.96μs   245.48μs   379.11ns  ±32%    64.84ms     0%
:recife.core/local-op-to-tla                    24,957   127.00ns   204.00ns   350.00ns   451.00ns   736.00ns   164.71μs   256.98ns  ±38%     6.41ms     0%

Accounted                                                                                                                                     2.40m    308%
Clock                                                                                                                                        46.62s    100%
"

  ;; TLA+ comparison.
  "
Time	 Diameter	Found	Distinct	Queue
00:00:00	0	4	4	4
00:00:03	10	11 100	3 732	1 136
00:00:17	38	94 984	23 412	0")

;; CLJ
"Finished in 01min 04s at (2022-12-27 21:56:27)"

()
