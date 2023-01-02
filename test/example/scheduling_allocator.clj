(ns example.scheduling-allocator
  "It's a translation from one of the examples in the `tlaplus/Examples` repo in Github.
  It follows the MIT License.
  See https://github.com/tlaplus/Examples/blob/master/specifications/allocator/SchedulingAllocator.tla"
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.set :as set]
   [medley.core :as medley]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def clients
  #{:c1 :c2 #_:c3})

(def resources
  #{:r1 :r2})

(def global
  {::unsat (->> clients (mapv #(vector % #{})) (into {}))
   ::alloc (->> clients (mapv #(vector % #{})) (into {}))
   ::sched []})

(def non-deterministic-params
  {:c clients
   :S (->> (seq resources) comb/subsets (mapv set) set)})

(r/defproc request {}
  {[:request
    non-deterministic-params]
   (fn [{:keys [:c :S ::unsat ::alloc] :as db}]
     (when (and (seq S)
                (empty? (get unsat c))
                (empty? (get alloc c)))
       (assoc-in db [::unsat c] S)))})

(rh/deffairness allocate-fairness
  [db]
  (rh/for-all [c clients]
    (rh/fair
     (rh/for-some [S (:S non-deterministic-params)
                   i-sched #(set (range (count (::sched %))))]
       (rh/call :allocate
         (assoc db ::r/extra-args {:c c :S S :i-sched i-sched}))))))

(r/defproc ^{:fairness allocate-fairness} allocate {}
  {[:allocate
    (merge {:i-sched #(range (count (::sched %)))}
           non-deterministic-params)]
   (fn [{:keys [:i-sched :c :S ::sched ::unsat ::alloc] :as db}]
     (let [available-resources (set/difference resources (apply set/union (vals alloc)))]
       (when (and (seq S)
                  (set/subset? S (set/intersection available-resources (get unsat c)))
                  i-sched
                  (= (nth sched i-sched) c)
                  (->> (take i-sched sched)
                       (every? #(empty? (set/intersection (get unsat %) S)))))
         (-> db
             (update-in [::alloc c] set/union S)
             (update-in [::unsat c] set/difference S)
             (assoc ::sched (if (= S (get unsat c))
                              (vec (medley/remove-nth i-sched sched))
                              sched))))))})

(rh/deffairness return-fairness
  [{:keys [::unsat ::alloc] :as db}]
  (rh/for-all [c clients]
    (rh/fair
     (rh/and
      (empty? (get unsat c))
      (rh/call :return
        (assoc db ::r/extra-args {:c c :S (get alloc c)}))))))

(r/defproc ^{:fairness return-fairness} return {}
  {[:return
    non-deterministic-params]
   (fn [{:keys [:c :S ::alloc] :as db}]
     (when (and (seq S)
                (set/subset? S (get alloc c)))
       (update-in db [::alloc c] set/difference S)))})

(defn- to-schedule
  [{:keys [::sched ::unsat]}]
  (set (filter #(and (seq (get unsat %))
                     (not (contains? (set sched) %)))
               clients)))

(r/defproc ^:fair schedule {}
  {[:schedule
    {:sq #(comb/permutations (to-schedule %))}]
   (fn [{:keys [:sq] :as db}]
     (when (seq sq)
       (update db ::sched (comp vec concat) sq)))})

;; ResourceMutex ==
;;   \A c1,c2 \in Clients : c1 # c2 => alloc[c1] \cap alloc[c2] = {}
(rh/definvariant resource-mutex
  [{:keys [::alloc]}]
  (->> (for [c-1 clients
             c-2 clients
             :when (not= c-1 c-2)]
         (set/intersection (get alloc c-1) (get alloc c-2)))
       (every? empty?)))

(rh/definvariant allocator-invariant-1
  "All clients in the schedule have outstanding requests."
  [{:keys [::sched ::unsat]}]
  (every? (fn [client]
            (seq (get unsat client)))
          sched))

(rh/definvariant allocator-invariant-2
  "All clients that need to be scheduled have outstanding requests."
  [{:keys [::unsat] :as db}]
  (every? (fn [client]
            (seq (get unsat client)))
          (to-schedule db)))

(rh/definvariant allocator-invariant-3
  "Clients never hold a resource requested by a process earlier
in the schedule."
  [{:keys [::sched ::unsat ::alloc]}]
  (every? (fn [i-sched]
            (->> (take i-sched sched)
                 (every? #(empty? (set/intersection (get alloc (get sched i-sched))
                                                    (get unsat %))))))
          (range (count sched))))

(rh/definvariant allocator-invariant-4
  "The allocator can satisfy the requests of any scheduled client
assuming that the clients scheduled earlier release their resources."
  [{:keys [::unsat ::alloc ::sched]}]
  (let [unscheduled-clients (set/difference clients (set sched))]
    (->> sched
         (map-indexed (fn [idx client]
                        (let [available-resources (set/difference resources (apply set/union (vals alloc)))
                              previous-resources (set/union available-resources
                                                            (->> (take idx sched)
                                                                 (mapv #(get alloc %))
                                                                 (apply set/union))
                                                            (->> (take idx sched)
                                                                 (mapv #(get unsat %))
                                                                 (apply set/union))
                                                            (->> unscheduled-clients
                                                                 (mapv alloc)
                                                                 (apply set/union)))]
                          (set/subset? (get unsat client) previous-resources))))
         (every? true?))))

;; ClientsWillReturn ==
;;   \A c \in Clients : unsat[c]={} ~> alloc[c]={}
(rh/defproperty clients-will-return
  [{:keys [::unsat ::alloc]}]
  (rh/for-all [c clients]
    (rh/leads-to
     (empty? (get unsat c))
     (empty? (get alloc c)))))

;; ClientsWillObtain ==
;;   \A c \in Clients, r \in Resources : r \in unsat[c] ~> r \in alloc[c]
(rh/defproperty clients-will-obtain
  [{:keys [::unsat ::alloc]}]
  (rh/for-all [c clients
               r resources]
    (rh/leads-to
     (contains? (get unsat c) r)
     (contains? (get alloc c) r))))

;; InfOftenSatisfied ==
;;   \A c \in Clients : []<>(unsat[c] = {})
(rh/defproperty inf-often-satisfied
  [{:keys [::unsat]}]
  (rh/for-all [c clients]
    (rh/always
     (rh/eventually
      (empty? (get unsat c))))))

(comment

  ;; TODO:
  ;; - [x] Profile performance.
  ;; - [ ] Create helpers for `for-all`, `exists` and `invoke`.

  ;; 5854 states generated, 1690 distinct states found, 0 states left on queue.
  (r/run-model global #{request allocate return schedule
                        resource-mutex allocator-invariant-1 allocator-invariant-2
                        allocator-invariant-3 allocator-invariant-4
                        clients-will-return clients-will-obtain inf-often-satisfied}
               {#_ #_:debug? true
                #_ #_:workers 1})

  ;; 3m20 to 47s (~4.25x) after the performance improvements!!
  ;; 47s to 29s after adding keyword-cache

  "
pId                                              nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total

:recife.core/process-operator                 1,724,357    72.67μs   378.38μs   510.77μs   641.99μs     1.54ms     5.94s    684.70μs  ±87%    19.68m    756%
:recife.core/process-operator*                1,724,357    66.47μs   356.54μs   470.90μs   591.11μs     1.42ms     5.94s    636.79μs  ±87%    18.30m    703%
:recife.core/process-local-operator           4,136,825    13.07μs   107.86μs   128.63μs   147.99μs   311.81μs     4.72s    188.90μs  ±98%    13.02m    500%
:recife.core/tla-record-map--get             33,940,647     1.83μs     7.38μs    27.91μs    32.58μs    50.78μs     5.94s     21.32μs ±101%    12.06m    463%
:recife.core/result                           1,724,357    31.89μs   224.62μs   296.68μs   360.79μs   834.61μs     5.94s    390.10μs  ±87%    11.21m    431%
:tufte/compaction                                   289    87.63ms     1.39s      3.02s      3.89s      4.72s      5.94s      1.63s   ±52%     7.88m    302%
:recife.core/local-op-to-edn-main-var-tla     4,136,825   452.00ns    50.20μs    63.51μs    70.07μs   120.56μs     3.99s     88.09μs ±109%     6.07m    233%
:tla-edn.core/fcn                             3,096,279     8.87μs    50.66μs    62.38μs    69.86μs   133.82μs     3.99s    108.57μs ±107%     5.60m    215%
:recife.core/record-info-from-record--to-edn 11,743,237     1.76μs    14.16μs    22.33μs    24.68μs    41.13μs     4.72s     26.09μs  ±88%     5.11m    196%
:recife.core/local-op-to-tla                  4,136,825    93.00ns    49.40μs    57.72μs    63.58μs   120.07μs     2.80s     57.16μs  ±84%     3.94m    151%
:recife.core/tla-record-map--keys             9,125,511   322.00ns     7.85μs    17.36μs    18.98μs    27.84μs     3.11s     15.15μs ±105%     2.30m     88%
:recife.core/local-op-result                  4,136,825   585.00ns     2.26μs    44.66μs    48.67μs    69.26μs     4.72s     30.60μs ±116%     2.11m     81%
:recife.core/tla-record-map--assoc-1          2,306,086     2.94μs    28.18μs    35.45μs    38.98μs    71.46μs     3.99s     50.55μs  ±99%     1.94m     75%
:tla-edn.core/set                            13,989,860   521.00ns     3.38μs     7.64μs     8.61μs    11.98μs     3.89s      7.19μs  ±99%     1.68m     64%
:recife.core/tla-edn--unique-string          62,279,297    59.00ns   906.00ns     1.04μs     1.17μs     1.96μs   251.45ms     1.38μs  ±68%     1.43m     55%
:tla-edn.core/tuple                           6,276,914   475.00ns     6.71μs    10.72μs    11.98μs    17.78μs     3.21s     12.26μs  ±90%     1.28m     49%
:recife.core/tla-edn--string                 30,110,442   168.00ns   939.00ns     1.08μs     1.16μs     1.89μs   249.12ms     1.44μs  ±70%    43.40s     28%
:recife.core/to-tla-value                     1,724,357     1.10μs     3.68μs    32.80μs    35.59μs    44.08μs     4.87s     16.13μs ±128%    27.81s     18%
:recife.core/to-tla--tla-record-map          13,124,595   197.00ns   841.00ns     1.06μs     1.14μs     2.06μs   237.22ms     1.36μs  ±74%    17.87s     11%
:recife.core/to-edn-self-tla                  1,724,357     1.29μs     4.05μs     5.31μs     6.13μs     9.46μs   251.60ms     6.75μs  ±76%    11.65s      7%
:recife.core/val-tla-unique                   2,306,086   597.00ns     2.85μs     3.39μs     3.57μs     5.55μs   183.21ms     4.45μs  ±79%    10.27s      7%
:tla-edn.core/int                             5,325,630   258.00ns   814.00ns   934.00ns   975.00ns     1.71μs   240.03ms     1.35μs  ±80%     7.21s      5%
:recife.core/to-edn-main-var-tla              1,724,357   487.00ns     1.32μs     1.52μs     1.60μs     2.82μs   220.79ms     1.85μs  ±58%     3.19s      2%
:recife.core/to-edn-extra-args                1,724,357    37.00ns     1.22μs     1.41μs     1.48μs     2.71μs   191.69ms     1.84μs  ±68%     3.18s      2%
:recife.core/hash                             9,506,759     2.00ns   200.00ns   252.00ns   270.00ns   328.00ns    38.73ms   229.91ns  ±32%     2.19s      1%

Accounted                                                                                                                                    115.73m   4444%
Clock                                                                                                                                          2.60m    100%
"

  ())
