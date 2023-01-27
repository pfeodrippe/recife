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
  #{:c1 :c2 :c3})

(def resources
  #{:r1 :r2})

(def global
  {::unsat (->> clients (mapv #(vector % #{})) (into {}))
   ::alloc (->> clients (mapv #(vector % #{})) (into {}))
   ::sched []})

(def non-deterministic-params
  {:c clients
   :S (->> (seq resources) comb/subsets (mapv set) set)})

(r/defproc request
  {[:request
    non-deterministic-params]
   (fn [{:keys [:c :S ::unsat ::alloc] :as db}]
     (when (and (seq S)
                (empty? (get unsat c))
                (empty? (get alloc c)))
       (assoc-in db [::unsat c] S)))})

(r/defproc allocate
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

(rh/deffairness allocate-fairness
  [db]
  (rh/for-all [c clients]
    (rh/fair
     (rh/for-some [S (:S non-deterministic-params)
                   i-sched #(set (range (count (::sched %))))]
       (rh/call allocate
         (assoc db ::r/extra-args {:c c :S S :i-sched i-sched}))))))

(r/defproc return
  {[:return
    non-deterministic-params]
   (fn [{:keys [:c :S ::alloc] :as db}]
     (when (and (seq S)
                (set/subset? S (get alloc c)))
       (update-in db [::alloc c] set/difference S)))})

(rh/deffairness return-fairness
  [{:keys [::unsat ::alloc] :as db}]
  (rh/for-all [c clients]
    (rh/fair
     (rh/and*
      (empty? (get unsat c))
      (rh/call return
        (assoc db ::r/extra-args {:c c :S (get alloc c)}))))))

(defn- to-schedule
  [{:keys [::sched ::unsat]}]
  (set (filter #(and (seq (get unsat %))
                     (not (contains? (set sched) %)))
               clients)))

(r/defproc ^:fair schedule
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

(rh/defaction-property action-prop
  [{:keys [::unsat]}
   {unsat' ::unsat}]
  (not= unsat unsat'))

(comment

  ;; TODO:
  ;; - [x] Profile performance.
  ;; - [x] Create helpers for `for-all`, `exists` and `invoke`.

  ;; Back to state.
  ;; 5854 states generated, 1690 distinct states found, 0 states left on queue.
  (r/run-model global #{request allocate return schedule
                        resource-mutex allocator-invariant-1 allocator-invariant-2
                        allocator-invariant-3 allocator-invariant-4
                        clients-will-return clients-will-obtain inf-often-satisfied

                        ;; You can also use nested components that Recife will
                        ;; flatten it for you. This way, you can group things
                        ;; the way you prefer.
                        [allocate-fairness return-fairness]

                        ;; You can also check action properties, they receive 2 arguments,
                        ;; the current state (db) and the next state (db'). Uncomment
                        ;; below to see things failing when unsat is not different from
                        ;; unsat in the next state.
                        action-prop}
               {#_ #_:debug? true
                #_ #_:workers 1})

  (count (r/read-saved-data))
  (r/halt!)

  ;; 3m20 to 47s (~4.25x) after the performance improvements!!
  ;; 47s to 29s after adding keyword-cache
  ;; 14s after fairness was fixed

  ())
