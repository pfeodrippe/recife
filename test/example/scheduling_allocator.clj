(ns example.scheduling-allocator
  "It's a translation from one of the examples in the `tlaplus/Examples` repo in Github.
  It follows the MIT License.
  See https://github.com/tlaplus/Examples/blob/master/specifications/allocator/SchedulingAllocator.tla"
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.set :as set]
   [medley.core :as medley]
   [recife.core :as r]
   [recife.tla :as rt]))

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

(def allocate-fairness
  [:forall {'c clients}
   [:fair
    [:exists {'S (:S non-deterministic-params)
              'i_sched #(set (range (count (::sched %))))}
     [:call :allocate
      [:invoke {:c 'c :S 'S :i-sched 'i_sched}
       (fn [{:keys [:c :S :i-sched] :as args}]
         (assoc args ::r/extra-args {:c c :S S :i-sched i-sched}))]]]]])

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

(def ^:private return-fairness
  [:forall {'c clients}
   [:fair
    [:and
     [:invoke {:c 'c}
      (fn [{:keys [:c ::unsat]}]
        (empty? (get unsat c)))]
     [:call :return
      [:invoke {:c 'c}
       (fn [{:keys [:c ::alloc] :as args}]
         (assoc args ::r/extra-args {:c c :S (get alloc c)}))]]]]])

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
(r/definvariant resource-mutex
  (fn [{:keys [::alloc]}]
    (->> (for [c-1 clients
               c-2 clients
               :when (not= c-1 c-2)]
           (set/intersection (get alloc c-1) (get alloc c-2)))
         (every? empty?))))

(r/definvariant allocator-invariant-1
  "All clients in the schedule have outstanding requests."
  (fn [{:keys [::sched ::unsat]}]
    (every? (fn [client]
              (seq (get unsat client)))
            sched)))

(r/definvariant allocator-invariant-2
  "All clients that need to be scheduled have outstanding requests."
  (fn [{:keys [::unsat] :as db}]
    (every? (fn [client]
              (seq (get unsat client)))
            (to-schedule db))))

(r/definvariant allocator-invariant-3
  "Clients never hold a resource requested by a process earlier
in the schedule."
  (fn [{:keys [::sched ::unsat ::alloc]}]
    (every? (fn [i-sched]
              (->> (take i-sched sched)
                   (every? #(empty? (set/intersection (get alloc (get sched i-sched))
                                                      (get unsat %))))))
            (range (count sched)))))

(r/definvariant allocator-invariant-4
  "The allocator can satisfy the requests of any scheduled client
assuming that the clients scheduled earlier release their resources."
  (fn [{:keys [::unsat ::alloc ::sched]}]
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
           (every? true?)))))

;; ClientsWillReturn ==
;;   \A c \in Clients : unsat[c]={} ~> alloc[c]={}
(r/defproperty clients-will-return
  [:forall {'c clients}
   [:leads-to
    [:invoke {:c 'c}
     (fn [{:keys [:c ::unsat]}]
       (empty? (get unsat c)))]
    [:invoke {:c 'c}
     (fn [{:keys [:c ::alloc]}]
       (empty? (get alloc c)))]]])

(comment

  (clojure.walk/macroexpand-all
   '(rt/for-all [c clients
                 r resources]
      (rt/leads-to
       (contains? (get unsat c) r)
       (contains? (get alloc c) r))))

  (clojure.walk/macroexpand-all
   (quote
    (rt/defproperty sss
      [unsat alloc]
      (rt/for-all [c clients
                   r resources]
        (rt/leads-to
         (contains? (get unsat c) r)
         (contains? (get alloc c) r))))))

  [:forall {'c clients
            'r resources}
   [:leads-to
    [:invoke {:c 'c :r 'r}
     (fn [{:keys [:c ::unsat :r]}]
       (contains? (get unsat c) r))]
    [:invoke {:c 'c :r 'r}
     (fn [{:keys [:c ::alloc :r]}]
       (contains? (get alloc c) r))]]]

  ())

;; ClientsWillObtain ==
;;   \A c \in Clients, r \in Resources : r \in unsat[c] ~> r \in alloc[c]
(rt/defproperty clients-will-obtain
  [unsat alloc]
  (rt/for-all [c clients
               r resources]
    (rt/leads-to
     (contains? (get unsat c) r)
     (contains? (get alloc c) r))))

;; InfOftenSatisfied ==
;;   \A c \in Clients : []<>(unsat[c] = {})
(r/defproperty inf-often-satisfied
  [:forall {'c clients}
   [:always
    [:eventually
     [:invoke {:c 'c}
      (fn [{:keys [:c ::unsat]}]
        (empty? (get unsat c)))]]]])

(comment

  ;; TODO:
  ;; - [x] Profile performance.
  ;; - [ ] Create helpers for `forall`, `exists` and `invoke`.

  (r/run-model global #{request allocate return schedule
                        resource-mutex allocator-invariant-1 allocator-invariant-2
                        allocator-invariant-3 allocator-invariant-4
                        clients-will-return clients-will-obtain inf-often-satisfied}
               {#_ #_:debug? true
                #_ #_:workers 1})
  ())
