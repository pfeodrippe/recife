(ns example.scheduling-allocator
  "It's a translation from one of the examples in the `tlaplus/Examples` repo in Github.
  It follows the MIT License.
  See https://github.com/tlaplus/Examples/blob/master/specifications/allocator/SchedulingAllocator.tla"
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.set :as set]
   [medley.core :as medley]
   [recife.core :as r]))

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

;; ClientsWillObtain ==
;;   \A c \in Clients, r \in Resources : r \in unsat[c] ~> r \in alloc[c]
(r/defproperty clients-will-obtain
  [:forall {'c clients
            'r resources}
   [:leads-to
    [:invoke {:c 'c :r 'r}
     (fn [{:keys [:c ::unsat :r]}]
       (contains? (get unsat c) r))]
    [:invoke {:c 'c :r 'r}
     (fn [{:keys [:c ::alloc :r]}]
       (contains? (get alloc c) r))]]])

;; TODO: SF Fairness.
;; \A c \in Clients: SF_vars(\E S \in SUBSET Resources: Allocate(c,S))

(comment
  ;; ----- Invariants and properties -----

  ;; TODO
  ;; InfOftenSatisfied ==
  ;;   \A c \in Clients : []<>(unsat[c] = {})

  ;; TODO:
  ;; - [ ] Profile performance.
  ;; - [ ] Create helpers for `forall`, `exists` and `invoke`.

  (r/run-model global #{request allocate return schedule
                        resource-mutex clients-will-return #_clients-will-obtain}
               {#_ #_:debug? true
                :workers 1})
  ())
