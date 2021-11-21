(ns example.simple-allocator
  "See https://github.com/tlaplus/Examples/blob/master/specifications/allocator/SimpleAllocator.tla."
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.set :as set]
   [recife.core :as r]))

(def clients
  #{:c1 :c2 :c3})

(def resources
  #{:r1 :r2})

(def global
  {::unsat (->> clients (mapv #(vector % #{})) (into {}))
   ::alloc (->> resources (mapv #(vector % #{})) (into {}))})

(def non-deterministic-params
  "Used for non determinism, see each `defproc` and pay attention
  to the `:c` and `:S` keys."
  {:c clients
   :S (->> (seq resources) comb/subsets (mapv set))})

(r/defproc request {}
  {[:request
    non-deterministic-params]
   (fn [{:keys [:c :S ::unsat ::alloc] :as db}]
     (when (and (seq S)
                (empty? (get unsat c))
                (empty? (get alloc c)))
       (assoc-in db [::unsat c] S)))})

(r/defproc allocate {}
  {[:allocate
    non-deterministic-params]
   (fn [{:keys [:c :S ::unsat ::alloc] :as db}]
     (let [available-resources (set/difference resources (apply set/union (vals alloc)))]
       (when (and (seq S)
                  (set/subset? S
                               (set/intersection available-resources (get unsat c))))
         (-> db
             (update-in [::alloc c] set/union S)
             (update-in [::unsat c] set/difference S)))))})

(r/defproc return {}
  {[:return
    non-deterministic-params]
   (fn [{:keys [:c :S ::alloc] :as db}]
     (when (and (seq S)
                (set/subset? S (get alloc c)))
       (update-in db [::alloc c] set/difference S)))})

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
     (fn [{:keys [:c ::unsat]}])]
    [:invoke {:c 'c}
     (fn [{:keys [:c ::alloc]}])]]])

(comment
  ;; ----- Invariants and properties -----
  ;; TODO
  ;; ClientsWillObtain ==
  ;;   \A c \in Clients, r \in Resources : r \in unsat[c] ~> r \in alloc[c]

  ;; TODO
  ;; InfOftenSatisfied ==
  ;;   \A c \in Clients : []<>(unsat[c] = {})

  ;; TODO: Next?

  ;; TODO:
  ;; - [ ] Maybe watch for new vars and serialze then to the caller JVM?

  (r/run-model global #{request allocate return
                        resource-mutex clients-will-return}
               {:trace-example? true})

  ())
