(ns recife.example.readers-writers
  "Based on https://github.com/informalsystems/quint/tree/main/examples/ReadersWriters,
  check both TLA+ and Quint examples."
  (:require
   [clojure.set :as set]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  ;; Qualified keywords (::readers is equivalent to :recife.example.readers-writers/readers)
  ;; are always global.
  ;; Unqualified keywords (which shouldn't exist for `global`) are always unamespaced,
  ;; e.g. `:actor` in the first defproc.
  {::readers #{}
   ::writers #{}
   ::waiting []})

(def actors (range 3))

(defn- waiting-to-read
  [{:keys [::waiting]}]
  (->> waiting
       (filter (comp #{:read} :action))
       (mapv :actor)
       set))

(defn- waiting-to-write
  [{:keys [::waiting]}]
  (->> waiting
       (filter (comp #{:write} :action))
       (mapv :actor)
       set))

(defn- read*
  [actor db]
  (-> db
      (update ::readers conj actor)
      (update ::waiting rest)))

(defn- write*
  [actor {:keys [::readers] :as db}]
  (when (empty? readers)
    (-> db
        (update ::writers conj actor)
        (update ::waiting rest))))

(r/defproc try-read
  {[:try-read
    {:actor actors}]
   (fn [{:keys [actor] :as db}]
     (when-not (contains? (waiting-to-read db) actor)
       (update db ::waiting conj {:action :read
                                  :actor actor})))})

(r/defproc try-write
  {[:try-write
    {:actor actors}]
   (fn [{:keys [actor] :as db}]
     (when-not (contains? (waiting-to-write db) actor)
       (update db ::waiting conj {:action :write
                                  :actor actor})))})

(r/defproc read-or-write
  (fn [{:keys [::waiting ::writers] :as db}]
    (when (and (seq waiting)
               (empty? writers))
      (let [{:keys [action actor]} (first waiting)]
        (if (= action :read)
          (read* actor db)
          (write* actor db))))))

(r/defproc stop
  {[:stop
    {:actor #(set/union (::readers %)
                        (::writers %))}]
   (fn [{:keys [actor ::readers] :as db}]
     (if (contains? readers actor)
       (update db ::readers disj actor)
       (update db ::writers disj actor)))})

(rh/definvariant safety
  [{:keys [::readers ::writers]}]
  (and (not (and (seq readers)
                 (seq writers)))
       (<= (count writers) 1)))

(rh/defproperty eventually-actor-is-reader
  [{:keys [::readers]}]
  (rh/for-all [actor actors]
    (rh/always
     (rh/eventually
      (contains? readers actor)))))

(rh/defproperty eventually-actor-is-writer
  [{:keys [::writers]}]
  (rh/for-all [actor actors]
    (rh/always
     (rh/eventually
      (contains? writers actor)))))

(rh/defproperty eventually-actor-is-not-a-reader
  [{:keys [::readers]}]
  (rh/for-all [actor actors]
    (rh/always
     (rh/eventually
      (not (contains? readers actor))))))

(rh/defproperty eventually-actor-is-not-a-writer
  [{:keys [::writers]}]
  (rh/for-all [actor actors]
    (rh/always
     (rh/eventually
      (not (contains? writers actor))))))

(rh/deffairness fairness
  [db]
  (rh/and*
   (rh/for-all [actor actors]
     (rh/fair
      (rh/call try-read
        (assoc db ::r/extra-args {:actor actor}))))

   (rh/for-all [actor actors]
     (rh/fair
      (rh/call try-write
        (assoc db ::r/extra-args {:actor actor}))))

   (rh/fair (rh/call read-or-write db))

   (rh/fair
    (rh/for-all [actor actors]
      (rh/call stop
        (assoc db ::r/extra-args {:actor actor}))))))

(comment

  (r/run-model global #{try-read try-write read-or-write stop
                        safety fairness
                        eventually-actor-is-reader eventually-actor-is-writer
                        eventually-actor-is-not-a-reader eventually-actor-is-not-a-writer}
               {#_ #_:trace-example true
                #_ #_:workers 1
                #_ #_:debug true})

  (def result (r/get-result))
  (r/halt!)

  (mapv waiting-to-read
        (rh/get-trace (r/halt!)))

  ;; TODO:
  ;; - [x] Use async by default
  ;; - [x] See why the process is running indefinitely
  ;; - [x] Fix fairness
  ;;   - [x] Make procs names unique if not defined
  ;;   - [x] Check if we need to munge and demunge
  ;;   - [x] Parse proc to `:call` instead of a keyword
  ;;   - [x] Fix fairness in scheduling allocation
  ;; - [ ] Could we remove `pc` for perf purposes if we see that it's not being
  ;;       used?
  ;;   - https://kirkpatricktech.org/2019/01/18/fairness-semantics-of-pluscal/
  ;; - [ ] For the same fairness/liveness, we call multiple functions using
  ;;       the samwe db, could we improve perf here?
  ;; - [ ] Can we uses Clerk to show information about each defproc (by
  ;;       instrumenting more stuff)?
  ;; - [ ] Add docstring support for each def
  ;; - [ ] Don't return a vector with indexes in the trace unless asked for

  ())
