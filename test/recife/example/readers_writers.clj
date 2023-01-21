(ns recife.example.readers-writers
  "Based on https://github.com/informalsystems/quint/tree/main/examples/ReadersWriters,
  check both TLA+ and Quint examples."
  (:require
   [clojure.set :as set]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  ;; Namespaced keywords (::readers is equivalent to :recife.example.readers-writers/readers)
  ;; are always global.
  ;; Unamespaces keywords (which shouldn't exist for `global`) are always unamespaced,
  ;; e.g. `:actor` in the first defproc.
  {::readers #{}
   ::writers #{}
   ::waiting []})

(def actors (range 3))

(defn- waiting-to-read
  [{:keys [::waiting]}]
  (->> waiting
       (filter (comp :read :action))
       (mapv :actor)
       set))

(defn- waiting-to-write
  [{:keys [::waiting]}]
  (->> waiting
       (filter (comp :write :action))
       (mapv :actor)
       set))

(defn- read*
  [actor db]
  (-> db
      (update ::readers conj actor)
      (update ::waiting (comp vec rest))))

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

(comment

  (r/run-model global #{try-read try-write read-or-write stop})
  (r/halt!)

  ())

;; TODO:
;; - [ ] Can we uses Clerk to show information about each defproc (by
;;       instrumenting more stuff)?
;; - [ ] Use async by default
