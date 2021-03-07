(ns hillel.ch2-recycler-1
  (:require
   [recife.core :as r]))

(def global
  {:trash/capacity 10
   :trash/bin #{}
   :recycle/capacity 10
   :recycle/bin #{}
   ::items [{:type :recycle :size 5}
            {:type :trash :size 5}
            {:type :recycle :size 4}
            {:type :recycle :size 3}]})

(r/defproc main {:procs #{:x}
                 :local {:pc ::main}}
  {::main
   (fn [{:keys [::items] :as db}]
     (if-let [{:keys [:type :size] :as item} (first items)]
       (cond
         (and (= type :recycle)
              (< size (:recycle/capacity db)))
         (-> (update db ::items rest)
             (update :recycle/capacity - size)
             (update :recycle/bin conj item))

         (< size (:trash/capacity db))
         (-> (update db ::items rest)
             (update :trash/capacity - size)
             (update :trash/bin conj item)))
       (r/done db)))})

(r/definvariant invariant
  (fn [db]
    (and (>= (:trash/capacity db) 0)
         (>= (:recycle/capacity db) 0))))
