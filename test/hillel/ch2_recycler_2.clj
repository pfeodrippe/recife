(ns hillel.ch2-recycler-2
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {:trash/capacity (r/one-of (range 10))
   :trash/bin []
   :recycle/capacity (r/one-of (range 10))
   :recycle/bin []
   ;; We use way less items than what's in the book because the parsing
   ;; from/to edn takes some time. We can try to optimize this later, but let's
   ;; focus on making this work first.
   ::items (repeatedly 2 #(identity {:type (r/one-of #{:recycle :trash})
                                     :size (r/one-of (range 6))}))})

(r/defproc main {:procs #{:x}
                 :local {:pc ::main}}
  {::main
   (fn [{:keys [::items] :as db}]
     (if-let [{:keys [:type :size] :as item} (first items)]
       (let [db (update db ::items rest)]
         (cond
           (and (= type :recycle)
                (< size (:recycle/capacity db)))
           (-> db
               (update :recycle/capacity - size)
               (update :recycle/bin conj item))

           (< size (:trash/capacity db))
           (-> db
               (update :trash/capacity - size)
               (update :trash/bin conj item))

           :else db))
       (r/done db)))})

(rh/definvariant invariant
  [db]
  (and (>= (:trash/capacity db) 0)
       (>= (:recycle/capacity db) 0)))

(comment

  (r/run-model global #{main invariant})

  ())
