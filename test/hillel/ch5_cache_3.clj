(ns hillel.ch5-cache-3
  (:require
   [malli.core :as m]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def max-consumer-req 1)

(def global
  ;; We use `r/one-of` with a identifier so we can have a reference
  ;; to the same value in the global state.
  {:cache/resource-cap (r/one-of :resource (map inc (range 6)))
   :cache/resources-left (r/one-of :resource (map inc (range 6)))})

(r/defproc cache {:procs #{:a1 :a2}
                  :local {:resources-needed max-consumer-req
                          :ran? false
                          :pc ::wait-for-resources}}
  {::wait-for-resources
   (fn [{:keys [:cache/resources-left :resources-needed :ran?] :as db}]
     (when (and (not ran?)
                (>= resources-left resources-needed))
       (r/goto db ::use-resources)))

   [::use-resources
    {:x (range max-consumer-req)}]
   (fn [{:keys [:resources-needed :x] :as db}]
     (if (pos? resources-needed)
       (-> db
           (update :cache/resources-left dec)
           (update :resources-needed dec))
       (-> db
           (assoc :resources-needed x
                  :ran? true)
           (r/goto ::wait-for-resources))))})

(r/defproc time' {:procs #{:time}
                 :local {:pc ::tick}}
  {::tick
   (fn [{:keys [:cache/resource-cap] :as db}]
     (-> db
         (assoc :cache/resources-left resource-cap)
         (update ::r/procs (fn [procs]
                             (->> procs
                                  (mapv (fn [[k m]]
                                          [k (assoc m :ran? false)]))
                                  (into {}))))))})

(rh/definvariant invariant
  [{:keys [:cache/resources-left]}]
  ;; See that you also can pass a two-sized vector as the result
  ;; of `invariant` so you can see this in the output (it also works
  ;; with temporal properties).
  [(>= resources-left 0) {:well [:this :is :it]}])

(rh/definvariant type-ok
  [db]
  ;; As this is just normal Clojure code, you can use whatever
  ;; libarry you want, e.g. `Malli` to check for your own schema.
  (m/validate [:map
               [:cache/resource-cap :int]
               [:cache/resources-left :int]]
              db))

(comment

  ;; invariant
  @(r/run-model global #{cache time' invariant type-ok})

  ())
