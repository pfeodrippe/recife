(ns recife.trace
  (:require
   [malli.core :as m]
   [recife.core :as r]))

(defn temporal-property->map
  [temporal-property]
  {:type :root
   :child (m/parse
           [:schema {:registry
                     {"tla"
                      [:or
                       [:catn
                        [:type [:= :for-all]]
                        [:bindings [:map-of :symbol [:schema [:ref "tla"]]]]
                        [:child [:schema [:ref "tla"]]]]
                       [:catn
                        [:type [:= :leads-to]]
                        [:source [:schema [:ref "tla"]]]
                        [:target [:schema [:ref "tla"]]]]
                       [:catn
                        [:type [:= :invoke]]
                        [:bindings [:map-of :keyword [:schema [:ref "tla"]]]]
                        [:function [:schema [:ref "tla"]]]]
                       [:catn
                        [:type [:= :not]]
                        [:child [:schema [:ref "tla"]]]]
                       fn?
                       coll?
                       symbol?]}}
            "tla"]
           (:property temporal-property))})

(defmulti parse-tla :type)

(defmethod parse-tla :default
  [value]
  (throw (ex-info "Unknown tla identifier"
                  {:identifier (:type value)
                   :error :__NOT_IMPLEMENTED
                   :value value})))

(defmethod parse-tla :invoke
  [{:keys [bindings function env]}]
  `{:value (~function (merge (:db ~'trace) ~bindings))
    :debug {:env ~env
            :type :invoke}})

(defmethod parse-tla :not
  [{:keys [child env]}]
  `(let [res# ~(parse-tla (assoc child :env env))]
     {:value (not (:value res#))
      :debug {:child res#
              :env ~env
              :type :not}}))

(defmethod parse-tla :for-all
  [{:keys [bindings child env]}]
  `(let [result# (doall
                  (for ~(->> bindings
                             (apply concat)
                             vec)
                    ~(parse-tla (assoc child :env (merge env (->> (keys bindings)
                                                                  (mapv (fn [sym]
                                                                          [(keyword sym) sym]))
                                                                  (into {})))))))]
     {:value (mapv :value result#)
      :debug {:children result#
              :env ~env
              :type :for-all}}))

(defn -leads-to
  [trace id env source-result target-result]
  (let [key* {:id id
              :env env
              :type :leads-to}
        last-state? (or (nil? (:next-idx trace))
                        (<= (:next-idx trace)
                            (:idx trace)))
        source-value (:value source-result)
        target-value (:value target-result)
        history (get @(:tableau trace) key*)
        was-enabled (:source-enabled (last history))
        source-enabled (if target-value
                         ;; Disable source when target is true.
                         false
                         (or source-value was-enabled))]
    (swap! (:tableau trace) update key*
           (comp vec conj)
           {:idx (:idx trace)
            :source-enabled (boolean source-enabled)
            :target-enabled target-value})
    ;; If we are not in last state, we don't know the result yet.
    {:value (boolean
             (or (not last-state?)
                 (not source-enabled)
                 ;; We need to iterate on the loopback (if any) to check
                 ;; if target becomes eventually true.
                 (when-let [next-idx (:next-idx trace)]
                   (some :target-enabled (drop next-idx history)))))
     :debug {:env env
             :source-result source-result
             :target-result target-result
             :type :leads-to}}))

(defmethod parse-tla :leads-to
  [{:keys [source target env]}]
  (let [id (keyword (gensym))]
    `(-leads-to ~'trace ~id ~env
                ~(parse-tla (assoc source :env env))
                ~(parse-tla (assoc target :env env)))))

#_(defmethod parse-tla :always
  [{:keys [source target env]}]
  (let [id (keyword (gensym))]
    `(let [trace# ~'trace
           k# {:id ~id
               :env ~env
               :type :leads-to}
           last-state?# (or (nil? (:next-idx trace#))
                            (<= (:next-idx trace#)
                                (:idx trace#)))
           source-result# ~(parse-tla (assoc source :env env))
           target-result# ~(parse-tla (assoc target :env env))
           source-value# (:value source-result#)
           target-value# (:value target-result#)
           history# (get @(:tableau trace#) k#)
           was-enabled# (:source-enabled (last history#))
           source-enabled# (if target-value#
                             ;; Disable source when target is true.
                             false
                             (or source-value# was-enabled#))]
       (swap! (:tableau trace#) update k#
              (comp vec conj)
              {:idx (:idx trace#)
               :source-enabled (boolean source-enabled#)
               :target-enabled target-value#})
       ;; If we are not in last state, we don't know the result yet.
       {:value (boolean
                 (or (not last-state?#)
                     (not source-enabled#)
                     ;; We need to iterate on the loopback (if any) to check
                     ;; if target becomes eventually true.
                     (when-let [next-idx# (:next-idx trace#)]
                       (some :target-enabled (drop next-idx# history#)))))
        :debug {:env ~env
                :source-result source-result#
                :target-result target-result#
                :type :leads-to}})))

(defn -process-for-all
  [result]
  (let [values (flatten (:value result))]
    {:value (every? true? values)
     :debug {:type :root
             :child result}}))

;; :root knows how to deal with the different kinds of children so
;; it can process a boolean result.
(defmethod parse-tla :root
  [{:keys [child]}]
  `(let [result# ~(parse-tla child)]
     (case ~(:type child)
       :for-all (-process-for-all result#))))

(defn check-temporal-property
  "Given a result (the dereffed return from `r/run-model`), it checks if a
  temporal property is violated by it.

  It should be useful for discovering which temporal property broke an
  assumption (as TLC does not give us this information by default).

  We reconstruct clojure code from the Recife TLA+ representation (hiccup-like)
  and try to emulate (as best as possible) the algorithm for checking a temporal
  property using pure Clojure.

  The return is a map with a `:violated?` key, indicating if this temp property
  is violated by the trace from `result`."
  [result temporal-property]
  (def temporal-property temporal-property)
  (let [compiled-property
        (eval `(fn [~'trace]
                 (let [~'trace (update ~'trace :tableau atom)]
                   [~(parse-tla (temporal-property->map temporal-property))
                    (deref (:tableau ~'trace))])))]
    (loop [[[idx db] & trace-rest] (:trace result)
           tableau {}
           trace-result nil]
      (let [trace {:idx idx
                   :next-idx (if (seq trace-rest)
                               (inc idx)
                               (-> result :trace-info :violation :state-number))
                   :db db
                   :tableau tableau}]

        (if idx
          (let [[trace-result tableau] (compiled-property trace)]
            (recur trace-rest
                   tableau
                   trace-result))
          (with-meta {:violated? (not (:value trace-result))}
            {:debug (:debug trace-result)
             :tableau (:tableau trace)}))))))

(defn check-temporal-properties
  "Given a result (the dereffed return from `r/run-model`) and the temporal properties
  (`components`, which is the same set that you pass to `r/run-model` in the second argument,
  but we just care here about the temporal properties), it checks which temporal property
  are violated.

  Check `check-temporal-property` for more information about the return values."
  [result components]
  (let [properties (set (filter #(= (type %) ::r/Property)
                                (flatten (seq components))))]
    (->> properties
         (mapv (fn [{:keys [name] :as property}]
                 [name (check-temporal-property result property)]))
         (into {}))))

(comment

  ;; TODO:
  ;; - [x] Leads to
  ;; - [x] For all
  ;; - [x] Not
  ;; - [ ] Always
  ;; - [ ] Eventually
  ;; - [ ] For some
  ;; - [ ] Call
  ;; - [ ] And*
  ;; - [ ] Or*

  ())
