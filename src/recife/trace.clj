(ns recife.trace
  (:require
   [clojure.edn :as edn]
   [malli.core :as m]
   [recife.core :as r]))

(defn temporal-property->map
  [temporal-property]
  (m/parse
   [:schema {:registry
             {"tla"
              [:or
               [:catn
                [:type [:= :for-all]]
                [:bindings [:map-of :symbol [:schema [:ref "tla"]]]]
                [:child [:schema [:ref "tla"]]]]
               [:catn
                [:type [:= :always]]
                [:child [:schema [:ref "tla"]]]]
               [:catn
                [:type [:= :eventually]]
                [:child [:schema [:ref "tla"]]]]
               [:catn
                [:type [:= :leads-to]]
                [:source [:schema [:ref "tla"]]]
                [:target [:schema [:ref "tla"]]]]
               ;; TODO: And can receive multiple arguments.
               #_[:catn
                  [:type [:= :and]]
                  [:source [:schema [:ref "tla"]]]
                  [:target [:schema [:ref "tla"]]]]
               [:catn
                [:type [:= :implies]]
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
   (:property temporal-property)))

(defonce ^:dynamic *trace-view* nil)

(defmacro with-info
  [info & body]
  `(binding [*trace-view* (merge *trace-view* ~info)]
     ~@body))

(defn debug
  [v]
  (when (:debug *trace-view*)
    (swap! (:debug *trace-view*) conj
           (merge {:debug v}
                  (select-keys *trace-view* [:env :loopback-idx :chain])
                  (dissoc (:current-state *trace-view*) ::r/procs :recife/metadata)))))

(defmulti parse-tla :type)

(defmethod parse-tla :default
  [value]
  (throw (ex-info "Unknown tla identifier"
                  {:identifier (:type value)
                   :error :__NOT_IMPLEMENTED
                   :value value})))

(defn -not
  [env child-fn]
  (with-info {:chain (concat (:chain *trace-view*) [:not])}
    (let [result (child-fn)]
      (debug {:result result
              :type :not
              :env env})
      (not result))))

(defmethod parse-tla :not
  [{:keys [child env]}]
  `(-not ~env (fn []
                ~(parse-tla (assoc child :env env)))))

(defmethod parse-tla :for-all
  [{:keys [bindings child env]}]
  `(with-info {:chain (concat (:chain *trace-view*) [:for-all])}
     (let [result# (doall
                    (for ~(->> bindings
                               (mapv (fn [[k# v#]]
                                       [k# (if (and (seq? v#)
                                                    (number? (first v#)))
                                             `(quote ~v#)
                                             v#)]))
                               (apply concat)
                               vec)
                      ~(parse-tla (assoc child :env (merge env (->> (keys bindings)
                                                                    (mapv (fn [sym]
                                                                            [(keyword sym) sym]))
                                                                    (into {})))))))]
       (debug {:result result#
               :type :for-all
               :env ~env})
       (every? (comp true? boolean) result#))))

(defmethod parse-tla :invoke
  [{:keys [bindings function env]}]
  `(with-info {:chain (concat (:chain *trace-view*) [:invoke])}
     (let [result# (~function (merge (:current-state *trace-view*) ~bindings))]
       (debug {:result result#
               :type :invoke
               :form (edn/read-string ~(:form (meta bindings)))
               :env ~env})
       result#)))

;; - We are trying to make [](F => []G) work first.
;; - How do we keep G activated once it's triggered?
;; - Don't check state by state, check the entire behavior.
;; - `always` probably needs to receive the trace + loopback that's concerning for
;; itself.
;;   - The parents cuts it
;;   - ~~If there is no loopback, then `always` is false~~
;;     - NOT true!! Let's assume that the trace is complete
;; - What we need now is get for eventually to work, but without
;;   knowing that it's eventually by using the definition

(defn -always
  [env child-fn]
  (let [{:keys [behavior loopback-idx]} *trace-view*
        invoke-child (fn [idx state behavior-rest]
                       (with-info {:env env
                                   :current-state state
                                   :loopback-idx (if (pos? (- idx loopback-idx))
                                                   (inc (- (count behavior) loopback-idx))
                                                   loopback-idx)
                                   :behavior (concat (concat [state] behavior-rest)
                                                     ;; If we are inside the loopback, append
                                                     ;; the loopback behavior until just before the actual
                                                     ;; state.
                                                     (->> (take (- idx loopback-idx) behavior)
                                                          (mapv #(assoc % ::idx (+ loopback-idx idx)))))}
                         (let [result (child-fn)]
                           #_(println :>>>RESULT result :chain (:chain *trace-view*))
                           result)))
        result (loop [[state & behavior-rest] behavior]
                 #_(do (println "\n")
                     (clojure.pprint/pprint
                      {:state state
                       :chain (:chain *trace-view*)}))
                 (let [idx (::idx state)]
                   (cond
                     (not state)
                     true

                     (invoke-child idx state behavior-rest)
                     (recur behavior-rest)

                     :else
                     false)))]
    (debug {:result result
            :env env
            :type :always})
    result))

(defmethod parse-tla :always
  [{:keys [child env]}]
  `(with-info {:chain (concat (:chain *trace-view*) [:always])}
     (-always ~env (fn []
                     ~(parse-tla (assoc child :env env))))))

#_`(fn [trace-view#]
     (binding [*trace-view* trace-view#]
       ~(parse-tla (temporal-property->map temporal-property))))

;; Using the definition that eventually is equal to `(not (always (not true))`
;; or not always false.
(defn -eventually
  [env child-fn]
  (with-info {:chain (concat (:chain *trace-view*) [:eventually])}
    (let [result (child-fn)]
      (debug {:result result
              :type :eventually
              :env env})
      result)))

(defmethod parse-tla :eventually
  [{:keys [child env]}]
  `(-eventually ~env (fn []
                       ~(parse-tla {:type :not
                                    :env env
                                    :child {:type :always
                                            :child {:type :not
                                                    :child (assoc child :env env)
                                                    :env env}
                                            :env env}}))))

(defn -implies
  [env source-fn target-fn]
  (with-info {:env env}
    (let [result (or (not (source-fn))
                     (target-fn))]
      (debug {:type :implies
              :result result
              :env env})
      result)))

(defmethod parse-tla :implies
  [{:keys [source target env]}]
  `(with-info {:chain (concat (:chain *trace-view*) [:implies])}
     (-implies ~env
               (fn []
                 ~(parse-tla (assoc source :env env)))
               (fn []
                 ~(parse-tla (assoc target :env env))))))

;; Using the definition for leads-to: F ~> G == [](F => <>G).
(defmethod parse-tla :leads-to
  [{:keys [source target env]}]
  `(with-info {:chain (concat (:chain *trace-view*) [:leads-to])}
     (-always ~env
              (fn []
                ~(parse-tla {:type :implies
                             :env env
                             :source (assoc source :env env)
                             :target {:type :eventually
                                      :env env
                                      :child (assoc target :env env)}})))))

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
  (def result result)

  (:property temporal-property)
  (identity result)

  (temporal-property->map temporal-property)

  (let [compiled-property
        (eval `(fn [trace-view#]
                 (binding [*trace-view* trace-view#]
                   ~(parse-tla (temporal-property->map temporal-property)))))

        loopback-idx (-> result :trace-info :violation :state-number)
        behavior (->> (:trace result)
                      (mapv second))
        trace-view {:behavior (->> behavior
                                   (map-indexed (fn [idx v]
                                                  (assoc v ::idx idx)))
                                   vec)
                    :loopback-idx loopback-idx
                    :current-state (assoc (first behavior) ::idx 0)
                    :debug (atom [])}
        violated? (not (compiled-property trace-view))]
    (def ^:dynamic *trace-view* trace-view)
    (with-meta {:violated? violated?}
      {:violated? violated?
       :debug @(:debug trace-view)})))

(defn check-temporal-properties
  "Given a result (the dereffed return from `r/run-model`) and the temporal properties
  (`components`, which is the same set that you pass to `r/run-model` in the second argument,
  but we just care here about the temporal properties), it checks which temporal property
  are violated.

  Check `check-temporal-property` for more information about the return values."
  [result components]
  (let [properties (->> (flatten (seq components))
                        (filter #(= (type %) ::r/Property))
                        set)]
    (->> properties
         (sort-by :name)
         (mapv (fn [{:keys [name] :as property}]
                 [name (check-temporal-property result property)]))
         (into {}))))

(comment

  ;; TODO:
  ;; - [x] Try to encode operators using formal definitions
  ;;   - Page 88 of Specifying Systems (Sec. 8.1)
  ;;     - http://lamport.azurewebsites.net/tla/book-21-07-04.pdf
  ;; - [x] Always
  ;; - [x] Leads to
  ;; - [x] Eventually
  ;; - [x] Implies
  ;; - [x] For all
  ;; - [x] Call (invoke)
  ;; - [ ] Not
  ;; - [ ] For some
  ;; - [ ] And*
  ;; - [ ] Or*

  ())
