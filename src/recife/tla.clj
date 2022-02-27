(ns recife.tla
  (:require
   [recife.core :as r]))

(def ^:dynamic *env* {:global-vars [] :local-vars []})

(defn- add-global-vars
  [vs]
  (update *env* :global-vars
          #(apply conj % vs)))

(defn- add-local-vars
  [vs]
  (update *env* :local-vars
          #(apply conj % vs)))

(defmacro defproperty
  [name params & body]
  `(r/defproperty ~name (binding [*env* (quote ~(add-global-vars
                                                 (mapv (fn [p#]
                                                         `(quote ~p#))
                                                       params)))]
                          (eval '~@body))))

(defn- for-all*
  [bindings body]
  (println :>>>>env-for-all *env*)
  (let [actual-bindings {`'~(first bindings) (second bindings)}]
    `(binding [*env* ~(add-local-vars (keys actual-bindings))]
       (eval '[:forall ~actual-bindings
               ~(if (> (count bindings) 2)
                  (binding [*env* (add-local-vars (keys actual-bindings))]
                    (for-all* (drop 2 bindings) body))
                  `(do ~@body))]))))

(defmacro for-all
  [bindings & body]
  (for-all* bindings body))

(defmacro leads-to
  [source target]
  `[:leads-to
    [:invoke (quote ~(->> (:local-vars *env*)
                          (mapv (fn [l]
                                  [(keyword l) l]))
                          (into {})))
     (fn [{:keys ~(vec (concat (:global-vars *env*)
                               (:local-vars *env*)))}]
       ~source)]
    (fn [{:keys ~(vec (concat (:global-vars *env*)
                              (:local-vars *env*)))}]
      ~target)])

#_(defmacro for-all
    []
    [:forall {'c clients
              'r resources}
     [:leads-to
      [:invoke {:c 'c :r 'r}
       (fn [{:keys [:c ::unsat :r]}]
         (contains? (get unsat c) r))]
      [:invoke {:c 'c :r 'r}
       (fn [{:keys [:c ::alloc :r]}]
         (contains? (get alloc c) r))]]])
