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

(defn- invoke
  [form]
  (if (= (namespace (symbol (resolve (first form)))) "recife.tla")
    form
    `[:invoke (quote ~(->> (:local-vars *env*)
                           (mapv (fn [l]
                                   [(keyword l) l]))
                           (into {})))
      (fn [{:keys ~(vec (:local-vars *env*))
            :as db#}]
        (let [~(first (:global-vars *env*)) db#]
          ~form))]))

(defmacro leads-to
  [source target]
  `[:leads-to
    ~(invoke source)
    ~(invoke target)])

(defmacro always
  [body]
  `[:always
    ~(invoke body)])

(defmacro eventually
  [body]
  `[:eventually
    ~(invoke body)])
