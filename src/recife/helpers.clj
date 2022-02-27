(ns recife.helpers
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

(defn- invoke
  [form]
  (let [local-vars (->> (:local-vars *env*)
                        (mapv (fn [l]
                                (if (seq? l)
                                  (last l)
                                  l))))
        global-vars (->> (:global-vars *env*)
                         (mapv (fn [l]
                                 (if (seq? l)
                                   (last l)
                                   l))))]
    (if (or (keyword? (first form))
            (::operator? (meta (resolve (first form))))
            (= (namespace (symbol (resolve (first form)))) "recife.helpers"))
      form
      `[:invoke (quote ~(->> local-vars
                             (mapv (fn [l] [(keyword l) l]))
                             (into {})))
        (fn [{:keys ~(vec local-vars)
              :as db#}]
          (let [~(first global-vars) db#]
            ~form))])))

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
       (eval '[:for-all ~actual-bindings
               ~(binding [*env* (add-local-vars (keys actual-bindings))]
                  (if (> (count bindings) 2)
                    (for-all* (drop 2 bindings) body)
                    `(do ~(invoke body))))]))))

(defmacro for-all
  [bindings body]
  (for-all* bindings body))

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
