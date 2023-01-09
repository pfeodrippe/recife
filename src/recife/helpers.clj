(ns recife.helpers
  (:refer-clojure :exclude [and])
  (:require
   [clojure.math.combinatorics :as comb]
   [recife.core :as r])
  (:import (tlc2.tool.impl Tool)))

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
    (if (clojure.core/and (seq? form)
                          (not (keyword? (first form)))
                          (resolve (first form))
                          (or (::operator? (meta (resolve (first form))))
                              (= (namespace (symbol (resolve (first form)))) "recife.helpers")))
      form
      `[:invoke (quote ~(->> local-vars
                             (mapv (fn [l] [(keyword l) l]))
                             (into {})))
        (fn [{:keys ~(vec local-vars)
              :as db#}]
          (let [~(first global-vars) db#]
            ~form))])))

(defn- parse-decl
  [decl]
  (if (= (count decl) 2)
    [nil (first decl) (drop 1 decl)]
    [(first decl) (second decl) (drop 2 decl)]))

(defmacro defproperty
  {:arglists '([name doc-string? db])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)]
    `(r/defproperty ~name
       (binding [*env* (quote ~(add-global-vars
                                (mapv (fn [p#] `(quote ~p#))
                                      params)))]
         (eval '~@body)))))

(defmacro deffairness
  {:arglists '([name doc-string? db])}
  [name & decl]
  (let [[doc-string params body] (parse-decl decl)]
    `(def ~(with-meta name
             {:doc doc-string})
       (binding [*env* (quote ~(add-global-vars
                                (mapv (fn [p#] `(quote ~p#))
                                      params)))]
         (eval '~@body)))))

(defmacro definvariant
  {:arglists '([name doc-string? db])}
  [name & decl]
  (let [[doc-string params body] (parse-decl decl)]
    `(r/definvariant ~name
       ~doc-string
       (fn ~params
         ~@body))))

(defmacro defconstraint
  {:arglists '([name doc-string? db])}
  [name & decl]
  (let [[_doc-string params body] (parse-decl decl)]
    `(r/defconstraint ~name
       (fn ~params
         ~@body))))

(defn- for-all*
  [bindings body]
  (let [actual-bindings {`'~(first bindings) (second bindings)}]
    `(binding [*env* (quote ~(add-local-vars (keys actual-bindings)))]
       (eval '[:for-all ~actual-bindings
               ~(binding [*env* (add-local-vars (keys actual-bindings))]
                  (if (> (count bindings) 2)
                    (for-all* (drop 2 bindings) body)
                    `(do ~(invoke body))))]))))

(defmacro for-all
  [bindings body]
  (for-all* bindings body))

(defn- for-some*
  [bindings body]
  (let [actual-bindings {`'~(first bindings) (second bindings)}]
    `(binding [*env* (quote ~(add-local-vars (keys actual-bindings)))]
       (eval '[:exists ~actual-bindings
               ~(binding [*env* (add-local-vars (keys actual-bindings))]
                  (if (> (count bindings) 2)
                    (for-some* (drop 2 bindings) body)
                    `(do ~(invoke body))))]))))

(defmacro for-some
  [bindings body]
  (for-some* bindings body))

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

(defmacro fair
  [body]
  `[:fair
    ~(invoke body)])

(defmacro fair+
  [body]
  `[:fair+
    ~(invoke body)])

(defmacro call
  [k body]
  `[:call ~k
    ~(invoke body)])

(defmacro and
  [& body]
  `[:and ~@(mapv invoke body)])

(defn combine
  "Get all the possible combinations of the domain with the values.

  See https://www.learntla.com/core/functions.html#function-sets and
  `-maps` at https://github.com/Viasat/salt#maps."
  [domain values]
  (let [domain->pairs (->> (for [a (set domain)
                                 b (set values)]
                             [a b])
                           (group-by first))
        domain-seq? (when (every? number? domain)
                      (let [domain-max (apply max domain)
                            domain-range (range (inc domain-max))]
                        (= (set domain-range)
                           (set domain))))]
    (->> (apply comb/cartesian-product (vals domain->pairs))
         (mapv #(if domain-seq?
                  (mapv last (sort-by first %))
                  (into {} %)))
         set)))

(defn get-level
  "Get current TLC level (depth in a trace)."
  []
  #_(when Tool/currentState
    (.getLevel Tool/currentState)))
