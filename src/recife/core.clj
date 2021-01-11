(ns recife.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.xml :as xml]
   [lambdaisland.deep-diff2 :as ddiff]
   [medley.core :as medley]
   [recife.alloy :as ra]
   [recife.util.ordering :as ord])
  (:import
   (lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion)))

(defn- munge-kw
  [k]
  (if-some [k-ns (namespace k)]
    (keyword (-> k-ns (str "__" (name k)) (str/replace #"\." "_")))
    k))

(declare subset?)

(declare union)

(declare difference)

(defn one-of
  "Tells Recife to choose one of the values as its initial value."
  [s]
  {::type ::one-of
   ::possible-values s})

(defmulti parse-dynamic-expression type)

(defmethod parse-dynamic-expression clojure.lang.Symbol
  [expr]
  `(ra/join ~expr ~'value ~'time))

(defmethod parse-dynamic-expression clojure.lang.Keyword
  [expr]
  (-> expr munge-kw name symbol))

(defmethod parse-dynamic-expression java.lang.Long
  [expr]
  expr)

;; XXX: This represents a map that it's not dynamic, it will create
;; a "instance" of `__Map` using set comprehension.
(defrecord SubMap [v])

;; XXX: Invalid is a signature with multiplicity one that we forbid of
;; be any of the dynamic values. It's a hack so we can have exactly the map
;; that we want =D
(defmethod parse-dynamic-expression SubMap
  [{expr :v}]
  `(let [v# (ra/select (fn [~'m]
                         (= (ra/join ~'m
                                     ~'entries)
                            (ra/++ ~@(mapv (fn [[k v]]
                                             `{::value (ra/cartesian ~(parse-dynamic-expression k)
                                                                     ~(parse-dynamic-expression v))
                                               ::meta {:key-map ~(parse-dynamic-expression k)}})
                                           expr))))
                       ~'__Map)]
     (if (not (empty? v#))
       v#
       ~'__Invalid)))

(defmethod parse-dynamic-expression clojure.lang.IPersistentMap
  [expr]
  (let [res (mapv (fn [[k v]]
                    ;; TODO: We should check by the map at runtime instead of static.
                    `(= (ra/join ~(parse-dynamic-expression k)
                                 ~'value
                                 (ord/next ~'Time ~'time))
                        ~(parse-dynamic-expression (if (map? v)
                                                     ;; FIXME: This SubMap thing is very weird.
                                                     (SubMap. v)
                                                     v))))
                  expr)]
    (if (seq res)
      `(and ~@res)
      ;; XXX: If there is nothing at the hashmap, just set it to true.
      `(= 1 1))))

(defmethod parse-dynamic-expression clojure.lang.IPersistentSet
  [expr]
  (if (seq expr)
    `(ra/union ~@(mapv (fn [v]
                         (parse-dynamic-expression (if (map? v)
                                                     (SubMap. v)
                                                     v)))
                       expr))
    :none))

(defmethod parse-dynamic-expression clojure.lang.IPersistentList
  [expr]
  (let [expr (ra/macroexpand-all expr)
        op' (first expr)
        op'' (if (or (special-symbol? op')
                     (keyword? op')
                     (nil? (resolve op')))
               op'
               (symbol (resolve op')))
        op (condp = op''
             'recife.core/union 'recife.alloy/union
             'recife.core/difference 'recife.alloy/difference
             op'')]
    (cond
      (and (keyword? op)
           (= (last expr) 'global))
      (parse-dynamic-expression (symbol (name (munge-kw op))))

      (keyword? op)
      `(ra/join ~(symbol op) (ra/join ~(parse-dynamic-expression (last expr)) ~'entries))

      (and (symbol? op)
           (= (last expr) 'global))
      `(ra/join ~(parse-dynamic-expression (symbol (name (munge-kw op))))
                ~'value
                ~'time)

      (contains? `#{+ - * = < or and ra/union ra/difference not} op)
      `(~op ~@(map parse-dynamic-expression (rest expr)))

      ;; FIXME: `first` is working only for sets now.
      (= op 'clojure.core/first)
      `(let [~'ranks (ra/join ~(parse-dynamic-expression (last expr))
                              (ra/join ~'diff ~'rank))]
         (ra/select (fn [~'m]
                      (= (ra/join ~'m (ra/join ~'diff ~'rank))
                         (max ~'ranks)))
                    ~(parse-dynamic-expression (last expr))))
      ;; FIXME: Support `else`.
      (= op 'if)
      `(if ~(parse-dynamic-expression (nth expr 1))
         ~(parse-dynamic-expression (nth expr 2))
         ~(if (> (count expr) 3)
            (parse-dynamic-expression (nth expr 3))
            ;; XXX: We do this so it's not vacuously true.
            `(= 1 0)))

      (= op 'recife.core/implies)
      `(if ~(parse-dynamic-expression (nth expr 1))
         ~(parse-dynamic-expression (nth expr 2))
         (= 1 1))

      (= op 'clojure.core/seq)
      `(not (empty? ~(parse-dynamic-expression (nth expr 1))))

      (= op 'do)
      (parse-dynamic-expression (last expr))

      (= op 'recife.core/finished?)
      `(= ~(parse-dynamic-expression (last expr)) ~'__recife__done)

      :else
      (throw (ex-info "Dynamic expression not supported"
                      {:expr expr})))))

(defmethod parse-dynamic-expression clojure.lang.LazySeq
  [expr]
  (parse-dynamic-expression (apply list expr)))

(defmethod parse-dynamic-expression :default
  [expr]
  (throw (ex-info "parse-dynamic-expression error"
                  {:expr expr
                   :type (type expr)})))

;; FIXME: We will couple the initial value with the schema, but will change later.
(defn process
  [{:keys [:procs :global :local] r-while :while} labels labels-and-forms-m]
  (let [labels' (mapv #(symbol (str "__" (name %))) labels)
        munged-labels (conj labels' '__recife__done)
        new-sigs (->> munged-labels
                      (mapv (fn [label] {label {:multiplicity :one}}))
                      (apply merge))
        label->before-and-after-label (->> munged-labels
                                           (partition 2 1)
                                           (map vec)
                                           (zipmap labels))
        local (assoc local :pc (first munged-labels))
        keywordize-local (fn [proc k] (keyword (name proc) (name k)))
        local-with-ns (->> procs
                           (mapv (fn [proc]
                                   (medley/map-keys #(keywordize-local proc %) local)))
                           (apply merge))]
    {:new-sigs new-sigs
     :internal-pcs (->> procs
                        (map #(keyword (name %) "pc"))
                        (map (comp symbol munge-kw))
                        set)
     :local local-with-ns
     :vars (merge local-with-ns global)
     :steps (->> labels-and-forms-m
                 (medley/map-kv
                  (fn [label form]
                    (-> `(declare ~(symbol (name label))) eval)
                    [label
                     (let [global-keys (mapv munge-kw (keys global))
                           local-keys (keys local)
                           pred-args (concat global-keys local-keys)
                           [parsed-form metadata] (let [meta-collector (atom [])]
                                                    [(->> (parse-dynamic-expression (ra/macroexpand-all form))
                                                          (walk/postwalk (fn [x]
                                                                           (if-let [v (::meta x)]
                                                                             (do
                                                                               (swap! meta-collector conj v)
                                                                               (::value x))
                                                                             x))))
                                                     @meta-collector])
                           ;; TODO: Change it so it handles if branches correctly.
                           ;; ATM it is handling `when` only.
                           unchanged-objs (->> (if (list? form)
                                                 (last form)
                                                 form)
                                               (mapv key)
                                               (mapv #(cond
                                                        (symbol? %) (str (ra/parse %) ".value.time")
                                                        (namespace %) (name (munge-kw %))
                                                        :else (name (ra/parse %)))))]
                       {:internal-name (symbol (str "__" (name label)))
                        :metadata metadata
                        :form (ra/pred label
                                       (->> pred-args
                                            (mapcat (fn [k] [k :- :Obj]))
                                            (concat [:time :- :Time]))
                                       (if r-while
                                         `(if (not (= (ra/join ~(munge-kw (key (first r-while)))
                                                               ~'value
                                                               ~'time)
                                                      ~(if (empty? (val (first r-while)))
                                                         :none
                                                         (val (first r-while)))))
                                            (and (= (ra/join ~'pc ~'value (ord/next ~'Time ~'time))
                                                    ~'__recife__done)
                                                 (ra/all [:a :- [:Obj :- :pc]]
                                                   (and (= (ra/join ~'a ~'value (ord/next ~'Time ~'time))
                                                           (ra/join ~'a ~'value ~'time)))))
                                            (and (= (ra/join ~'pc ~'value (ord/next ~'Time ~'time))
                                                    (ra/join ~'pc ~'value ~'time))
                                                 ~parsed-form
                                                 (ra/all [:a :- [:Obj :- ~(if (seq unchanged-objs)
                                                                            (format "{%s}"
                                                                                    (str/join " + " unchanged-objs))
                                                                            :none)
                                                                 :- :pc]]
                                                   (and (= (ra/join ~'a ~'value (ord/next ~'Time ~'time))
                                                           (ra/join ~'a ~'value ~'time))))))
                                         `(and ~@(let [[before after] (label->before-and-after-label label)]
                                                   [`(= (ra/join ~'pc ~'value ~'time)
                                                        ~before)
                                                    `(= (ra/join ~'pc ~'value (ord/next ~'Time ~'time))
                                                        ~after)])
                                               ~parsed-form
                                               (ra/all [:a :- [:Obj :- ~(if (seq unchanged-objs)
                                                                          (format "{%s}"
                                                                                  (str/join " + " unchanged-objs))
                                                                          :none)
                                                               :- :pc]]
                                                 (and (= (ra/join ~'a ~'value (ord/next ~'Time ~'time))
                                                         (ra/join ~'a ~'value ~'time)))))))
                        :invokes (for [proc procs]
                                   (->> local-keys
                                        (map (fn [k]
                                               (-> (keywordize-local proc k)
                                                   munge-kw
                                                   name
                                                   (str "_0")
                                                   symbol)))
                                        (concat [(symbol (name label)) 'time]
                                                (->> global-keys
                                                     (mapv (comp symbol #(str % "_0") name))))))})])))}))

(defmacro with-proc
  [m & labels-and-forms]
  `(process ~m
            (vec (take-nth 2 '~labels-and-forms))
            '~(->> labels-and-forms
                   (partition 2)
                   (mapv vec)
                   (into {}))))

(defmacro checker
  [body]
  `(parse-dynamic-expression (quote ~body)))

(defmacro finder
  [body]
  `(parse-dynamic-expression (list `not (quote ~body))))

(declare finished?)

(declare implies)

(defn- parse-initial-value [v]
  (cond
    (= (::type v) ::one-of)
    (let [v' (->> (::possible-values v)
                  (mapv parse-initial-value))]
      `(ra/union ~@v'))

    (int? v)
    v

    (or (symbol? v)
        (keyword? v))
    (munge-kw v)

    (coll? v)
    (if (seq v)
      (->> v
           (mapv parse-initial-value)
           set)
      :none)

    :else
    {:__WRONG (type v)}))

(defn parse-proc
  [{:keys [:vars :steps :new-sigs :internal-pcs]}]
  (let [schema' (->> vars
                     (medley/map-keys munge-kw)
                     (medley/map-vals
                      (fn [v]
                        {:multiplicity :one
                         :extends :Obj})))
        all-metadata (mapcat (comp :metadata val) steps)
        map-types (->> all-metadata
                       (filter :key-map)
                       (map :key-map)
                       set)
        schema (-> schema'
                   (assoc :Time {:ordered? true}
                          :__Invalid {:multiplicity :one}
                          :Obj {:relations {:value [:univ :set :-> 'Time]}
                                :abstract? true})
                   (merge new-sigs))
        initial-expressions (->> vars
                                 (medley/map-keys munge-kw)
                                 (medley/map-vals parse-initial-value)
                                 (mapv (fn [[k v]]
                                         (cond
                                           (= v :none)
                                           `(and (ra/subset? (ra/join ~(name k) ~'value (ord/first ~'Time))
                                                             ~v))

                                           (set? v)
                                           `(and (= (ra/join ~(name k) ~'value (ord/first ~'Time))
                                                    (ra/union ~@v)))

                                           :else
                                           `(and (ra/subset? (ra/join ~(name k) ~'value (ord/first ~'Time))
                                                             ~v)
                                                 (= (count (ra/join ~(name k) ~'value (ord/first ~'Time)))
                                                    1))))))]
    {:vars vars
     :initial-expressions initial-expressions
     :map-types map-types
     :internal-pcs internal-pcs
     :steps steps
     :preds (map (comp :form val) steps)
     :schema schema}))

(defn build-model
  [procs]
  (let [vars (apply merge (map :vars procs))
        steps (apply merge (map :steps procs))
        map-type-signatures (->> (apply set/union (map :map-types procs))
                                 (map #(vector % {:multiplicity :one
                                                  :extends :__MapDomain}))
                                 (into {}))
        r-all `(ra/all ~(vec
                         (concat '[time :- [Time - last]]
                                 (->> vars
                                      (medley/map-keys munge-kw)
                                      (medley/map-vals parse-initial-value)
                                      (mapcat (fn [[k _v]]
                                                [(symbol (str (name k) "_" 0)) :- (symbol (name k))]))
                                      vec)))
                 (or ~@(mapcat (comp :invokes val) steps)
                     (and ~@(->> vars
                                 (medley/map-keys munge-kw)
                                 (medley/map-vals parse-initial-value)
                                 (map (fn [[k _v]]
                                        (let [sym (symbol (str (name k) "_" 0))]
                                          `(and (= (ra/join ~sym ~'value (ord/next ~'Time ~'time))
                                                   (ra/join ~sym ~'value ~'time))))))
                                 vec))))]
    {:trace (ra/fact 'Trace
                     `(and ~@(mapcat :initial-expressions procs)
                           ~r-all))
     :internal-pcs (apply set/union (map :internal-pcs procs))
     :map-types (apply set/union (map :map-types procs))
     :steps steps
     :preds (map (comp :form val) steps)
     :signatures (ra/signatures (-> (apply merge (map :schema procs))
                                    (assoc :__Map {:relations {:entries [:__MapDomain :-> :lone :univ]}}
                                           :__MapDomain {:abstract? true})
                                    (merge map-type-signatures)))}))

(defn xml->timeline
  []
  (let [field-maps (->> (xml/parse "alloy_example_output.xml")
                        xml-seq
                        (filter (comp #{:field} :tag)))
        map->value (->> field-maps
                        (filter (comp #{"entries"} :label :attrs))
                        first
                        :content
                        (filter (comp #{:tuple} :tag))
                        (mapv :content)
                        (mapv (fn [atoms] (mapv (comp :label :attrs) atoms)))
                        (group-by first)
                        (medley/map-vals #(->> %
                                               (mapv (fn [[_ k v]]
                                                       [(keyword (subs k 0 (- (count k) 2)))
                                                        v]))
                                               (into {}))))
        timeline' (->> field-maps
                       (filter (comp #{"value"} :label :attrs))
                       first
                       :content
                       (filter (comp #{:tuple} :tag))
                       (mapv :content)
                       (mapv (fn [atoms] (mapv (comp :label :attrs) atoms)))
                       (group-by last)
                       (medley/map-vals
                        (fn [v]
                          (->> v
                               (mapv (fn [[k-str v _]]
                                       (let [[ns' key'] (-> k-str
                                                            (subs 0 (- (count k-str) 2))
                                                            clojure.repl/demunge
                                                            (str/split #"--"))]
                                         [(keyword ns' key')
                                          (let [map-value (map->value v)]
                                            (cond
                                              map-value map-value
                                              :else v))])))
                               (group-by first)
                               (medley/map-vals (fn [coll]
                                                  (->> coll
                                                       (mapcat #(drop 1 %))
                                                       set)))
                               (medley/map-kv
                                (fn [k set']
                                  [k
                                   (if (= (name k) "pc")
                                     (let [value-k (-> (first set')
                                                       (subs 2 (- (count (first set')) 2))
                                                       clojure.repl/demunge
                                                       keyword)]
                                       (if (= value-k :recife--done)
                                         :recife/done
                                         value-k))
                                     (->> set'
                                          (mapv #(if (str/includes? % "__")
                                                   (let [[ns' key'] (-> %
                                                                        (subs 0 (- (count %) 2))
                                                                        clojure.repl/demunge
                                                                        (str/split #"--"))]
                                                     (keyword ns' key'))
                                                   %))
                                          set))])))))
                       (medley/map-keys #(-> %
                                             (str/split #"\$")
                                             last
                                             Integer/parseInt))
                       (sort-by first)
                       (map last))
        default-values (->> timeline'
                            (mapcat keys)
                            distinct
                            (mapv #(vector % #{}))
                            (into {}))
        timeline (mapv #(merge default-values %) timeline')]
    timeline))

(defn run-procs
  ([procs]
   (run-procs procs nil))
  ([procs checker-expr]
   (let [{:keys [:trace :signatures :preds :steps :internal-pcs :map-types]} (->> procs
                                                                                  (map parse-proc)
                                                                                  build-model)]
     (when (-> signatures
               (ra/build-alloy-module
                (conj preds trace)
                {:run (format (str "one sig diff{
  rank : (univ - Int) -> one Int
}"
                                   "\n"
                                   "fact {some time: Time | {%s} and {all disj m1, m2: __Map | {%s}} and {no value.Time :> __Invalid}}\n\n"
                                   "run {} for 9 but 6 Int\n\n"
                                   (when checker-expr
                                     (format "check {all time: Time | %s} for 9 but 6 Int"
                                             (ra/parse checker-expr))))
                              (->> internal-pcs
                                   (mapv ra/custom-munge)
                                   (mapv #(format "%s.value.time = __recife__done" %))
                                   (str/join " and "))
                              (->> map-types
                                   (mapv ra/custom-munge)
                                   (mapv #(format "m1.entries[%s] != m2.entries[%s]" % %))
                                   (str/join " or ")))})
               (ra/run-alloy-expression {:expression "1"})
               ra/parse-alloy-result)
       (xml->timeline)))))

(defn timeline-diff
  [timeline]
  (->> timeline
       (partition 2 1)
       (map #(ddiff/diff (first %) (second %)))
       (map (fn [step]
              (medley/filter-vals (fn [v]
                                    (let [result (atom false)]
                                      (clojure.walk/prewalk (fn [form]
                                                              (when (or (instance? Mismatch form)
                                                                        (instance? Deletion form)
                                                                        (instance? Insertion form))
                                                                (reset! result true))
                                                              form)
                                                            v)
                                      @result))
                                  step)))
       (cons (first timeline))))

(defn print-timeline-diff
  [timeline]
  (ddiff/pretty-print (timeline-diff timeline)))

(defn next-solution!
  []
  (when (ra/next-solution!)
    (xml->timeline)))
