(ns arrudeia.core
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.walk :as walk]
   [clojure.pprint :as pp]))

(def ^:dynamic *proc-name*)

(def ^:dynamic *result-modifiers* {})

(def ^:dynamic *bypass* true)

(def disable-macros?
  (= (System/getenv "ARRUDEIA_DISABLE_MACROS") "1"))

(defmacro with-bypass
  [& body]
  `(binding [*bypass* true]
     ~@body))

(defmacro without-bypass
  [& body]
  `(binding [*bypass* false]
     ~@body))

(defonce semaphore (atom {:debug []}))
(defonce exceptions (atom []))

(defn waiting-step
  ([[proc-name keyword idx]]
   (waiting-step {} [proc-name keyword idx]))
  ([args [proc-name keyword idx]]
   (when (not *bypass*)
     (while (not (or (and (get @semaphore [proc-name :arrudeia/next])
                          (not (contains? #{keyword idx}
                                          (get @semaphore [proc-name :arrudeia/next]))))
                     (= (get-in @semaphore [proc-name idx]) :start)
                     (= (get-in @semaphore [proc-name keyword]) :start)))
       (when (Thread/interrupted)
         (.stop (Thread/currentThread))))
     (when (and (get @semaphore [proc-name :arrudeia/next])
                (contains? #{keyword idx}
                           (get @semaphore [proc-name :arrudeia/next])))
       (swap! semaphore assoc [proc-name :arrudeia/next] nil))
     (swap! semaphore update :debug conj {:start [proc-name keyword idx]}))
   args))

(defn done-step
  [args [proc-name keyword idx]]
  (if (not *bypass*)
    (let [{:keys [:cancel-execution?]} (get-in @semaphore [proc-name :arrudeia/opts keyword])]
      ;; You may want to cancel the execution if you are the last step so you don't
      ;; run the rest of the code, stopping in the side effect of this step.
      (swap! semaphore update :debug conj {:done [proc-name keyword idx]
                                           :cancel-execution? cancel-execution?})
      (swap! semaphore assoc-in [proc-name [idx :args-after]] args)
      (swap! semaphore assoc-in [proc-name idx] :done)
      (swap! semaphore assoc-in [proc-name [keyword :args-after]] args)
      (swap! semaphore assoc-in [proc-name keyword] :done)
      (if cancel-execution?
        (.interrupt (Thread/currentThread))
        ;; Apply result-modifier to args.
        ((or (get *result-modifiers* keyword)
             (get *result-modifiers* idx)
             identity)
         args)))
    args))

(defn var->keyword
  [v]
  (keyword (str (:ns (meta v)))
           (str (:name (meta v)))))

(defmacro ^:deprecated label
  "Use `with-label` instead."
  [{:keys [:identifier :idx]} & body]
  (if disable-macros?
    `(do ~@body)
    `(do
       (waiting-step {} [*proc-name* ~identifier ~idx])
       (done-step
        ~@body
        [*proc-name* ~identifier ~idx]))))

(defmacro with-label
  [opts & body]
  (if disable-macros?
    `(do ~@body)
    `(let [opts# ~opts
           identifier# (or (:identifier opts#)
                           ;; If we don't have a identifier, then `opts`
                           ;; is a keyword.
                           opts#)
           idx# (:idx opts#)]
       (waiting-step {} [*proc-name* identifier# idx#])
       (done-step
        ~@body
        [*proc-name* identifier# idx#]))))

(defn build-thread-first-macro-body
  [->-macro & forms]
  (let [keyword-steps
        (map-indexed (fn [idx form]
                       (cond
                         (and (zero? idx)
                              (symbol? form)) (keyword (str *ns*) (str form))
                         (symbol? form) (var->keyword (resolve form))
                         (list? form) (let [res-form (resolve (first form))]
                                        (when-not (:macro (meta res-form))
                                          (var->keyword res-form)))
                         :else idx))
                     forms)]
    `(~->-macro
      ~@(->> forms
             (map (fn [idx k form]
                    (cond
                      (zero? idx)
                      `(with-label {:identifier ~(:arrudeia/name (meta form) k)
                                    :idx ~idx}
                         ~form)

                      (nil? k) `((fn [args#] (-> args# ~form)))
                      :else
                      `((fn [args#]
                          (with-label {:identifier ~(:arrudeia/name (meta form) k)
                                       :idx ~idx}
                            (-> args# ~form))))))
                  (range)
                  keyword-steps)))))

(defmacro thread-first-macro-builder
  "Yes, a macro that creates another macro.
  It should be useful when you have some thread first
  macro from some of your dependencies (e.g. cats.core/->=) and
  you would like to use arrudeia.

  It also creates a new data reader so you could use it
  with a tagged literal."
  [name ->-macro]
  `(do
     (defmacro ~(symbol name)
       ([~'& ~'forms]
        (if disable-macros?
          `(~~->-macro ~@~'forms)
          (apply build-thread-first-macro-body ~->-macro ~'forms))))

     (defn ~(symbol (str name "-reader"))
       [~'form]
       (if disable-macros?
         ~'form
         (walk/postwalk
          (fn [v#]
            (if (and (symbol? v#)
                     (= (resolve ~->-macro)
                        (resolve v#)))
              (symbol (str ~*ns* "/" ~name))
              v#))
          ~'form)))))

(thread-first-macro-builder "->*" `->)

(defrecord ArrudeiaProcess [proc proc-name]
  clojure.lang.IDeref
  (deref [_]
    ;; this `swap!` means that processes which are deferred will be
    ;; run until completion
    (swap! semaphore assoc [proc-name :arrudeia/next] :arrudeia/until-the-end)
    @proc))

(prefer-method pp/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref)

(defmacro register
  [proc-name pipe & [{:keys [:result-modifiers]}]]
  `(do (swap! semaphore (constantly {:debug []}))
       (let [p# (-> {:proc (future
                             (binding [*proc-name* ~proc-name
                                       *bypass* false
                                       *result-modifiers* ~result-modifiers]
                               (try
                                 ~pipe
                                 ;; If a thread is stopped, do not show exception.
                                 (catch java.lang.ThreadDeath _#)
                                 (catch Exception e#
                                   (clojure.pprint/pprint
                                    {:EXCEPTION
                                     {~proc-name e#}})
                                   (swap! exceptions conj {~proc-name e#}))
                                 (catch Error e#
                                   (clojure.pprint/pprint
                                    {:EXCEPTION
                                     {~proc-name e#}})
                                   (swap! exceptions conj {~proc-name e#})))))
                     :proc-name ~proc-name}
                    map->ArrudeiaProcess)]
         p#)))

(defn run-step
  ([proc-with-step]
   (run-step proc-with-step {}))
  ([[{:keys [:proc-name :proc]} step]
    {:keys [:run-intermediate-steps? :step-opts]
     :or {run-intermediate-steps? true}}]
   (swap! semaphore assoc-in [proc-name :arrudeia/opts step] (merge step-opts {:proc-future proc}))
   (when run-intermediate-steps?
     (swap! semaphore assoc [proc-name :arrudeia/next] step))
   ;; Trigger new step.
   (swap! semaphore assoc-in [proc-name step] :start)
   ;; Wait for triggered step.
   (while (not= (get-in @semaphore [proc-name step]) :done))
   ;; Step could be used again at pipeline, so we reset its associated values.
   (swap! semaphore assoc-in [proc-name step] nil)
   (swap! semaphore assoc-in [proc-name :arrudeia/opts step] {})
   (get-in @semaphore [proc-name [step :args-after]])))

(defn cancel-remaining-steps
  [procs]
  (run! future-cancel (set (mapv :proc procs))))

(defn run-processes!
  "Returns a map of `steps` with their returned values.
  `:step-handler` is a function which receives a map with `:idx`, `:proc`,
  `:step` and `:response`, which is just the step response."
  ([steps]
   (run-processes! steps {}))
  ([steps {:keys [:step-handler]}]
   (swap! semaphore (constantly {:debug []}))
   (try
     (let [reversed-steps-proc-names (->> steps (mapv (comp :proc-name first)) reverse)
           ;; Find indexes which should have the execution cancelled (last step of
           ;; a proc).
           cancelled-indexes (->> (distinct reversed-steps-proc-names)
                                  (mapv #(- (dec (count steps))
                                            (.indexOf reversed-steps-proc-names %)))
                                  set)]
       (->> steps
            (map-indexed (fn [idx [proc step :as step']]
                           (let [response (run-step step' {:step-opts
                                                           (when (contains? cancelled-indexes idx)
                                                             {:cancel-execution? true})})]
                             (if step-handler
                               (step-handler {:idx idx
                                              :proc (:proc-name proc)
                                              :step step
                                              :response response})
                               response))))
            (mapv (fn [[proc step] response]
                    {:proc (:proc-name proc)
                     :step step
                     :response response})
                  steps)))
     (finally
       (try
         (->> steps
              (map first)
              set
              (run! deref))
         (catch java.util.concurrent.CancellationException _))))))

(defn parse-process-names
  [process-name->process process-with-steps]
  (->> process-with-steps
       (mapv #(vector (process-name->process (first %))
                      (last %)))))

(defn valid-interleavings
  "It returns all valid interleavings for processes with their steps.

  For the example below, see that, for any interleaving, `:step2` never comes
  before of `:step1` and `:step3` never appears before `:step2`. The order
  passed at input is respected at interleavings so you always have valid
  steps (we trust you to pass it correctly).

  Usage example:
  (valid-interleavings [[:t1 :step1]
                        [:t1 :step2]
                        [:t1 :step3]]
                       [[:t2 :other-step-1]
                        [:t2 :other-step-2]])
  =>
  [...           ;; other interleavings
   [[:t1 :step1]
    [:t2 :other-step-1]
    [:t1 :step2]
    [:t1 :step3]
    [:t2 :other-step-2]]
   ...]          ;; other interleavings
  "
  [& processes-with-steps]
  (->> (combo/permutations (apply concat processes-with-steps))
       (filter #(reduce (fn [_acc steps]
                          (if (= steps (filter (set steps) %))
                            true
                            (reduced false)))
                        true
                        processes-with-steps))))

(comment

  ;; TODO:
  ;; - [ ] Graceful thread cancelling.

  ())
