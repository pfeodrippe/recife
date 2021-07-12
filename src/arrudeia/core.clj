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

(defn debug!
  [v]
  (swap! semaphore update :debug conj v))

(defn waiting-step
  ([[proc-name keyword idx]]
   (waiting-step {} [proc-name keyword idx]))
  ([args [proc-name keyword idx]]
   (when (not *bypass*)
     (debug! {:waiting-step {:proc-name proc-name
                             :keyword keyword
                             :idx idx
                             :stage :init}})
     (while (not (or (and (get @semaphore [proc-name :arrudeia/next])
                          ;; TODO: Change this to use current.
                          (not (contains? #{keyword idx}
                                          (get @semaphore [proc-name :arrudeia/next]))))
                     ;; If it's the first step (init) and we cannot continue,
                     ;; then wait.
                     (and (= keyword :arrudeia/init) (get-in @semaphore [proc-name :arrudeia/started?]))
                     (= (get-in @semaphore [proc-name idx]) :start)
                     (= (get-in @semaphore [proc-name keyword]) :start)))
       (when (Thread/interrupted)
         (.interrupt (Thread/currentThread))))
     (when (and (get @semaphore [proc-name :arrudeia/next])
                (contains? #{keyword idx}
                           (get @semaphore [proc-name :arrudeia/next])))
       (swap! semaphore assoc [proc-name :arrudeia/next] nil))
     (debug! {:start [proc-name keyword idx]}))
   args))

(defn done-step
  [args [proc-name keyword idx]]
  (if (not *bypass*)
    (let [{:keys [:cancel-execution?]} (get-in @semaphore [proc-name :arrudeia/params keyword])]
      ;; You may want to cancel the execution if you are the last step so you don't
      ;; run the rest of the code, stopping in the side effect of this step.
      (debug! {:done [proc-name keyword idx]})
      (swap! semaphore assoc-in [proc-name [idx :args-after]] args)
      (swap! semaphore assoc-in [proc-name idx] :done)
      (swap! semaphore assoc-in [proc-name [keyword :args-after]] args)
      (swap! semaphore assoc-in [proc-name keyword] :done)

      (when cancel-execution?
        (throw (InterruptedException. "Cancelling `Arrudeia`  execution")))

      ;; Apply result-modifier to args.
      (try
        ((or (get *result-modifiers* keyword)
             (get *result-modifiers* idx)
             identity)
         args)
        (finally
          ;; If we are not the actual target step, don't need to wait.
          (when (or (= (get-in @semaphore [proc-name :arrudeia/previous-step]) keyword)
                    (= keyword :arrudeia/init))
            ;; Wait until we can continue so we don't do unwanted side effects.
            (while (not (get-in @semaphore [proc-name :arrudeia/params keyword :continue?]))
              (when (Thread/interrupted)
                (.interrupt (Thread/currentThread))))
            ;; Blocks next step.
            (debug! {:blocks-next-step [proc-name keyword idx]})
            (swap! semaphore assoc-in [proc-name :arrudeia/params keyword :continue?] false)))))
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
       (let [result# (do ~@body)]
         (done-step result# [*proc-name* identifier# idx#])))))

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
    @proc)

  java.io.Closeable
  (close [_]
    (future-cancel proc)))

(prefer-method pp/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref)

(defmacro register
  [proc-name pipe & [{:keys [:result-modifiers]}]]
  `(do (swap! semaphore (constantly {:debug []}))
       (let [temp-proc-name# ~proc-name
             proc-name# (or temp-proc-name# (keyword (str (gensym))))
             p# (-> {:proc (future
                             (binding [*proc-name* proc-name#
                                       *bypass* false
                                       *result-modifiers* ~result-modifiers]
                               (try
                                 (with-label :arrudeia/init nil)
                                 ~pipe
                                 ;; If a thread is stopped, do not show exception.
                                 (catch InterruptedException _#)
                                 (catch java.lang.ThreadDeath _#)
                                 (catch Exception e#
                                   (clojure.pprint/pprint
                                    {:EXCEPTION
                                     {proc-name# e#}})
                                   (swap! exceptions conj {proc-name# e#}))
                                 (catch Error e#
                                   (clojure.pprint/pprint
                                    {:EXCEPTION
                                     {proc-name# e#}})
                                   (swap! exceptions conj {proc-name# e#})))))
                     :proc-name proc-name#}
                    map->ArrudeiaProcess)]
         p#)))

(defn run-step
  ([proc-with-step]
   (run-step proc-with-step {}))
  ([[{:keys [:proc-name :proc]} step]
    {:keys [:run-intermediate-steps? :step-opts]
     :or {run-intermediate-steps? true}}]
   (debug! {:run-step {:proc-name proc-name
                       :stage :init
                       :step step}})
   (let [previous-step (get-in @semaphore [proc-name :arrudeia/previous-step] :arrudeia/init)
         cancel-execution? (:cancel-execution? step-opts)]
     (when (= previous-step :arrudeia/init)
       ;; First time we are calling `run-step` for this proc.
       (swap! semaphore assoc-in [proc-name :arrudeia/started?] true))

     ;; Continue execution from last step.
     (swap! semaphore assoc-in [proc-name :arrudeia/params previous-step :continue?] true)

     ;; Wait until it's blocked again.
     (debug! {:run-step {:proc-name proc-name
                         :stage :waiting-to-continue
                         :previous-step previous-step
                         :step step}})
     (while (get-in @semaphore [proc-name :arrudeia/params previous-step :continue?]))
     (debug! {:run-step {:proc-name proc-name
                         :stage :continuing
                         :previous-step previous-step
                         :step step}})

     ;; Last step is now the current step.
     (swap! semaphore assoc-in [proc-name :arrudeia/previous-step] step)

     ;; Add params so the process can read it.
     (swap! semaphore assoc-in [proc-name :arrudeia/params step] (merge step-opts {:proc-future proc}))
     (when run-intermediate-steps?
       (swap! semaphore assoc [proc-name :arrudeia/next] step))

     ;; Trigger new step.
     (swap! semaphore assoc-in [proc-name step] :start)

     ;; Wait for triggered step.
     (while (not= (get-in @semaphore [proc-name step]) :done))

     ;; Step could be used again at pipeline, so we reset its associated values.
     (swap! semaphore assoc-in [proc-name step] nil)
     (swap! semaphore assoc-in [proc-name :arrudeia/params step] {})
     (get-in @semaphore [proc-name [step :args-after]]))))

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
                steps)))))

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
  ;; - [ ] Maybe make it thread friendly where the user passes the atom? Or
  ;;       where the user initializes a object.
  ;; - [ ] There are some side-effects when you run again, maybe the user
  ;;       initiliazing the atom will help with it.
  ;; - [ ] Add to docs that you should `.close` the process so you don't have
  ;;       leaked objects.
  ;; - [ ] Maybe use `locking` instead of a semaphore.

  ())
