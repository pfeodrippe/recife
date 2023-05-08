(ns recife.clerk
  "You should have https://clojars.org/io.github.nextjournal/clerk and
  https://clojars.org/io.github.pfeodrippe/dev-tooling on the
  classpath to use this namespace.

  The functions require clerk on demand as we don't want to affect the startup
  time for a Recife model run."
  (:require
   [clojure.pprint :as pp]
   [com.pfeodrippe.tooling.clerk :as-alias tool.clerk]
   [com.pfeodrippe.tooling.clerk.util :as-alias tool.clerk.util]
   [nextjournal.clerk :as-alias clerk]
   [nextjournal.clerk.config :as clerk.config]
   [nextjournal.clerk.viewer :as-alias v]
   [recife.buffer :as-alias r.buf]
   [recife.core :as-alias r]
   [recife.model :as-alias rm]
   [recife.helpers :as rh]))

(defmacro with-recife
  [& body]
  (when (not (System/getProperty "RECIFE_OPTS_FILE_PATH"))
    (require '[com.pfeodrippe.tooling.clerk :as tool.clerk])
    (require '[com.pfeodrippe.tooling.clerk.util :as tool.clerk.util])
    (require '[nextjournal.clerk :as clerk])
    (require '[nextjournal.clerk.viewer :as v])
    (require '[recife.buffer :as r.buf])
    (require '[recife.core :as r])
    (require '[recife.model :as rm])
    `(do ~@body)))

(with-recife
  (r.buf/watch! ::r/status
                (fn [_status]
                  (future (clerk/recompute!)))))

(defmacro example
  [& body]
  (when clerk.config/*in-clerk*
    `(with-recife
       (clerk/with-viewer v/examples-viewer
         (mapv (fn [form# val#]
                 {:form form#
                  :val val#})
               ~(mapv (fn [x#] `'~x#) body)
               ~(vec body))))))

(def ^:private ident-viewer
  (with-recife
    {:transform-fn
     (fn [wrapped-value]
       (-> wrapped-value
           clerk/mark-preserve-keys
           (update-in [:nextjournal/value :form] clerk/code)))

     :render-fn
     '(fn [{:keys [form val]} opts]
        [:div
         {:class "py-[7px]"}
         [:div [:div.bg-slate-100.px-2.rounded
                (nextjournal.clerk.render/inspect-presented opts form)]]
         [:div.flex.mt-1
          (nextjournal.clerk.render/inspect-presented opts val)]])}))

(def -main-viewer
  (with-recife
    {:transform-fn
     (v/update-val
      (fn [example]
        (mapv (partial v/with-viewer ident-viewer) [example])))

     :render-fn
     '(fn [examples opts]
        (into [:div.border-l-2.border-slate-300.pl-4
               [:div.uppercase.tracking-wider.text-xs.font-sans.text-blue-500.mt-4.mb-2
                "Recife"]]
              (nextjournal.clerk.render/inspect-children opts)
              examples))}))

(defonce model-lock (Object.))

(defmacro -run-model
  [form & body]
  (when clerk.config/*in-clerk*
    `(with-recife
       (clerk/with-viewer -main-viewer
         {:form '~form
          :val (do ~@body)}))))

(defonce *cache (atom {}))

(rh/defconstraint finite-trace
  [_]
  (<= (rh/get-level) 100))

(defn -run-delayed
  [id cache-key global components {::keys [block] :as opts}]
  (with-recife
    (if (or block (tool.clerk/build?))
      (let [model @(r/run-model global components opts)]
        (swap! *cache assoc cache-key model)
        model)
      (let [*state (atom {:id id
                          :model nil})
            my-run (with-meta {:id id
                               :*state *state
                               :model
                               (future
                                 ;; We are sleeping so we don't have race
                                 ;; conditions between `show!` and `recompute!`,
                                 ;; see https://github.com/nextjournal/clerk/issues/414.
                                 (Thread/sleep 1000)
                                 (locking model-lock
                                   (let [model (r/run-model global components opts)]
                                     #_(println :>>>id (:id model))
                                     (try
                                       model
                                       (finally
                                         (swap! *state assoc :model model)
                                         (while (not= (:status (rm/model-state model))
                                                      :done)
                                           (Thread/sleep 100))))))
                                 #_(println :>>FINISHING id))}
                     {:type ::clerk-model})]
        (swap! *cache assoc cache-key my-run)
        my-run))))

(defmacro run-model
  "A wrapper over `recife.core/run-model` so we can present
  thins with Clerk, this will only run in Clerk's context!

  If you want to run it outside Clerk's context, use `recife.core/run-model`
  instead.

  Returned process is cached using form metadata `:recife.clerk/id` + some simple
  heuristics over the other arguments."
  {:arglists (:arglists (meta #'r/run-model))}
  ([global components]
   `(run-model ~global ~components nil ~(meta &form)))
  ([global components opts]
   `(run-model ~global ~components ~opts ~(meta &form)))
  ([global components opts form-meta]
   `(when #_(:only ~opts) true
      (-run-model
       ~(concat '(r/run-model)
                (if opts
                  (drop-last (drop 1 &form))
                  (drop-last (drop-last (drop 1 &form)))))
       (let [id# (::id ~form-meta)
             opts# (merge {:trace-example true}
                          ~opts)
             components# (set (conj ~components
                                    ;; Add constraint so we can always have
                                    ;; finite traces
                                    finite-trace))
             cache-key# {id# [(quote ~(drop-last &form))
                              ~global
                              (mapv :hash (sort-by :hash components#))
                              opts#]}]
         (or (get @*cache cache-key#)
             (-run-delayed id# cache-key#
                           ~global
                           components#
                           (merge opts#
                                  (select-keys ~form-meta [::block])))))))))

(defn- adapt-result
  [result]
  ;; Update trace elements so they contains the
  ;; pretty-printed version that we can use for the
  ;; node tooltip.
  (-> result
      (update :trace
              (fn [trace]
                (if (not (sequential? trace))
                  trace
                  (mapv (fn [[idx state]]
                          [idx (assoc state :printed
                                      (with-out-str
                                        (pp/pprint
                                         (dissoc state
                                                 ::r/procs
                                                 :recife/metadata))))])
                        trace))))))

(def ^:private render-response
  (with-recife
    (v/resolve-aliases
     {'render 'nextjournal.clerk.render
      'navbar 'nextjournal.clerk.render.navbar
      'viewer 'nextjournal.clerk.viewer
      'r 'reagent.core
      'v 'nextjournal.clerk.viewer}
     '(defn render-response
        [{:keys [trace trace-info trace-info-code experimental continue violations]
          :as value}
         opts]
        (v/html
         [v/with-d3-require {:package ["mermaid@9.4.0/dist/mermaid.js"]}
          (fn [mermaid]
            (when value
              [v/with-d3-require {:package ["elgrapho"]}
               (fn [elgrapho]
                 (cond
                   (seq violations)
                   [:div (nextjournal.clerk.render/inspect (v/code value))]

                   (not (sequential? trace))
                   [:div
                    [:br]
                    [:div.mt-5.bg-green-100.p-2
                     [:span.text-green-500.mr-3
                      "✔"]
                     [:span "No violations found"]]]

                   :else
                   (let [reg #".*android|.*Android.*|.*webOs.*|.*Android.*|.*iPhone.*"
                         mobile? (or (re-matches reg (str js/navigator.userAgent))
                                     (re-matches reg (str js/navigator.vendor))
                                     (re-matches reg (str js/window.opera))
                                     #_true)
                         {:keys [violation]} trace-info
                         stuttering? (= (:type violation) :stuttering)
                         model {:nodes
                                (->> trace
                                     (mapv (fn [[idx state]]
                                             {:x (+ 0 (* 0.1M idx))
                                              :y (+ 0 (* -0.1M idx))
                                              :group idx
                                              :label (if (= idx 0)
                                                       "0 - INITIAL"
                                                       (str idx
                                                            " - "
                                                            (-> (:recife/metadata state)
                                                                :context
                                                                first
                                                                name)
                                                            (if (and stuttering?
                                                                     (= (dec (count trace))
                                                                        idx))
                                                              " -- CRASH!"
                                                              "")))}))
                                     vec)

                                :edges
                                (cond-> (->> trace
                                             (partition 2 1)
                                             (mapv (fn [[[idx-0 state-0]
                                                         [idx-1 state-1]]]
                                                     {:from idx-0
                                                      :to idx-1})))
                                  (= (:type violation) :back-to-state)
                                  (conj {:from (dec (count trace))
                                         :to (:state-number violation)}))}
                         build-graph
                         (fn [el]
                           (new elgrapho
                                (clj->js
                                 {:model (-> model
                                             clj->js
                                             ((-> elgrapho
                                                  .-layouts
                                                  #_.-ForceDirected
                                                  .-Hairball
                                                  #_.-Cluster
                                                  #_.-Chord)))
                                  :container el
                                  :width 700
                                  :height 700
                                  ;; Disable animations at startup so we don't bounce
                                  ;; when zooming in.
                                  :animations false
                                  :arrows true
                                  #_ #_:glowBlend 0.2})))

                         build-mermaid
                         (fn [el]
                           (when el
                             (set! (.-innerHTML el) "")
                             (let [max-per-line 6
                                   group->node (->> (:nodes model)
                                                    (mapv
                                                     (fn [m]
                                                       (update m :label
                                                               #(-> (clojure.string/replace
                                                                     % #" " "")
                                                                    (clojure.string/replace
                                                                     #"-" "_")))))
                                                    (mapv (juxt :group identity))
                                                    (into (sorted-map)))
                                   render-label (fn [group]
                                                  (if (zero? group)
                                                    "[*]"
                                                    (str (:label (group->node group))
                                                         ":::_white")))
                                   render
                                   (fn [start-idx]
                                     (.render
                                      mermaid
                                      (str (gensym))
                                      (str (->> ["stateDiagram-v2"
                                                 "direction LR"
                                                 "classDef _white fill:white"]
                                                (clojure.string/join "\n"))
                                           "\n"
                                           (->> (mapv (fn [{:keys [from to]}]
                                                        (str "  "
                                                             (render-label from)
                                                             " --> "
                                                             (render-label to)))
                                                      (:edges model))
                                                (drop start-idx)
                                                (take max-per-line)
                                                (clojure.string/join "\n"))
                                           "\n")
                                      #(set! (.-innerHTML el) (str (.-innerHTML el) %))))]
                               (loop [start-idx 0]
                                 (when (< start-idx (count trace))
                                   (render start-idx)
                                   (recur (+ start-idx (inc max-per-line))))))))]
                     [:div.mt-5
                      (if (:trace-example trace-info)
                        [:div.bg-green-100.rounded-md.p-5
                         [:span.text-green-500.mr-3
                          "✔"]
                         [:span "No violations found (trace below is a valid one)"]]
                        [:div.bg-red-100.p-5
                         [:div.mb-2
                          [:span.text-red-500.mr-3 "⚠"]
                          [:span "Violation found"]]
                         [:div
                          [:div
                           (cond
                             (= (:type violation) :back-to-state)
                             (str "The system can be stuck in a loop, starting on state number "
                                  (:state-number violation)
                                  ", that will violate the following temporal  properties:")

                             stuttering?
                             [:span
                              (str "The system is allowed to stop (stutter) at any time "
                                   "if there is nothing saying that it cannot stop, you should "
                                   "define a ")
                              [:a {:href "https://www.hillelwayne.com/post/fairness/"}
                               "fair "]
                              [:span "action."]]

                             (= (:type violation) :deadlock)
                             [:span
                              (str "Deadlock was reached at step "
                                   (dec (count trace))
                                   ".")
                              [:br]
                              [:br]
                              (str "This means that there is no way to advance to a new state "
                                   "in this trace.")
                              [:br]
                              (str "If you don't care "
                                   "about deadlocks, you may disable it by "
                                   "passing `:no-deadlock` to `opts` or by using "
                                   "`r/done` to mark a process as completed.")]

                             (= (:type violation) :action-property)
                             (if (zero? (dec (count trace)))
                               (str "The following action property was violated by the initial state:")
                               (str "The following action property was violated by step "
                                    (dec (count trace))
                                    ":"))

                             :else
                             (if (zero? (dec (count trace)))
                               (str "The following invariant was violated by the initial state:")
                               (str "The following invariant was violated by step "
                                    (dec (count trace))
                                    ":")))]
                          (cond
                            (= (:type violation) :back-to-state)
                            (into [:ul]
                                  (->> (:violated-temporal-properties experimental)
                                       (filter (comp :violated? val))
                                       (mapv first)
                                       (mapv #(v/code (str "#'" (symbol %))))
                                       (mapv (comp (partial vector :li)
                                                   nextjournal.clerk.render/inspect))))

                            (contains? #{:invariant :action-property} (:type violation))
                            (into [:ul]
                                  (->> [(:name violation)]
                                       (mapv #(v/code (str "#'" (symbol %))))
                                       (mapv (comp (partial vector :li)
                                                   nextjournal.clerk.render/inspect)))))]])
                      [:div.mt-3
                       {:ref
                        (fn [el]
                          (when el
                            (if mobile?
                              (build-mermaid el)
                              (let [graph (build-graph el)]
                                ;; Zoom out a little bit so we have room for the
                                ;; nodes.
                                (.zoomToPoint graph 0 0
                                              (/ 1 1.1)
                                              (/ 1 1.1))
                                ;; Enable animations again after startup.
                                (set! (.-animations graph) true)
                                (set! (.-tooltipTemplate graph)
                                      (fn [idx el]
                                        (set! (.-innerHTML el)
                                              (str "<pre>\n"
                                                   (str "Step " idx
                                                        "\n"
                                                        "--------"
                                                        "\n"
                                                        (:printed (last (get trace idx))))
                                                   (str "</pre>")))))
                                graph))))}]])))]))])))))

(def recife-response-viewer
  (with-recife
    {:name ::recife-response-viewer
     :pred #(= (type %) ::r/RecifeResponse)
     :transform-fn (comp clerk/mark-presented
                         (clerk/update-val adapt-result))
     :render-fn (list 'do
                      render-response
                      '(fn [value]
                         (render-response value)))}))

(def ^:private model-render-fn
  (list 'do
        render-response
        '(fn [value]
           (when value
             (if (:status value)
               [:div.bg-gray-100.p-5.mt-2.w-full
                (if (= (:status value) :running)
                    [:div.animate-pulse
                     "⚙ Running"]
                    [:div.animate-pulse
                     "⌛ Waiting"])]
               (render-response value))))))

(def recife-model-viewer
  (with-recife
    {:name ::recife-model-viewer
     :pred #(= (type %) recife.core.RecifeModel)
     :transform-fn (comp clerk/mark-presented
                         (clerk/update-val
                          (fn [value]
                            (let [{:keys [status] :as model-state}
                                  (rm/model-state value)]
                              (if (= status :done)
                                (adapt-result (r/get-result value))
                                model-state)))))
     :render-fn model-render-fn}))

(def recife-model-clerk-viewer
  (with-recife
    {:name ::clerk-model-viewer
     :pred #(= (type %) ::clerk-model)
     :transform-fn (comp clerk/mark-presented
                         (clerk/update-val
                          (fn [{:keys [*state]}]
                            #_(println :>>STATE @*state)
                            (let [{:keys [model]} @*state]
                              (if model
                                (let [{:keys [status] :as model-state}
                                      (rm/model-state model)]
                                  #_(println :>>>>STATUS (:id model) status)
                                  (if (= status :done)
                                    (adapt-result (r/get-result model))
                                    model-state))
                                {:status :waiting})))))
     :render-fn model-render-fn}))

(with-recife
  (tool.clerk.util/add-global-viewers!
   [recife-response-viewer
    recife-model-viewer
    recife-model-clerk-viewer]))

(with-recife
  (clerk/recompute!))

(comment

  (clerk/recompute!)

  (do
    (def value
      (r/get-result)
      #_{:trace
         [[0
           {:recife.notebook.slow-start/hour 0,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}}}]
          [1
           {:recife.notebook.slow-start/hour 1,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [2
           {:recife.notebook.slow-start/hour 2,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [3
           {:recife.notebook.slow-start/hour 3,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [4
           {:recife.notebook.slow-start/hour 4,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [5
           {:recife.notebook.slow-start/hour 5,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [6
           {:recife.notebook.slow-start/hour 6,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [7
           {:recife.notebook.slow-start/hour 7,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [8
           {:recife.notebook.slow-start/hour 8,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [9
           {:recife.notebook.slow-start/hour 9,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [10
           {:recife.notebook.slow-start/hour 10,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [11
           {:recife.notebook.slow-start/hour 11,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [12
           {:recife.notebook.slow-start/hour 12,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [13
           {:recife.notebook.slow-start/hour 13,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [14
           {:recife.notebook.slow-start/hour 14,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [15
           {:recife.notebook.slow-start/hour 15,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [16
           {:recife.notebook.slow-start/hour 16,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [17
           {:recife.notebook.slow-start/hour 17,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [18
           {:recife.notebook.slow-start/hour 18,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [19
           {:recife.notebook.slow-start/hour 19,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [20
           {:recife.notebook.slow-start/hour 20,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [21
           {:recife.notebook.slow-start/hour 21,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [22
           {:recife.notebook.slow-start/hour 22,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]
          [23
           {:recife.notebook.slow-start/hour 23,
            :recife.core/procs
            #:recife.notebook.slow-start{:tick-v1
                                         {:pc
                                          :recife.notebook.slow-start/tick-v1}},
            :recife/metadata
            {:context
             [:recife.notebook.slow-start/tick-v1
              {:self :recife.notebook.slow-start/tick-v1}]}}]],
         :trace-info {:trace-example true},
         :distinct-states 24,
         :generated-states 25,
         :seed -1603827235550408286,
         :fp 25,
         :recife/transit-states-file-path
         "/var/folders/6r/vx4stgxd5yg6pbv_t_b2wttw0000gp/T/transit-output17294373035229478550.msgpack"})

    (def trace
      (:trace value))

    (def trace-info
      (:trace-info value))

    (def violation
      (:violation (:trace-info value)))

    (def stuttering?
      (= (:type violation) :stuttering))

    (def experimental
      (:experimental value)))

  ())
