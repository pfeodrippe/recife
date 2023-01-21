(ns example.implementation.wire-1
  "Very simple implementation of `hillel.ch1-wire-1` spec
  se we can show how to call the \"real\" app with generated traces.

  It's not supposed to be one on one with the specification, but it will be
  very similar as it's a constrained example (we will have more complex ones,
  this is just a good starting point).

  We are interested in the app behaviour of a entire trace, we could have used the
  implementation to drive the spec, but this is unrealistic as a real app will
  comunicate with a real database or do requests, which will make the spec
  check too slow for any practical purposes, so we are not going down this road.
  "
  (:require
   [arrudeia.core :as ar]
   [hillel.ch1-wire-1 :as model]
   [recife.core :as r]
   [medley.core :as medley]
   [recife.analyzer :as analyzer]))

;; This is our "DB".
(def balances (atom {}))

(defn- adapt
  [args]
  (update args :money bigdec))

(defn- check-funds
  [{:keys [:sender :money] :as args}]
  ;; `arrudeia` is a library to help you with scheduling, `with-label`
  ;; enables you to give a identifier (just a keyowrd) to this step (which
  ;; is a arbitrary form) which will wait until it's enabled to run, see
  ;; `comment` form below.
  (ar/with-label ::check-funds
    (when (< money (@balances sender))
      args)))

(defn- withdraw!
  [{:keys [:sender :money] :as args}]
  (ar/with-label ::withdraw!
    (swap! balances update sender - money))
  args)

(defn- deposit!
  [{:keys [:receiver :money] :as args}]
  (ar/with-label ::deposit!
    (swap! balances update receiver + money))
  args)

(defn request
  [args]
  ;; `check-funds` can return `nil`, which will short-circuit the
  ;; pipeline.
  (some-> (adapt args)
          check-funds
          withdraw!
          deposit!)
  @balances)

(comment

  (def result @(r/run-model model/global
                            #{model/wire model/invariant}
                            {:fp 6
                             :seed -3669946775118883845}))

  (do
    (reset! balances {:alice 5M :bob 5M})
    (def x (ar/register nil (request {:money 4
                                      :sender :alice
                                      :receiver :bob})))
    (try
      (ar/run-step [x ::check-funds])
      (finally
        (Thread/sleep 100)
        (.close x))))

  ;; Run it manually by looking at the generated violated trace.
  (let [_ (reset! balances {:alice 5M :bob 5M})
        x (ar/register :x (request {:money 4
                                    :sender :alice
                                    :receiver :bob}))
        y (ar/register :y (request {:money 2
                                    :sender :alice
                                    :receiver :bob}))]
    (ar/run-processes! [[y ::check-funds]
                        [x ::check-funds]
                        [y ::withdraw!]
                        [x ::withdraw!]]
                       ;; We pass a step handler which adapts the response
                       ;; of this step, we are only interested on the balances so
                       ;; we can compare the values of these with the trace violation.
                       {:step-handler (fn [_] @balances)}))

  (->> @ar/semaphore
       :debug
       (map-indexed vector))

  ;; Automate it with some helpers.
  ;; We have some main points which need to be addresses (TODO):
  ;; - [x] Global initialization, what we need to call and how to adapt this data
  ;; -     from the trace to `arrudeia`.
  ;; - [x] Processes mapping, collect all the processes from the trace, map it
  ;; -     and "initilize" with any needed local variables.
  ;; - [x] Check impl global state with trace.
  ;; - [x] Test with traces from the states file.
  ;; - [x] Fix `arrudeia` in that it should not run the thread before any
  ;;       command. ATM it's running until the first step, but this may be
  ;;       underisable and hard to reason about.
  ;; - [x] Deal with context parameters besides `:self`.
  ;; - [x] Check impl local state with trace. Or maybe global state is enough?
  ;; - [ ] Maybe make dynamic processes work (processes created after state `0`)?
  ;; -     Do we need to assert the step which created the new process?
  ;; - [ ] Maybe make a protocol out of this?
  (def result
    @(r/run-model model/global #{model/wire model/invariant} {:fp 6
                                                              :seed -3669946775118883845
                                                              :dump-states? true}))

  (let [init (fn [initial-global-state] (reset! balances initial-global-state))
        implementation-mapping
        (->> {::model/check-funds
              {:step ::check-funds
               :register (fn [{:keys [:context :model-previous-state]}]
                           (let [amount (get-in model-previous-state [::r/procs (:self context) :amount])]
                             (request {:money amount
                                       :sender :account/alice
                                       :receiver :account/bob})))}

              ::model/deposit {:step ::deposit!}
              ::model/withdraw {:step ::withdraw!}}
             (medley/map-vals
              (fn [m]
                (assoc m
                       :handler (fn [_] (medley/map-vals int @balances))
                       :assertion (fn [{:keys [:model-state :output]}]
                                    {:expected (dissoc model-state ::r/procs :recife/metadata)
                                     :actual output})))))]
    (analyzer/analyze result {:init init
                              :implementation-mapping implementation-mapping
                              :number-of-traces 100}))

  ())

(comment

  ;; TODO:
  ;; [x] - We are not interested only in the final result, but also in the
  ;;       transient ones, so arrudeia can receive a function which adapts the
  ;;       response for us.

  ())
