;; ◊page-name[{:subtitle "good things happen"}]{temporal}

;; Here we will talk about temporal and action properties, these are
;; ways to describe interactions between states, I really recommend
;; ◊link{https://learntla.com/core/temporal-logic.html}{Hillel's
;; article about it}.

(ns recife.notebook.temporal
  {:nextjournal.clerk/visibility {:result :hide}
   :clerk/name "temporal"}
  (:require
   [recife.clerk :as rc]
   [recife.core :as r]
   [recife.helpers :as rh]))

;; ◊title{Our previous spec}

;; In ◊xref{:doc/slow-start}, we ended up with the following spec.

(def global
  {::hour 0})

(r/defproc tick
  (fn [{::keys [hour] :as db}]
    (if (= hour 23)
      (assoc db ::hour 0)
      (update db ::hour inc))))

(rh/definvariant no-hour-after-23
  [{::keys [hour]}]
  (<= hour 23))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::tick-from-slow-start}
(rc/run-model global #{tick no-hour-after-23})

;; ◊title{Action properties}

;; An action property enables you to make assertions using the
;; current state (◊code{db})
;; ◊em{and} the next one (◊code{db')}.

;; ◊code{rh/defaction-property} is very similar to a invariant
;; (◊code{rh/definvariant}), but instead of receiving only the current
;; state, it also receives the next, so you can check stronger
;; properties.

;; Let's check if our clock is
;; ◊link{https://www.merriam-webster.com/dictionary/monotonic#:~:text=adjective,subscripts%20of%20the%20terms%20increase}{monotonic}
;; (which means that it should always increase the hours or at least stay the
;; same).

(rh/defaction-property monotonic
  ;; Note that we receive two arguments instead of only one!
  ;; The argument names are arbitrary, you could use
  ;; `[db-current db-next]` or whatever if you wanted.
  [db db']
  (>= (::hour db')
      (::hour db)))

;; ◊note{◊link{https://learntla.com/core/action-properties.html}{Check
;; Hillel's action properties}.}

;; This property should fail as we know that the clocks resets to 0
;; after some time (because of ◊code{tick}). Each state, by itself,
;; does not violate anything, what is violated is the ◊em{change} from
;; one state to the next one!

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::monotonic}
(rc/run-model global #{tick no-hour-after-23 monotonic})

;; While you can do more with action properties than with invariants,
;; they are related in that they are ◊em{safety properties}. Safety
;; properties are a subset of ◊em{temporal properties} which check that bad
;; things don't happen (we saw bad things happening for
;; ◊code{monotonic} while it didn't happen for
;; ◊code{no-hour-after-23}), but we also don't know if the spec is
;; really doing what we want it to do, we don't known if ◊em{good
;; things will happen}.

;; ◊title{Liveness}

;; Until now, you were able to check assertions against a state (or a
;; state and the next one), you may ask if there is more, could you
;; assert against the entire trace? You can betcha, and for this
;; we will be using ◊em{liveness properties}.

;; One of the meanings of liveness is "the quality or state of being
;; alive", and this suits really well into our context here, we want our
;; spec to be healthy, we want it doing something, advancing over
;; time, modifying states (a spec that does nothing does not violate
;; invariants or action properties, but it's also not useful at all!).

;; For our clock, we can check, for example, if we will eventually
;; reach hour 10.

(rh/defproperty eventually-hour-10
  [{::keys [hour]}]
  (rh/eventually
   (= hour 10)))

;; As you can see above, to define temporal properties, we use
;; ◊code{rh/defproperty}, it's similar to ◊code{rh/definvariant}, but
;; it allows you the usage of macros inside its body as some of
;; the operators we use for liveness are impossible to describe using Clojure
;; only.

;; ◊note{TLC is the backend for Recife, here we use these terms interchangeably.}

;; ◊code{rh/eventually} is one of these macros. In compilation
;; time, it checks for any Clojure code, converts them to a function (which
;; can access ◊code{db}) and transpiles the call to code
;; that TLC can use for assertions. Don't worry, most of it is still
;; normal Clojure, e.g. ◊code{(= hour 10)} is arbitrary code, no
;; limitation here.

;; Given that, this should work, right?

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::eventually-hour-10}
(rc/run-model global #{tick no-hour-after-23 eventually-hour-10})

;; Hunnn, funny... it seems that the spec is allowed to halt (or more
;; formally, to stutter), note that it has crashed just before
;; reaching hour 10. This happens because TLC does not like to make
;; assumptions on its own, you didn't say anything about ◊em{not
;; crashing}, so it checks this scenario as well. This trace
;; never reaches hour 10, so it's never ◊em{eventually} 10, violating
;; the temporal property.

;; To fix this, we have to make our processes a little bit fairer.

;; ◊title{Fairness}

;; To fix the stuttering problem (which you only have to worry about
;; when you start to deal with liveness properties), you have to
;; explictly tell Recife that you want a given action to ◊em{not} halt (or
;; not to crash). For this, you can make an action fair by attaching
;; the ◊code{^:fair} metadata to it.

(r/defproc ^:fair tick-fair
  ;; Below is the same as `tick`.
  (fn [{::keys [hour] :as db}]
    (if (= hour 23)
      (assoc db ::hour 0)
      (update db ::hour inc))))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::tick-fair}
(rc/run-model global #{tick-fair no-hour-after-23 eventually-hour-10})

;; Wowww, how fun, right?

;; Be careful to make actions fair only when you know that the real
;; system that the spec represents can guarantee such
;; fairness. E.g. if a message delivery system is ◊em{at-most-once},
;; but it does not say anything about being ◊em{at-least-once}, then
;; it's better not to make the spec fair at the part that delivers
;; messages (as the messages can be dropped), otherwise you will be assuming
;; something that does not hold in practice.

;; ◊note{Check
;; ◊link{https://learntla.com/core/temporal-logic.html#anything-can-crash}{Hillel's
;; Anything can crash section}.}

;; There are some nuances around fairness, e.g. weak vs strong
;; fairness distinction or that you are able to make some part of the
;; action fair, but we are not going into it right now, check the note
;; if you want to go deeper.

;; ◊title{◊em{Eventually} by itself is not enough}

;; Let's say that we have a tick action that goes from 23 to 11.

(r/defproc ^:fair tick-bad
  (fn [{::keys [hour] :as db}]
    (if (= hour 23)
      (assoc db ::hour 11)
      (update db ::hour inc))))

;; ◊code{eventually-hour-10} would still pass successfully as we have
;; hour 10 happening once. But how do we guarantee that the spec will
;; always be able to reach hour 10, no matter how many clock rounds we
;; have? How do we catch ◊code{tick-bad}?

;; We want something stronger than ◊em{eventually}, we want our spec
;; to reach hour 10 repeatedly, always eventually reaching hour 10.

;; Luckily, there is ◊code{rh/always}, with it you can describe
;; what you would like to validate for the entire trace (even if it's an
;; infinite trace, which it's the case for our clock). You can combine
;; it with ◊code{rh/eventually} to check that something repeatedly
;; happens at some point in time (you are ◊em{not} saying that it should
;; happen for all the states, only that it will continuosly happen
;; in some state).

(rh/defproperty repeatedly-often-hour-10
  [{::keys [hour]}]
  (rh/always
   (rh/eventually
    (= hour 10))))

;; Note see that we are wrapping the same body we had in
;; ◊code{eventually-hour-10} with ◊code{rh/always}. This will check that
;; hour 10 will happen infinitely often (it does not matter how many
;; states it takes, only that it reaches hour 10 over and over).

;; Let's take a look on our bad tick first.

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::repeatedly-often-hour-10-bad}
(rc/run-model global #{tick-bad no-hour-after-23 repeatedly-often-hour-10})

;; A violation happens in which hour 10 is not repeatedly reached as
;; the trace will be stuck in the ◊code{11 -> ... -> 23 -> ... -> 11
;; -> ... -> 23 -> ...} loop. It can be a little bit too much at
;; first, but take your time to understand it.

;; Finally, when using the good tick (◊code{tick-fair}), we should have no violations =D

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::repeatedly-often-hour-10-good}
(rc/run-model global #{tick-fair no-hour-after-23 repeatedly-often-hour-10})

;; ◊title{Eventually always?}

;; If there is ◊code{always eventually}, would we also have
;; ◊code{eventually always}? If so, do they have the same meaning?

;; Yes, it exists. No, they mean different things. While ◊code{always
;; eventually} means that something happens repeatedly often,
;; ◊code{eventually always} means that some state will eventually
;; happen and that the spec will stay in the state forever on.

(rh/defproperty eventually-always-hour-10
  [{::keys [hour]}]
  (rh/eventually
   (rh/always
    (= hour 10))))

;; A valid trace for the temporal property above would be ◊code{0 -> 1 -> ... -> -> ... -> 10
;; -> 10 -> 10 -> 10 ...}, but we know that this isn't valid according
;; to ◊code{tick-fair}.

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::repeatedly-often-hour-10-good}
(rc/run-model global #{tick-fair no-hour-after-23 repeatedly-often-hour-10 eventually-always-hour-10})

;; ◊title{Wrapping things up}

;; Well, this is a lot food for thought, I'm sure, let's stop
;; here. Hope you were able to feel what Recife/TLC is about, but
;; there is much ahead, stay tuned.

;; We will start using other examples in our next articles, a clock
;; is useful because is simple enough to understand, but there is
;; so much we can do with it. Also, I don't want to see a clock
;; on my front anymore.

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment

  @(r/run-model global #{tick-bad no-hour-after-23 eventually-hour-10}
                {:trace-example true})

  (r/get-result)

  (r/read-saved-data ::r/debug)

  ;; TODO:
  ;; - [x] Action property
  ;; - [x] Temporal property
  ;; - [ ] Bonus article using defchecker
  ;; - [ ] Two processes try to alter the clock
  ;; - [x] Eventually, clock arrives at some time
  ;; - [x] Infinitely often, clock arrives at some time
  ;; - [ ] In another notebook, explain the trace output
  ;; - [ ] Be able to show docstring for a var
  ;; - [ ] Add the option of the user seeing the raw data in table format
  ;; - [ ] Show how many seconds ago the run was if the user clicks on a
  ;;       `i` icon (also number of states and related info)
  ;; - [ ] We could show random examples in Clerk if
  ;;      `:trace-example` is activated. This should be feasible by
  ;;      using `r/save!`
  ;;   - Even for incomplete traces, it may be useful to show
  ;;     something in the screen (for fast feedback)

  ;; TODO (Tools):
  ;; - [x] Fix code style (now everything is blank and there is no mb)
  ;; - [x] Fix code viewer margin
  ;; - [x] Fix render-js for the trace
  ;; - [x] Fix TLA+ evaluation (it's not running)
  ;; - [x] Fix font for lists
  ;; - [-] Generate PDF version of each article
  ;;   - [x] Update Clerk to the latest version
  ;;   - Not now, it's ugly
  ;; - [ ] Cache locally so JVM restarts do not disrupt our workflow

  ;; Next we will ditch our clock and use other examples.

  (into (sorted-map) (System/getProperties))

  (System/getProperty "java.home")

  ())
