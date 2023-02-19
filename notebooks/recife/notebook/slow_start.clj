;; ◊page-name[{:subtitle "Learn on your own time"}]{slow start}

;; Hi, I will be starting a series of articles about Recife, this is the
;; first one. Check the ◊xref{:doc/reasoning} for this project.

;; In this non-quick start, we will be dealing with a simple clock,
;; this should cover some features that you will find in Recife. Other
;; notebooks from this guide will go deeper into these and other
;; Recife/Temporal Logic (TLA+) concepts.

;; ◊title{Setting things up}

;; ◊note{Get Recife on
;; ◊link{https://clojars.org/pfeodrippe/recife}{clojars}.}

(ns recife.notebook.slow-start
  {:nextjournal.clerk/visibility {:result :hide}
   :clerk/name "slow start"}
  (:require
   [recife.clerk :as rc]
   [recife.core :as r]
   [recife.helpers :as rh]))

;; We want to define a specification, and a specification needs some
;; state.
(def global
  {::hour 0})

;; ◊code{global} represents the ◊em{initial} state of the
;; specification.

;; ◊note{Receive a state, return a (possibly modified) state.}
(defn update-clock-1
  [db]
  (update db ::hour inc))

;; Leveraging the simplicty of Clojure, the entire state we have to
;; deal with is a map, we modify the specification state by
;; returning another map.

;; We are able to use ◊code{update-clock-v1} in the
;; first Recife operator we will meet, ◊code{defproc}.
(r/defproc tick-v1 update-clock-1)

;; ◊note{A trace is a sequence of states, a trace can also be called a
;; behavior.}

;; ◊code{defproc} defines a process that will be used in Recife's
;; runtime.

;; ◊code{tick-v1} can be invoked like a Clojure function and it should do
;; what we expect.
^{:nextjournal.clerk/visibility {:result :show :code :hide}}
(rc/example
 (tick-v1 global)
 (-> (tick-v1 global)
     tick-v1
     tick-v1))

;; We can also inline a anonymous function if we want, there is nothing special
;; about it.
(r/defproc tick-v1
  (fn [db]
    (update db ::hour inc)))

;; ◊title{Running the specification}

;; For running a Recife specification, we use ◊code{r/run-model}, as
;; arguments, it receives the initial state, the components and some
;; optional args.

(comment

  ;; This will return an asynchronous process that you can `@` (deref).
  (r/run-model global #{tick-v1} {:trace-example true})

  ;; Halt will stop the process AND deref. You will need to use it, otherwise
  ;; the clock will just keep running and running (it's unbounded!).
  (r/halt!)

  ;; You can also get the last result using this.
  (r/get-result)

  ())

;; We don't want just to present static code like this, the main
;; motivation for this guide is to make Recife (and TLA+) concepts
;; easier to digest, so a custom trace visualizer was built for it
;; that we can use here. You will be able to follow the words without
;; having to start a REPL on your own (although it would be beneficial
;; if you do so, this article is a
;; ◊link{https://github.com/pfeodrippe/recife/blob/master/notebooks/recife/notebook/slow_start.clj}{Clerk
;; notebook}, which means that you can run it, modify it, break
;; it... it's just a Clojure namespace).

;; Let's see our clock in action.

;; ◊note{◊code{:trace-example} means that the output of `r/run-model` will
;; return a trace example if no violation is found.}

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::infinite}
(rc/run-model global #{tick-v1})

;; ◊note{The trace visualizer is bounded.}

;; ◊code{::hour} is always incremented by 1 (which it's expected),
;; but we have lots of steps (really, infinite) because there is
;; nothing saying when we should stop, we will solve this in the next
;; section.

;; ◊note{If you are in a mobile device, the trace is uglier and hover
;; doesn't work, check this article in a computer if possible.}

;; The first thing to notice is the trace format,
;; the step label consists of the step index followed by the ◊em{operator}
;; that led to that step. A proc can contain multiple
;; operators, and, in this case, we have a proc with only one operator
;; (◊code{tick-v1} is also the name of the operator).

;; If you are in a computer, you are able to hover your cursor on
;; each step and check the corresponding state.

;; ◊title{Constraining the possible states}

;; ◊note{◊code{rh/defconstraint} does ◊em{not} receive a function,
;; it's more akin to ◊code{defn}.}

;; For defining a constraint, we use ◊code{rh/defconstraint}, it
;; receives ◊code{db}, but, this time, we are not interested in
;; modifying the state, we now return a boolean and, for every state
;; found, Recife will check if it satisfies the constraint (truthy
;; value), if it doesn't, then the state is thrown out.

;; We will say that we don't care about any hours past 25.
(rh/defconstraint disallow-after-25
  [{::keys [hour]}]
  (<= hour 25))

;; Then we add the constraint into our model run.

(comment

  ;; Remember, you can also deref the response if you want.
  ;; The run should finish now and we should be shown an `:ok`,
  ;; meaning that no violation was found (as we are not verifying
  ;; anything yet!).
  (r/run-model global #{tick-v1 disallow-after-25})

  ;; You can always use `:trace-example` to have a random trace
  ;; example if there are no violations (as it's the case here).
  @(r/run-model global #{tick-v1 disallow-after-25}
                {:trace-example true})

  ())

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::disallowed}
(rc/run-model global #{tick-v1 disallow-after-25})

;; But we still have something weird here as hour 25 does not make
;; much sense (in our clock), how can we make the specification tell
;; us about that?

;; ◊title{Checking the spec}

;; While in constraining we are able to say which states we don't
;; care about, in validation we want to say what's a good state looks
;; like and be warned if something is bad.

;; ◊note{A violation happens when some state is invalid.}

;; The simplest way of doing this in Recife is writing an invariant
;; with ◊code{rh/definvariant}. Just like ◊code{rh/defconstraint},
;; this macro receives a state and you should return a boolean (or
;; truthy/falsy value) indicating if there is no ◊em{violation}.

(rh/definvariant no-hour-after-23
  [{::keys [hour]}]
  (<= hour 23))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::first-invariant}
(rc/run-model global #{tick-v1 disallow-after-25 no-hour-after-23})

;;  🥳 Our first spec violation!!

;; I'm pretty sure you have some ideas on how to fix that.

;; ◊title{Fixing the spec violation}

;; Let's define a new proc with a fix that will replace the previous
;; one.

;; ◊note{Note that ◊code{rh/definvariant} and ◊code{rh/defconstraint}
;; receive a ◊code{defn}-like body, while ◊code{r/defproc} receives a
;; function, we will see the reason for this in the future.}
(r/defproc tick-v2
  (fn [{::keys [hour] :as db}]
    ;; Now we can return `nil`, what happens in this case?
    ;; When we return `nil` in a proc operator, we are saying to the
    ;; Recife runtime that the state won't change, it will stay the
    ;; same, it's equivalent to just return `db` itself, but it helps
    ;; to create guards like the below one.
    ;; Basically, this is `defproc`'s nil punning.
    (when (<= hour 22)
      (update db ::hour inc))))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::tick-v2}
(rc/run-model global #{tick-v2 disallow-after-25 no-hour-after-23})

;; Hunn... this is unexpected. We are having another type of violation
;; here, a ◊em{deadlock}. This means that there is no way to advance
;; to a new state in this trace and that there are remaining proc that
;; are not marked as complete (see the error message above to see how
;; to fix it).

;; ◊note{Only disable deadlock if it's really needed.}

;; For this simple clock example, we don't care about deadlocks, so
;; let's switch it off. Also let's get rid of the constraint as we
;; don't need it anymore (as it's not even possible to reach more
;; than 23 hours).
^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::tick-v2}
(rc/run-model global #{tick-v2 no-hour-after-23}
              {:no-deadlock true})

;; But I'm not happy with that, nor you should be, let's create a new
;; tick proc that does the right thing (23h -> 0h). This also allow us
;; to remove ◊code{:no-deadlock} as we don't stop anymore, we just go
;; round and round.

(r/defproc tick-v3
  (fn [{::keys [hour] :as db}]
    (if (= hour 23)
      (assoc db ::hour 0)
      (update db ::hour inc))))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::tick-v3}
(rc/run-model global #{tick-v3 no-hour-after-23})

;; Awesome, we can stop here, it's a lot already (and there tons of
;; things and nuances to discuss about). Thanks for reading this =D

;; ◊title{Wrapping up}

;; In our next articles, we will see how to deal with things a little
;; bit more complicated than invariants, but which will enables us to
;; do different kinds of checks (e.g. how do we check that the clock
;; will ◊em{always eventually} point to 3 o'clock?). For now, I
;; recommend you to read some articles from Hillel (he uses TLA+
;; though, but he explains some of the concepts pretty well):
;; ◊numbered-list{
;;   ◊link{https://www.hillelwayne.com/post/action-properties/}{Action
;; properties}
;;
;;   ◊link{https://learntla.com/core/temporal-logic.html}{Temporal
;; properties}
;;
;;   ◊link{https://www.hillelwayne.com/post/fairness/}{Fairness}
;; }

;; -------------------------------------------

;; Check the raw article
;; ◊link{https://github.com/pfeodrippe/recife/blob/master/notebooks/recife/notebook/slow_start.clj}{here}.

;; Discuss or comment
;; ◊link{https://github.com/pfeodrippe/recife/discussions/18}{here}.

;;See you next, bye \o

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment

  ;; TODO:
  ;; - [x] Return trace example when halting
  ;; - [x] Find a way to require Clerk without interfering with the
  ;;       model run start time
  ;; - [ ] Visualize trace in Clerk
  ;;   - [x] Create `recife.clerk`
  ;;   - [x] `r/run-model` should act differently when running from Clerk
  ;;     - Wrapping it using `rc/example` should do what we expect
  ;;   - [x] Check the visualization
  ;;     - [x] Create viewer for Recife
  ;;   - [x] Show node info
  ;;   - [x] Chek why ch6-threads-3 is not working
  ;;     - There was a race condition where the output streaming was
  ;;       ending after the response was returned
  ;;   - [x] Make loading async
  ;;     - [x] Initial
  ;;     - [x] Running
  ;;     - [x] Final result
  ;;   - [x] Make queue in the background so specs can run from the
  ;;         top to the bottom
  ;;   - [x] Fix no trace violations for ch6-threads-3
  ;;   - [x] Fix showing only `blocked` and `running`, but not `waiting`
  ;;   - [x] Check async with build
  ;;   - [x] Show trace info
  ;;     - [x] Example
  ;;     - [x] Create custom viewer for examples
  ;;     - [x] Invariant
  ;;     - [x] Back to state
  ;;     - [x] Add loopback on the graph
  ;;     - [x] Violated temporal property
  ;;     - [x] Stuttering
  ;;   - [x] Fix intermediary state presentation (waiting/running)
  ;; - [x] Clock runs forever
  ;;   - [x] How to visualize this in Clerk?
  ;;   - [x] Add constraint
  ;;     - Will be removed later
  ;; - [x] Fix mobile rendering
  ;; - [x] Invariant for checking that hour does not pass of 23
  ;; - [ ] Present deadlock
  ;; - [ ] Fix spec
  ;; - [ ] defchecker
  ;; - [ ] Two processes try to alter the clock
  ;; - [ ] Eventually, clock arrives at some time
  ;; - [ ] Infinitely often, clock arrives at some time
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

  ;; TODO:
  ;; - [x] Fix code identation on mobile
  ;; - [x] Fix notes on mobile
  ;; - [x] Render without bundling
  ;; - [-] Diminish font size
  ;;   - No, we are fine with it
  ;; - [ ] Fix font for unbundled build
  ;; - [ ] Can we add grammar check?

  ())
