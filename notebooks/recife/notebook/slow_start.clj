;; ◊page-name[{:subtitle "Gimme code"}]{🏃🏽‍♀️slow start}

;; If you didn't already, see the ◊xref{:doc/reasoning} for this
;; library.

;; For this non-quick start, we will be dealing with a simple clock, this should be
;; a small sample of the functionality that you will find in Recife. Other notebooks
;; in this guide will go further into the tooling and and concepts you
;; find here.

;; ◊title{Setting things up}

;; ◊note{Get Recife on
;; ◊link{https://clojars.org/pfeodrippe/recife}{clojars}}

;; First, require some namespaces that we will use in our code.
(ns recife.notebook.slow-start
  {:nextjournal.clerk/visibility {:result :hide}
   :clerk/name "slow start"
   #_ #_:nextjournal.clerk/no-cache true}
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.clerk :as rc]))

;; ◊note{We are using Clerk to render this notebook, which
;; you can find
;; ◊link{https://github.com/pfeodrippe/recife/blob/master/notebooks/recife/notebook/slow_start.clj}{
;; here}}

;; We will define a specification, and we need some data to act
;; upon. Following Clojure conventions, everything we will need to deal
;; with is defined in a simple map.

(def global
  {::hour 0})

;; ◊code{global} contains only the ◊code{::hour} ◊em{variable}, this represents
;; a state that we ◊em{can} change and that Recife knows how to
;; deal with.

;; All keywords of this map should be namespaced so we can
;; differentiate it from local variables in our processes, you will
;; understand better why this is needed when we start to work with
;; multiple processes.

;; We then need some way of changing this state, a Clojure function
;; should do it for us. State is changed in a immutable way.

(defn update-clock-1
  [db]
  (update db ::hour inc))

;; This function receives a map and returns a map, ◊code{db} contains
;; the key ◊code{::hour}. We can give this function to the first (and
;; most important) Recife operator we will know, ◊code{defproc}.

(r/defproc tick-v1 update-clock-1)

;; We can also inline the function, it's more convenient to have everything
;; in one place.
(r/defproc tick-v1
  (fn [db]
    (update db ::hour inc)))

;; ◊note{A trace is a sequence of states, a trace can also be called a
;; behavior}

;; ◊code{defproc} defines a process that will be used in Recife's
;; runtime. This is the only way that we can use to change state, the
;; state changes will build a trace that Recife can analyze.

;; ◊code{tick-v1} can be invoked like a Clojure function and it should do
;; what we expect.
^{:nextjournal.clerk/visibility {:result :show :code :hide}}
(rc/example
 (tick-v1 global)
 (-> (tick-v1 global)
     tick-v1
     tick-v1))

;; ◊title{Running our first specification}

#_(comment

  ;; This will return an asynchronous process that you can `@` (deref).
  (r/run-model global #{tick-v1} {:trace-example true})

  ;; Halt will stop the process AND deref. You will need it, otherwise
  ;; the clock will just keep running and running.
  (r/halt!)

  ;; You can get the last result using this
  (r/get-result)

  ())

;; ◊title{Constraining}

;; ◊note{We will handle it better later, but for now let's just
;; constraint the states}

;; Let's define a constraint. ◊code{defconstraint} receives a map, but
;; this time we are not interested in changing the state, but we would
;; like to constrain the states we want the model to care about. If
;; there are one or more constraints, and for every state found, Recife
;; will check if it satisfies the constraint, if not, then it's thrown
;; out.

;; In this case, we are saying that we don't care about hours after
;; 23.
(rh/defconstraint disallow-after-23
  [{::keys [hour]}]
  (<= hour 23))

#_^{:nextjournal.clerk/visibility {:result :show :code :hide}}
(rc/example
 @(r/run-model global #{tick-v1 disallow-after-23}
               {:trace-example true}))

(comment

  ;; TODO:
  ;; - [ ] See how to present async stuff
  ;;   - [ ] Maybe using an atom?
  ;;   - [ ] Show when it has started
  ;; - [ ] Work on visualizing this trace first as this one is finite

  ;; Remember, you can also deref the respoinse if you want.
  ;; The run should finish now and we should be shown an `:ok`,
  ;; meaning that no violation was found (as we are not verifying
  ;; anything yet!).
  @(r/run-model global #{tick-v1 disallow-after-23})

  ;; You can always use `:trace-example` to have a random trace
  ;; example if there are no violations (as it's the case here).
  @(r/run-model global #{tick-v1 disallow-after-23}
                {:trace-example true})

  ())

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment

  ;; TODO:
  ;; - [x] Return trace example when halting
  ;; - [x] Find a way to require Clerk without interfering with the
  ;;       model run start time
  ;; - [ ] Visualize trace in Clerk
  ;;   - [x] Create `recife.clerk`
  ;;   - [ ] `r/run-model` should act differently when running from Clerk
  ;;     - [ ] Maybe get things from cache?
  ;;   - [ ] We could show random examples in Clerk if
  ;;        `:trace-example` is activated. This should be feasible by
  ;;        using `r/save!`
  ;;     - Even for incomplete traces, it may be useful to show
  ;;       something in the screen (fast feedback)
  ;; - [ ] Clock runs forever
  ;;   - [ ] How to visualize this in Clerk?
  ;;   - [ ] Add constraint
  ;;     - Will be removed later
  ;; - [ ] Invariant for checking that hour does not pass from 23
  ;; - [ ] defchecker
  ;; - [ ] Two processes try to alter the clock
  ;; - [ ] Eventually, clock arrives at some time
  ;; - [ ] Infinitely often, clock arrives at some time
  ;; - [ ] In another notebook, explain the trace output
  ;; - [ ] Be able to show docstring for a var

  ;; TODO:
  ;; - [ ] Fix code identation on mobile
  ;; - [ ] Fix notes on mobile

  ())
