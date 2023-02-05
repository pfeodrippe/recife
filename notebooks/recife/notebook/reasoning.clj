^{:nextjournal.clerk/visibility {:code :hide}}
(ns recife.notebook.reasoning
  {:clerk/name "reasoning"})

;; ◊page-name{🤔 reasoning}

;; Well, in 2020, I've started thinkering with a language called
;; ◊link{https://lamport.azurewebsites.net/tla/tla.html}{TLA+},
;; according to their website, it's a "high-level language for
;; modeling programs and systems". Bold words, right?

;; But it's really different. Not in the Clojure way, it's not the
;; same kind of disruption that we felt when we came from languages
;; like Java, Ruby or Python. This time the gap is even bigger, TLA+
;; is not a language that you use to build systems, but it's a
;; language you use for thinking about these systems, it's a
;; specification. You will find concepts like non-determinism or
;; temporal porperty traces that we don't usually need to deal with in
;; our day-to-day jobs.

;; While TLA+ is the language, there is also TLC, which is the system
;; that makes TLA+ waste cycles in our computers. And, fortunately,
;; TLC is a Java program (would it work with the REPL?)! I've started
;; to become more interested on hacking it than doing proper
;; specifications with the language itself, I have even done some
;; minor contributions to the
;; ◊link{https://github.com/tlaplus/tlaplus}{TLA+ project}.

;; There is more to it, but to not take much time here, after making
;; very ugly things to make TLC work with Clojure (in summary, we
;; can't run a spec in the same JVM because TLC has lots of static
;; states, so I had to workaround it by making Recife transparently
;; start a new JVM when you execute ◊code{r/run-model}), Recife was
;; born.

;; The main purpose in building Recife is to put this kind of tooling
;; in more hands, but, instead of having to learn TLA+, people can now
;; use the old Clojure that they know (it's not transpiled
;; (◊em{mostly}), it's really CLJ in the JVM!). I don't want the fun
;; to be all mine, time to share it.

;; ----------------------------------------------------------------

;; I recommend you check the work of
;; ◊link{https://www.hillelwayne.com}{Hillel Wayne}, he was the one
;; that introduced me (and others) to this world.
