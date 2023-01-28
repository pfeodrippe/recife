(ns recife.example.provenance
  (:require
   [clojure.set :as set]
   [recife.core :as r]
   [recife.helpers :as rh]
   [clojure.core.logic :as l]))

#_(l/run* [q]
    (l/fresh [a]
      (l/membero a [1 4 3])
      (l/membero a [4 5 3])
      (l/membero a [7 8 q])))
;; => (4 3)

(def global
  {})

(r/defproc try-read
  (fn [{:keys [actor] :as db}]))

;; TODO:
;; - [ ] Can we find multiple examples using TLC?
;;   - [x] Save violations
;;   - [x] Action property
;;   - [x] Invariant
;;   - [ ] Temporal property
;;     - [ ] Check property violation using Clojure so we can find the correct
;;           `defproperty`
;;       - [ ] Compile rh functions to clojure when requested
;;       - [ ] Accumulate state so we can show exactly where the error is located
;;             inside the trace
;;       - [ ] Check traces
;;       - [ ] Use an action property for it, see
;;             https://github.com/tlaplus/tlaplus/issues/786#issuecomment-1407496531
;;     - [ ] Do we have a way to know if a trace is "done" while
;;           running the spec?
;; - [ ] Maybe can use core.logic for the traces?

;; Instead of looking for violations, you may be
;; interested on the opposite, a trace example that satisfied some
;; arbitrary condition.
