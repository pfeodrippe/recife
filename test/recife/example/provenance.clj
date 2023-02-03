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
  {::q (r/one-of (range 100))
   ::a (r/one-of (range 100))})

(rh/defchecker member-of
  [{::keys [a q]}]
  (and (contains? (set [1 4 3]) a)
       (contains? (set [4 5 3]) a)
       (contains? (set [7 8 q]) a)))

(comment

  (r/run-model global #{member-of} {#_ #_:debug? true
                                    #_ #_:workers 1
                                    :continue true})

  ;; When you run above, you can see that it found the same `a` and `q`
  ;; as found by core logic.
  (r/read-saved-data :recife/violation)

  ())

;; TODO:
;; - [ ] Can we find multiple examples using TLC?
;;   - [x] Save violations
;;   - [x] Action property
;;   - [x] Invariant
;;   - [ ] Temporal property
;;     - [x] Check property violation using Clojure so we can find the correct
;;           `defproperty`
;;       - [x] Compile rh functions to clojure when requested
;;       - [x] Check traces
;;       - [ ] Use an action property for it, see
;;             https://github.com/tlaplus/tlaplus/issues/786#issuecomment-1407496531
;;   - [x] Test that the output is the same as the core logic one
;; - [-] Maybe can use core.logic for the traces?

;; Instead of looking for violations, you may be
;; interested on the opposite, a trace example that satisfied some
;; arbitrary condition.
