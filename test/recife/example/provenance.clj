(ns recife.example.provenance
  (:require
   [clojure.set :as set]
   [recife.core :as r]
   [recife.helpers :as rh]
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]))

;; Simple member example.
(comment

  (l/run* [q]
    (l/fresh [a]
      (l/membero a [1 4 3])
      (l/membero a [4 5 3])
      (l/membero a [7 8 q])))
  ;; => (4 3)

  ())

(def member-global
  {::q (r/one-of (range 100))
   ::a (r/one-of (range 100))})

(rh/defchecker member-of
  [{::keys [a q]}]
  (and (contains? (set [1 4 3]) a)
       (contains? (set [4 5 3]) a)
       (contains? (set [7 8 q]) a)))

(comment

  (r/run-model member-global #{member-of} {#_ #_:debug? true
                                           #_ #_:workers 1
                                           :continue true})

  ;; When you run above, you can see that it has found the same `a` and `q`
  ;; as found by core logic.
  ;; Violations are stored at `:recife/violation` so you don't need to wait
  ;; until the end of the run, they are saved whilt the model is running.
  (r/read-saved-data :recife/violation)

  ())

;; Denomination sums from https://blog.taylorwood.io/2018/05/10/clojure-logic.html.
(comment

  (defn productsumo [vars dens sum]
    (l/fresh [vhead vtail dhead dtail product run-sum]
      (l/conde
       [(l/emptyo vars) (l/== sum 0)]
       [(l/conso vhead vtail vars)
        (l/conso dhead dtail dens)
        (fd/* vhead dhead product)
        (fd/+ product run-sum sum)
        (productsumo vtail dtail run-sum)])))

  (defn change [amount denoms]
    (let [dens (sort > denoms)
          vars (repeatedly (count dens) l/lvar)]
      (l/run* [q]
        ;; we want a map from denominations to their quantities
        (l/== q (zipmap dens vars))
        ;; prune problem space...
        ;; every var/quantity must be 0 <= n <= amount
        (l/everyg #(fd/in % (fd/interval 0 amount)) vars)
        ;; the real work
        (productsumo vars dens amount))))

  (change 14 #{1 2 5 10})

  ())

(def den-global
  {::amount 14
   ::denoms (->> #{1 2 5 10}
                 (mapv (fn [value]
                         [value (r/one-of (range 20))]))
                 (into {}))})

(rh/defchecker check-amount
  [{::keys [amount denoms]}]
  (= amount
     (->> denoms
          (reduce (fn [acc [value times]]
                    (+ acc (* value times)))
                  0))))

(comment

  (r/run-model den-global #{check-amount} {:continue true})

  (r/read-saved-data :recife/violation)

  ())

;; TODO:
;; - [x] Can we find multiple examples using TLC?
;;   - [x] Save violations
;;   - [x] Action property
;;   - [x] Invariant
;;   - [x] Check property violation using Clojure so we can find the correct
;;         `defproperty`
;;     - [x] Compile rh functions to clojure when requested
;;     - [x] Check traces
;;   - [-] Temporal property
;;     - [-] Use an action property for it, see
;;           https://github.com/tlaplus/tlaplus/issues/786#issuecomment-1407496531
;;   - [x] Test that the output is the same as the core logic one
;; - [-] Maybe can use core.logic for the traces?

;; Instead of looking for violations, you may be
;; interested on the opposite, a example that satisfies some
;; arbitrary condition.
