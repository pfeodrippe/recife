;; ◊page-name[{:subtitle ""}]{logic and provenance}

;; We are going to see here Recife being used for ◊em{logic programming}
;; and for ◊em{provenance}. The first is a poor emulation of the core.logic
;; capabilities, the second one consists in searching for a possible state.

(ns recife.notebook.logic-and-provenance
  {:nextjournal.clerk/visibility {:result :hide}
   :clerk/name "logic and provenance"}
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [nextjournal.clerk :as clerk]
   [recife.clerk :as rc]
   [recife.core :as r]
   [recife.helpers :as rh]))

;; ◊title{Emulating core.logic}

;; Check the ◊link{https://blog.taylorwood.io/2018/05/10/clojure-logic.html}{core.logic article}
;; from Taylor Wood, it's a great introduction to the subject and library!

;; We will be using Taylor's Denomination Sums example, read the article,
;; but it basically consists in finding how many ways you can have an amount
;; given that you have, let's say, coins of 1, 2, 5 and 10 cents.

;; Taylor defines two functions using some core.logic namespaces (◊code{l} and ◊code{fd}
;; aliases) containing the logic for finding the sums, I won't explain them
;; here as he does it pretty well.
(defn productsumo
  "From https://blog.taylorwood.io/2018/05/10/clojure-logic.html."
  [vars dens sum]
  (l/fresh [vhead vtail dhead dtail product run-sum]
    (l/conde
     [(l/emptyo vars) (l/== sum 0)]
     [(l/conso vhead vtail vars)
      (l/conso dhead dtail dens)
      (fd/* vhead dhead product)
      (fd/+ product run-sum sum)
      (productsumo vtail dtail run-sum)])))

(defn change
  "From https://blog.taylorwood.io/2018/05/10/clojure-logic.html."
  [amount denoms]
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

;; Then we have the possible values when calling ◊code{change}.
(def change-result
  (->> (change 14 #{1 2 5 10})
       (mapv #(into (sorted-map) %))))

^{:nextjournal.clerk/visibility {:result :show :code :hide}}
(clerk/table change-result)

;; So far, so good, are we able to do something similar with Recife?

;; Yes, let's first start with our global state. We will define the desired
;; amount (◊code{14}) and the available coins (◊code{#{1 2 5 10}}).
^{:nextjournal.clerk/visibility {:result :show :code :show}}
(def den-global
  {::desired-amount 14
   ::denoms (->> #{1 2 5 10}
                 ;; We will limit the number of coins, min of 0 and
                 ;; max of 19 for each one.  ◊code{r/one-of} says to
                 ;; Recife that we would like that the engine test
                 ;; each of these possibilities as the initial state.
                 (mapv (fn [value]
                         [value (r/one-of (range 20))]))
                 (into {}))})

;; Then we create a checker for the amount. ◊code{rh/defchecker} is
;; similar to ◊code{rh/definvariant}, but instead of checking that
;; every value should ◊em{not} be this value, you are saying that you
;; would like to find an example that satisfies this check (basically,
;; one behaves as the complement of the other).
(rh/defchecker check-amount
  [{::keys [desired-amount denoms]}]
  (= desired-amount
     ;; We multiply each coint amount by the quantity of this coin
     ;; and reduce it to find the amount.
     (reduce (fn [acc [coin-value quantity]]
               (+ acc (* coin-value quantity)))
             0
             denoms)))

;; And that should be it, let's now run the model. Note that we are using
;; the flag ◊code{:continue} here, this means that the engine won't stop
;; in the first violation found (and satisfying the checker is a violation for
;; the engine!), it will find as many violations as there are (the downsde of
;; using this mode is that temporal properties are not checked, but we are
;; not checking them anyway in this example).
^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::core-logic
  ::rc/block true}
(rc/run-model den-global #{check-amount} {:continue true})

;; We have a key that we haven't see before, ◊code{:violations}, this
;; key only appears in the continue mode when there are violations.
;; The "violations", in our case, are the traces that satisfy the checker,
;; which is asking which coins sums amount to 14 cents. Each violation only
;; has the initial state as there is no action to be acted on, but we will
;; see later in the Provenance section how we can use this for more complex
;; logic.

;; And yes, these are exactly the same as the one found by the core.logic
;; example.
^{:nextjournal.clerk/visibility {:result :show :code :show}}
(= (->> (r/get-result)
        :violations
        (mapv (comp ::denoms last first :trace))
        set)
   (set change-result))

;; Of course, Recife wasn't created for this and it will be slower than
;; core.logic, but at least you don't need to learn an entirely different
;; language, you can use the normal Clojure you already know.

;; ◊title{Provenance}

;; For our purposes, provenance will mean discovering how something reached
;; at some state. Given that I have these actions, is it possible to reach
;; this or that state?

;;

(comment

  @(r/run-model den-global #{check-amount} {:continue true
                                            #_ #_:run-local true})

  (r/get-result)

  ())
