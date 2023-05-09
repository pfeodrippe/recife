;; ◊page-name[{:subtitle ""}]{logic and provenance}

;; We are going to see here Recife being used for ◊em{logic programming}
;; and for ◊em{provenance}. The first is a (poor) emulation of the core.logic
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
                 ;; max of 19 for each type. `r/one-of` says to Recife
                 ;; that we would like the engine to test each one of
                 ;; the possibilities as the initial state.
                 (mapv (fn [value]
                         [value (r/one-of (range 20))]))
                 (into {}))})

;; Then we create a checker for the amount. ◊code{rh/defchecker} is
;; similar to ◊code{rh/definvariant}, but instead of checking that
;; every value should ◊em{not} be this value, you are saying that you
;; would like to find an example that satisfies this check (basically,
;; one behaves as the complement of the other), and, if that's the case
;; a violation will occur (violating the ◊em{non}-occuring, which is what
;; we want).
(rh/defchecker check-amount
  [{::keys [desired-amount denoms]}]
  (= desired-amount
     ;; We multiply each coint amount by the quantity of this coin
     ;; and reduce it to find the amount.
     (reduce (fn [acc [coin-value quantity]]
               (+ acc (* coin-value quantity)))
             0
             denoms)))

;; Let's now run the model. Note below that we are using
;; the flag ◊code{:continue}, this means that the engine won't stop
;; in the first violation found (and satisfying the checker is a violation for
;; the engine!), it will find as many violations as there are (the downsde of
;; using this mode is that temporal properties are not checked, but we are
;; not checking them anyway in this example).
^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::core-logic
  ::rc/block true}
(rc/run-model den-global #{check-amount} {:continue true})

;; In the result, we have a key that we haven't see before,
;; ◊code{:violations}, this key only appears in the continue mode when
;; there are violations.  The "violations", in our case, are the
;; traces that satisfy the checker, which is asking which coins
;; quantities sum to 14 cents. Each violation only has the initial
;; state as there is no action to be acted on, but we will see later
;; in the Provenance section how we can use this for more complex
;; logic.

;; And yes, these are exactly the same as the ones found by the
;; core.logic example.
^{:nextjournal.clerk/visibility {:result :show :code :show}}
(= (->> (r/get-result)
        :violations
        (mapv (comp ::denoms last first :trace))
        set)
   (set change-result))

;; Recife wasn't created for this and it will be slower than
;; core.logic, but at least you don't need to learn an entirely different
;; language, you can use the normal Clojure you already know.

;; ◊title{Provenance}

;; For our purposes, provenance will mean discovering how something reached
;; at some state. Given that I have these actions, is it possible to reach
;; this or that state?

;; We will use an example derived from a system I had the opportunity
;; to work with. In the system, we can create employers, parent
;; employers, upload and insert invoices (these are two separate
;; actions). Let's do it.

;; ◊code{::id} is used as an counter, generating unique ids for the
;; employers and invoices.
(def global
  {::id 0})

(def available-employers
  "Helper function to expose the available employers."
  (comp keys ::employers))

(r/defproc add-employer
  (fn [{::keys [id] :as db}]
    (-> db
        (update ::id inc)
        ;; Create a new employer with a unique ID.
        ;; Note that `::employers` does not exist in `global`,
        ;; meaning that we can create (or remove) state as much
        ;; as we need.
        (update ::employers assoc
                id {:id id}))))

;; An employer can have an parent, but not a grandparent.
(r/defproc add-parent-employer
  {[::set-parent-employer
    ;; Using a vector as key for the proc map means that we will
    ;; want things to be non-deterministic (with all paths tested
    ;; by Recife). E.g. we will randomly choose one parent and one
    ;; child, and they will be available as the `:parent` and `:child`
    ;; keys respectively in the associated handler.
    {:parent available-employers
     :child available-employers}]
   (fn [{::keys [employers] :keys [parent child] :as db}]
     ;; If any of the conditions are not met, then do nothing.
     (when (and (not= parent child)
                ;; No grandparents.
                (not (:parent (get employers child)))
                (not (:parent (get employers parent)))
                ;; Also we cannot have recursive parents.
                (not (contains? (set (mapv :parent (vals employers)))
                                child)))
       (-> db
           (update-in [::employers child] assoc
                      :parent parent))))})

;; We can also upload an invoice as well as adding it to the system.
;; For context, they are different actions as the first one is uploaded
;; manually and the second one is inserted into our DB, but we care
;; about it in a high-level fashion, we don't really model these details
;; if we don't need.
(r/defproc invoice {:local {:pc ::admin-uploads-invoice}}
  {[::admin-uploads-invoice
    {:employer available-employers}]
   (fn [{::keys [id] :keys [employer] :as db}]
     (when employer
       (-> db
           ;; As `:choosen-employer` is an unamespaced keyword,
           ;; it will be a local variable for the process, only
           ;; available for `invoice`.
           (assoc :choosen-employer employer)
           (update ::id inc)
           (update-in [::employers employer :invoices-uploaded]
                      conj id)
           ;; `r/goto` sends the proc to another step.
           (r/goto ::system-inserts-invoice))))

   ::system-inserts-invoice
   (fn [{::keys [employers] :keys [choosen-employer] :as db}]
     (-> db
         (update-in [::employers choosen-employer :invoices]
                    concat
                    (get-in employers [choosen-employer :invoices-uploaded]))
         (assoc-in [::employers choosen-employer :invoices-uploaded] [])
         ;; Back to the initial step.
         (r/goto ::admin-uploads-invoice)))})

;; In common with the logic programming section, we want to know how
;; we are able to have an employer with at least one invoice. We
;; should have the entire trace available as a violation, learning how
;; our system behaves.
(rh/defchecker check
  [{::keys [employers]}]
  (some (comp seq :invoices)
        (vals employers)))

;; Add some invariants so we can check that the system is working
;; correctly.
(rh/definvariant no-grandparent
  [{::keys [employers]}]
  (not (some #(-> (:parent %) employers :parent)
             (vals employers))))

(rh/definvariant no-cyclical-parent
  [{::keys [employers]}]
  (not (some #(= (-> (:parent %) employers :parent)
                 (:id %))
             (vals employers))))

;; Also a constraint so we don't add infinite employers, it would be
;; useless.
(rh/defconstraint constraint
  [{::keys [employers]}]
  (<= (count employers) 3))

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::provenance
  ::rc/block true}
(rc/run-model global #{add-employer add-parent-employer invoice
                       check
                       no-grandparent no-cyclical-parent
                       constraint})

;; Nice! We have been able to find an example of what
;; we wanted, see the table below for more details.

^{:nextjournal.clerk/visibility {:result :show :code :hide}}
(clerk/table
 (->> (r/get-result)
      :trace
      (mapv last)
      (mapv #(select-keys % [::id ::employers :recife/metadata]))
      (mapv #(-> %
                 (update ::employers clerk/code)
                 (update :recife/metadata clerk/code)))))

;; You can see the behavior that leaded to each step along with the values,
;; so you can debug not only static data like we did in our logic programming
;; section, but also dynamic actions, being able to debug or learn about how
;; some good thing or bad thing happened.

;; That's it for today, see ya o/
