(ns recife.examples
  (:require
   [recife.core :as r]))

(comment
  ;; basics

  ;; procs  - set of identifiers
  ;; global - global variables
  ;; local  - local variables to ONE process
  (def inc*
    (r/with-proc {:procs  #{:p1}
                  :global {:my/number 0}
                  :local  {}}
      ;; labels can be anything, it's a arbitrary keyword
      :inc {:my/number (+ (:my/number global) 1)}))

  (r/run-procs [inc*])

  (r/timeline-diff (r/run-procs [inc*]))

  ())


(comment
  ;; interleaving for the same process type
  (def inc*
    (r/with-proc {:procs  #{:p1 :p2}
                  :global {:my/number 0}
                  :local  {}}
      :inc {:my/number (+ (:my/number global) 1)}))

  (r/run-procs [inc*])

  (r/timeline-diff (r/run-procs [inc*]))

  ())


(comment
  ;; interleaving between different process types
  (def inc*
    (r/with-proc {:procs  #{:p1}
                  :global {:my/number 0}
                  :local  {}}
      :inc {:my/number (+ (:my/number global) 1)}))

  (def dec*
    (r/with-proc {:procs  #{:t1}
                  :global {:my/number 0}
                  :local  {}}
      :dec {:my/number (- (:my/number global) 1)}))

  (r/run-procs [inc* dec*])

  (r/timeline-diff (r/run-procs [inc* dec*]))

  ())


(comment
  ;; multiple interleavings
  (def inc*
    (r/with-proc {:procs  #{:p1 :p2}
                  :global {:my/number 0}
                  :local  {}}
      :inc {:my/number (+ (:my/number global) 1)}))

  (def dec*
    (r/with-proc {:procs  #{:t1 :t2}
                  :global {:my/number 0}
                  :local  {}}
      :dec {:my/number (- (:my/number global) 1)}))

  (r/run-procs [inc* dec*])

  (r/timeline-diff (r/run-procs [inc* dec*]))

  ())


(comment
  ;; checker
  (def inc*
    (r/with-proc {:procs  #{:p1 :p2}
                  :global {:my/number 0}
                  :local  {}}
      :inc {:my/number (+ (:my/number global) 1)}))

  (def dec*
    (r/with-proc {:procs  #{:t1 :t2}
                  :global {:my/number 0}
                  :local  {}}
      :dec {:my/number (- (:my/number global) 1)}))

  (def less-than-3?
    (r/checker
     (or #_(= (:my/number global) -2)
         (= (:my/number global) -1)
         (= (:my/number global) 0)
         (= (:my/number global) 1)
         (= (:my/number global) 2))))

  (r/run-procs [inc* dec*] less-than-3?)

  ())


(comment
  ;; finder
  (def inc*
    (r/with-proc {:procs  #{:p1 :p2}
                  :global {:my/number 0}
                  :local  {}}
      :inc {:my/number (+ (:my/number global) 1)}))

  (def dec*
    (r/with-proc {:procs  #{:t1 :t2}
                  :global {:my/number 0}
                  :local  {}}
      :dec {:my/number (- (:my/number global) 1)}))

  (def some-is-3?
    (r/finder
     (= (:my/number global) 2)))

  (r/run-procs [inc* dec*] some-is-3?)

  ())


(comment
  ;; steps ordering
  (def inc-dec
    (r/with-proc {:procs  #{:p1}
                  :global {:my/number 0}
                  :local  {}}
      :inc {:my/number (+ (:my/number global) 1)}
      :dec {:my/number (- (:my/number global) 1)}))

  (r/run-procs [inc-dec])

  (r/timeline-diff (r/run-procs [inc-dec]))


  ())


(comment
  ;; local
  (def inc*
    (r/with-proc {:procs  #{:p1 :p2}
                  :global {:my/number 0}
                  :local  {:amount 2}}
      :double-amount {:amount (* 2 amount)}
      :inc {:my/number (+ (:my/number global) amount)}))

  (->> (r/run-procs [inc*])
       (mapv #(select-keys % [:p1/amount
                              :p2/amount
                              :my/number])))

  (r/timeline-diff (r/run-procs [inc*]))

  ())


(comment
  ;; multiple initial states
  (def inc*
    (r/with-proc {:procs  #{:p1}
                  :global {:my/number 0}
                  :local  {:amount (r/one-of (range 3))}}
      :double-amount {:amount (* 2 amount)}
      :inc {:my/number (+ (:my/number global) amount)}))

  (r/run-procs [inc*]
               (r/finder
                (= (:p1/amount global) 4)))

  (r/timeline-diff
   (r/run-procs [inc*] (r/finder (= (:p1/amount global) 4))))

  ())

(comment
  ;; account system 1
  (def account-sys
    (r/with-proc {:procs #{:p1 :p2}
                  :global {:account/flavio 10
                           :account/biro   10}
                  :local {:sender-new-balance   0
                          :receiver-new-balance 0
                          :money                (r/one-of (range 5))
                          :sender               :account/flavio
                          :receiver             :account/biro}}
      :adapt   {}
      :pack    {:receiver-new-balance (+ (receiver global) money)
                :sender-new-balance   (- (sender global) money)}
      :give    {sender sender-new-balance}
      :receive {receiver receiver-new-balance}))

  (def correct-balances?
    (r/checker
     (= (+ (:account/flavio global)
           (:account/biro global))
        20)))

  (->> (r/run-procs [account-sys] correct-balances?)
       (mapv #(select-keys % [:account/flavio
                              :account/biro])))

  (r/timeline-diff (r/run-procs [account-sys] correct-balances?))

  ())


(comment
  ;; account system 2
  (def account-sys
    (r/with-proc {:procs #{:p1 :p2}
                  :global {:account/flavio 10
                           :account/biro   10}
                  :local {:sender-new-balance   0
                          :receiver-new-balance 0
                          :money                (r/one-of (range 5))
                          :sender               :account/flavio
                          :receiver             :account/biro}}
      :adapt    {}
      :pack     {:sender-new-balance   (- (sender global) money)
                 :receiver-new-balance (+ (receiver global) money)}
      :transfer {sender sender-new-balance
                 receiver receiver-new-balance}))

  (def correct-balances?
    (r/checker
     (= (+ (:account/flavio global)
           (:account/biro global))
        20)))

  (->> (r/run-procs [account-sys] correct-balances?)
       (mapv #(select-keys % [:account/flavio
                              :account/biro])))

  (r/timeline-diff (r/run-procs [account-sys] correct-balances?))

  (def correct-transfers?
    (r/checker
     (r/implies (and (r/finished? (:p1/pc global))
                     (r/finished? (:p2/pc global)))
       (and (= (:account/flavio global)
               (- 10 (:p1/money global) (:p2/money global)))
            (= (:account/biro global)
               (+ 10 (:p1/money global) (:p2/money global)))))))

  #_(->> (r/run-procs [account-sys] correct-transfers?)
         (mapv #(select-keys % [:account/flavio
                                :account/biro])))

  (r/timeline-diff (r/run-procs [account-sys] correct-transfers?))

  ())


(comment
  ;; account system 3
  (def account-sys
    (r/with-proc {:procs #{:p1 :p2}
                  :global {:account/flavio 10
                           :account/biro   10}
                  :local {:money                (r/one-of (range 5))
                          :sender               :account/flavio
                          :receiver             :account/biro}}
      :adapt    {}
      :transfer {sender   (- (sender global) money)
                 receiver (+ (receiver global) money)}))

  (def correct-balances?
    (r/checker
     (= (+ (:account/flavio global)
           (:account/biro global))
        20)))

  (->> (r/run-procs [account-sys] correct-balances?)
       (mapv #(select-keys % [:account/flavio
                              :account/biro])))

  (r/timeline-diff (r/run-procs [account-sys] correct-balances?))

  (def correct-transfers?
    (r/checker
     (r/implies (and (r/finished? (:p1/pc global))
                     (r/finished? (:p2/pc global)))
       (and (= (:account/flavio global)
               (- 10 (:p1/money global) (:p2/money global)))
            (= (:account/biro global)
               (+ 10 (:p1/money global) (:p2/money global)))))))

  (->> (r/run-procs [account-sys] correct-transfers?)
       (mapv #(select-keys % [:account/flavio
                              :account/biro])))

  (r/timeline-diff (r/run-procs [account-sys] correct-transfers?))

  ())
