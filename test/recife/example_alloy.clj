(ns recife.example-alloy
  (:require
   [clojure.set :as set]
   [recife.alloy :as ra :refer [defpred deffact deffun defassert]]
   [recife.util.ordering :as ord]))

(def sigs
  (ra/signatures
   '{Name {}
     Addr {}
     Eita {}
     Book {:relations {addr [Name -> :lone Addr]}}}))

(defpred show
  [b :- Book]
  (and (> (count (ra/join addr b)) 1)
       (> (count (ra/join addr b Name)) 1)))

(defpred add
  [b :- Book, b' :- Book, n :- Name, a :- Addr]
  (= (ra/join addr b')
     (set/union (ra/join addr b) (ra/cartesian n a))))

(defpred del
  [b :- Book, b' :- Book, n :- Name]
  (= (ra/join addr b')
     (set/difference (ra/join addr b) (ra/cartesian n Addr))))

(defpred showAdd
  [b :- Book, b' :- Book, n :- Name, a :- Addr]
  (and (add b b' n a)
       (> (count (ra/join addr b' Name)) 1)))

(defassert delUndoesAdd
  (ra/all [b :- Book, b' :- Book, b'' :- Book, n :- Name, a :- Addr]
    (if (and (empty? (ra/join addr b n))
             (add b b' n a)
             (del b' b'' n))
      (= (ra/join addr b) (ra/join addr b'')))))

(defassert addIdempotent
  (ra/all [b :- Book, b' :- Book, b'' :- Book, n :- Name, a :- Addr]
    (if (and (add b b' n a)
             (add b' b'' n a))
      (= (ra/join addr b') (ra/join addr b'')))))

(defassert addLocal
  (ra/all [b :- Book, b' :- Book, n :- Name, n' :- Name, a :- Addr]
    (if (and (add b b' n a)
             (not= n n'))
      (= (ra/join addr b n') (ra/join addr b n')))))

(-> (ra/build-alloy-module sigs
                           [show add]
                           #_[show add del showAdd delUndoesAdd addIdempotent addLocal]
                           {:run #_ "run showAdd for 3 but 2 Book"
                            "run {} for 3 but 2 Book"
                            #_"check delUndoesAdd for 3"
                            #_"check delUndoesAdd for 10 but 3 Book"
                            #_"check addIdempotent for 3"
                            #_"check addLocal for 3 but 2 Book"})
    (ra/run-alloy-expression {:expression "Addr"
                              :show-viz? true}))

(-> "
sig Book  {addr: Name -> lone Addr}

sig Eita  {}

sig Addr  {}

sig Name  {}

pred show[b: Book] {
  {{{#{addr.b}} > 1}
and
{{#{addr.b.Name}} > 1}}
}

pred add[b: Book, n: Name, a: Addr] {
  {{addr.b'} = {{addr.b} + {n -> a}}}
}

run {} for 3 but 2 Book

"
    (ra/run-alloy-expression {:expression "Addr"
                              :show-viz? true}))

(comment

  (ra/next-solution!)

  (ra/parse (ra/macroexpand-all '(addr b Name)))

  (try (ra/parse '((addr b)))
       (catch Exception e
         (clojure.pprint/pprint e)))

  (.-sig-map ((addr Book) Name))

  Addr

  addr

  ())
