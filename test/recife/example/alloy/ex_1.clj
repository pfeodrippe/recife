(ns recife.example.alloy.ex-1
  (:require
   [clojure.set :as set]
   [recife.alloy :as ry]
   #_[recife.util.ordering :as ord]))

(def sigs
  (ry/signatures
   '{Name {}
     Addr {}
     Book {:relations {addr [Name -> :lone Addr]}}}))

;; TODO: See how to call `show` as a normal Clojure function, we may need
;; to generate a random instance of an atom (what about the relations?).
(ry/defpred show
  [b :- Book]
  (and (> (count (ry/j b addr)) 1)
       (> (count (->> (ry/j b addr)
                      (ry/j Name)))
          1)))

#_(defpred add
    [b1 :- Book, b2 :- Book, n :- Name, a :- Addr]
    (= (ry/j addr b1)
       (set/union (ry/j addr b) (ry/cartesian n a))))

#_(defpred del
  [b :- Book, b' :- Book, n :- Name]
  (= (ry/j addr b')
     (set/difference (ry/j addr b) (ry/cartesian n Addr))))

#_(defpred showAdd
  [b :- Book, b' :- Book, n :- Name, a :- Addr]
  (and (add b b' n a)
       (> (count (ry/j addr b' Name)) 1)))

#_(defassert delUndoesAdd
  (ry/all [b :- Book, b' :- Book, b'' :- Book, n :- Name, a :- Addr]
    (when (and (empty? (ry/j addr b n))
             (add b b' n a)
             (del b' b'' n))
      (= (ry/j addr b) (ry/j addr b'')))))

#_(defassert addIdempotent
  (ry/all [b :- Book, b' :- Book, b'' :- Book, n :- Name, a :- Addr]
    (when (and (add b b' n a)
             (add b' b'' n a))
      (= (ry/j addr b') (ry/j addr b'')))))

#_(defassert addLocal
  (ry/all [b :- Book, b' :- Book, n :- Name, n' :- Name, a :- Addr]
    (when (and (add b b' n a)
             (not= n n'))
      (= (ry/j addr b n') (ry/j addr b n')))))

(comment

  (-> (ry/build-alloy-module sigs
                             [show]
                             #_[show add del showAdd delUndoesAdd addIdempotent addLocal]
                             {:run #_ "run showAdd for 3 but 2 Book"
                              "run show for 3 but 1 Book"
                              #_"check delUndoesAdd for 3"
                              #_"check delUndoesAdd for 10 but 3 Book"
                              #_"check addIdempotent for 3"
                              #_"check addLocal for 3 but 2 Book"})
      (ry/run-alloy-expression {:expression "Addr"
                                :show-viz? true}))

  (ry/next-solution!)

  (-> "
sig Book  {addr: Name -> lone Addr}
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
      (ry/run-alloy-expression {:expression "Addr"
                                :show-viz? true}))

  ())

(comment

  (ry/next-solution!)

  (ry/parse (ry/macroexpand-all '(addr b Name)))

  (.-sig-map ((addr Book) Name))

  Addr

  addr

  ())
