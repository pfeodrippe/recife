(ns recife.example.alloy.migration
  "See https://www.hillelwayne.com/post/formally-modeling-migrations/."
  (:require
   [recife.alloy :as ry]))

(def sigs
  (ry/signatures
   '{Event {}

     Person {events [:set Event]
             likes [:set Event]
             hates [:set Event]}

     NewPerson {person Person}

     Attendance {event Event
                 attendee NewPerson}

     Review {:abstract true
             event Event
             reviewer NewPerson}

     Likes {:extends Review}
     Hates {:extends Review}}))

#_(ry/next-solution!)

(comment

  ;; Inside `with-eval` you can run Alloy expressions,
  ;; including multiple ones.
  (ry/with-eval
    attendee

    Review

    ;; As `event` appears in two signatures, we need
    ;; to disambiguate them.
    (ry/<* Attendance event)

    (ry/j Attendance attendee)

    ;; ~ operator.
    (ry/rev attendee)

    (ry/j NewPerson (ry/rev attendee))

    (ry/j NewPerson (ry/rev attendee) event)
    (ry/j NewPerson (ry/rev attendee) event))

  ())


;; pred valid[p: Person]
;; {
;;  no p.likes & p.hates
;; }
(ry/defpred valid
  [p :- Person]
  (empty? (ry/intersection
           (ry/j p likes)
           (ry/j p hates))))

;; pred valid[np: NewPerson] {
;;   all r1, r2: np.~reviewer |
;;     r1 in Likes and r2 in Hates
;;       implies r1.event != r2.event
;; }
(ry/defpred valid-new
  [np :- NewPerson]
  (ry/for-all [r1 :- "np.~reviewer"
               r2 :- "np.~reviewer"]
    (-> (and (ry/in r1 Likes)
             (ry/in r2 Hates))
        (ry/implies (not= (ry/j r1 event)
                          (ry/j r2 event))))))

;; {all p: Person | valid[p]}
(ry/defpred example
  []
  (ry/for-all [p :- Person]
    (valid p)))

;; pred same_events[p: Person, np: NewPerson] {
;;   p.events = np.~attendee.event
;; }
(ry/defpred same-events
  [p :- Person np :-> NewPerson]
  (= (ry/j p events)
     (ry/j np (ry/rev attendee) event)))

;; pred same_reviews[p: Person, np: NewPerson] {
;;  p.likes = (np.~reviewer & Likes).event
;;  p.hates = (np.~reviewer & Hates).event
;; }
(ry/defpred same-reviews
  [p :- Person np :-> NewPerson]
  (let [reviewer-rev (ry/rev reviewer)]
    (and (= (ry/j p likes)
            (-> (ry/j np reviewer-rev)
                (ry/intersection Likes)
                (ry/j event)))
         (= (ry/j p hates)
            (-> (ry/j np reviewer-rev)
                (ry/intersection Hates)
                (ry/j event))))))

;; pred equivalent[p: Person, np: NewPerson] {
;;   p = np.person
;;   same_events[p, np]
;;   same_reviews[p, np]
;; }
(ry/defpred equivalent
  [p :- Person np :-> NewPerson]
  (and (= p (ry/j np person))
       (same-events p np)
       (same-reviews p np)))

;; {
;;   one Person
;;   one NewPerson
;;   equivalent[Person, NewPerson]
;; }
(ry/defpred example2
  []
  (and (ry/one Person)
       (ry/one NewPerson)
       (equivalent Person NewPerson)))

;; assert equivalence_preserves_validity {
;;   all p: Person, np: NewPerson |
;;     equivalent[p, np] implies (valid[p] iff valid[np])
;; }
(ry/defassert equivalence-preserves-validity
  (ry/for-all [p :- Person np :- NewPerson]
    (ry/implies
     (equivalent p np)
     (ry/iff (valid p)
             (valid-new np)))))

(-> (ry/build-alloy-module sigs
                           [valid valid-new example same-events
                            same-reviews equivalent example2
                            equivalence-preserves-validity]
                           {:run
                            #_["run example2 for 2 but 1 Person, 1 NewPerson"]
                            #_["check equivalence_preserves_validity for 2 but 1 Person, 1 NewPerson"]
                            ["check equivalence_preserves_validity"]})
    (ry/run-alloy-expression {:debug true}))

(comment

  (ry/with-eval
    (ry/union Person Review)

    (ry/j Person likes)

    (ry/cartesian Person Event Review)

    (ry/for-all [p :- Person]
      (valid p)))

  (ry/next-solution!)

  (ry/eval-module-expression sigs "univ")

  ())
