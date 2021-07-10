(ns example.partner
  "This is a simple protocol for a integration which requires you to sign up
  children before you sign up any parent entities, it's required you to know the
  ids of the children beforehand (yes, it's a bad API design by our integration
  partner, but which we have to deal with nonetheless). It's a simplification of
  a real world spec which I wrote and found a violation, which made us change
  the implementation.

  Our entities are companies which can have children which also are companies,
  so you can have grandchildren or great-grandchildren (but no loops, you cannot
  have a child C1 who has a parent P1 where C1 has P1 as one of its children
  (transitively or directly)).
  ")

(def global
  ;; All global keywords should be namespaced so we can differentiate it
  ;; from local variables in our processes.
  {::accounts {}})

;; In the real world, we talk to our partner through a HTTP request, but here
;; we don't need to bother about status, HTTP or error handling. We will model
;; a API request by putting a element in a set, which the partner process will
;; use, retrieving the "request" information.

(comment

  ;; TODO for implementation:
  ;; - [ ] Add database (Postgres).
  ;; - [ ] Daycare center communication is through an HTTP request.

  ())
