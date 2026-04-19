#!/bin/sh
exec guile -L . "$0" "$@"
!#
;; Small test for your state-machine implementation (Guile Scheme)
(use-modules (components))
;; test-run.scm
;; Test for the "transition-request" design:
;; - state-resolver returns #t to request a transition, #f to stay.
;; - connectors map input -> index into lower-states (0-based).
;; - run always returns a state.

;; --- resolvers (return #t or #f) ---
(define (sr0 input)
  ;; from s0: 'go requests transition, otherwise stay
  (if (or (eq? input 'go)
	  (eq? input 'jump))
      #f
      #t)) ;; #t here is just a place holder, more tests to input could be applied
(define (sr1 input)
  ;; from s1: 'go requests transition, otherwise stay
  (not (or (eq? input 'go)
	   (eq? input 'jump))))
(define (sr2 input)
  ;; s2 is absorbing: never requests transition
  #t)
(define (sr3 input)
  ;; s3 is rejecting: always wish for transition unless there is no input anymore
  #f)

(define (conn0-res input)
  (cond ((eq? input 'go) 0)
        ((eq? input 'jump) 1)
	(else 2)))

(define (conn1-res input)
  (cond ((eq? input 'go) 0)
	((eq? input 'jump) 1)
	(else 2)))

(define (conn2-res input)
  (cond ((eq? input 'restart) 0)
	(else 1)))

(define-state start-state sr0)
(define-state first-state sr1)
(define-state second-state sr2)
(define-state third-state sr3)
(define-state error-state sr2)

(define-connector start-state-connector conn0-res)
(define-connector first-state-connector conn1-res)
(define-connector third-state-connector conn2-res)

(connect! start-state-connector (list start-state) (list first-state second-state error-state))
(connect! first-state-connector (list first-state) (list second-state third-state error-state))
(connect! third-state-connector (list third-state) (list start-state error-state))

(define-machine M start-state)

;; --- tests: list of (input-sequence . expected-state) ---
(define tests
  (list
   (cons '(go jump) third-state)
   ))

;; run tests and print results
(for-each
 (lambda (t)
   (let* ((in (car t))
	  (expected (cdr t))
	  (result (run M in)))
     (format #t "-----------------~%BEGIN~%[[[[inputs]]]]:~%~a~%[[[[result]]]]:~%~a~%[[[[expected]]]]:~%~a~%END~%---------------------~%"
             in result expected)))
 tests)
