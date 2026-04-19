(define-module (components)
  #:use-module (srfi srfi-9) ; records
  #:use-module (srfi srfi-9 gnu) ; gnu extension for srfi-9, customize record printer
  #:export (<state>
	    <connector>
	    <machine>

	    make-state
	    make-connector
	    make-machine

	    state?
	    connector?
	    machine?
	    
	    define-state
	    define-connector
	    define-machine

	    connect!
	    run))

(define-record-type <state>
  (make-state name validator resolver connector)
  state?
  (name state-name)
  (validator state-validator set-state-validator!)
  (resolver state-resolver set-state-resolver!)
  (connector state-connector set-state-connector!))
(define-record-type <connector>
  (make-connector name resolver lower-states)
  connector?
  (name connector-name)
  (resolver connector-resolver set-connector-resolver!)
  (lower-states connector-lower-states set-connector-lower-states!))
(define-record-type <machine>
  (make-machine name first-point)
  machine?
  (name machine-name)
  (first-point machine-first-point set-machine-first-point!))

(set-record-type-printer! <state>
  (lambda (record port)
    (format port "<[STATE] ~a>"
	    (state-name record))))
(set-record-type-printer! <connector>
  (lambda (record port)
    (format port "<[CONNECTOR] ~a>"
	    (connector-name record))))
(set-record-type-printer! <machine>
  (lambda (record port)
    (format port "<[MACHINE] ~a>"
	    (machine-name record))))

(define-syntax define-state
  (lambda (x)
    (syntax-case x ()
      ((_ name validator)
       (with-syntax ((state-name (datum->syntax #'name (syntax->datum #'name)))
		     (str-name (symbol->string (syntax->datum #'name)))
		     (state-validator (datum->syntax #'validator (syntax->datum #'validator))))
	 #`(define state-name (make-state str-name state-validator #f #f)))))))
(define-syntax define-connector
  (lambda (x)
    (syntax-case x ()
      ((_ name resolver)
       (with-syntax ((connector-name (datum->syntax #'name (syntax->datum #'name)))
		     (str-name (symbol->string (syntax->datum #'name)))
		     (connector-resolver (datum->syntax #'resolver (syntax->datum #'resolver))))
	 #`(define connector-name (make-connector str-name connector-resolver '())))))))
(define-syntax define-machine
  (lambda (x)
    (syntax-case x ()
      ((_ name first-point)
       (with-syntax ((machine-name (datum->syntax #'name (syntax->datum #'name)))
		     (str-name (symbol->string (syntax->datum #'name)))
		     (machine-first-point (datum->syntax #'first-point (syntax->datum #'first-point))))
	 #`(define machine-name (make-machine str-name machine-first-point)))))))

(define (connect! connector upper-states lower-states)
  (map (lambda (x) (set-state-connector! x connector)) upper-states)
  (set-connector-lower-states! connector lower-states))

(define (run machine list-of-inputs)
  (let loop ((current-state (machine-first-point machine))
	     (inputs list-of-inputs))
    (if (null? inputs)
	(begin (display current-state)
	       (newline)
	       current-state)
	(let* ((input (car inputs))
	       (state-valid-or-not ((state-validator current-state) input)))
	  (format #t "~a --~a--> " current-state input)
	  (if state-valid-or-not
	      (begin (display current-state)
		     (newline)
		     (loop current-state (cdr inputs)))
	      (let ((conn (state-connector current-state)))
		(let ((resolver (connector-resolver conn))
		      (lower-states (connector-lower-states conn)))
		  (let ((next-state (list-ref lower-states (resolver input))))
		    (loop (or next-state current-state) (cdr inputs))))))))))
