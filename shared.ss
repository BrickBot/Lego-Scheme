;; Lego Compiler - Shared structures and functions
;; Version 0.5.2
;; Copyleft (C) 1999 by Kasey Klipsch, Mitch Wagner, and Adam Wick


(define-record quote (const))
(define-record primref (prim))
(define-record var (name top-level?))

(define-record define (name exp))
(define-record lambda (ids body))
(define-record seq (exp1 exp2))
(define-record if (test conseq altern))
(define-record let (ids vals body))
(define-record letrec (ids vals body))
(define-record call (fun args))

(define-record program (tasks others))
(define-record task (num body))
(define-record func (num body))

(define-syntax c-record-case
  (letrec ([lets
	     (lambda (namesyn fields obj)
	       (let ([name (symbol->string (syntax-object->datum namesyn))])
		 (let f ([fields fields])
		   (if (null? fields)
		       '()
		       (let* ([fieldsyn (car fields)]
			      [field (syntax-object->datum fieldsyn)]
			      [var (datum->syntax-object
				     namesyn
				     (if (pair? field) (car field) field))]

			      [field (if (pair? field) (cadr field) field)]

			      [fieldstr
				(symbol->string field)]

			      [accessor
				(datum->syntax-object
				  namesyn
				  (string->symbol
				    (string-append
				      name "-" fieldstr)))])
			 `([,var (,accessor ,obj)]
			   ,@(f (cdr fields))))))))]

	   [lets2
	     (lambda (syn fields obj)
	       (let f ([fields fields])
		 (if (null? fields)
		     '()
		     (let* ([fieldsyn (car fields)]
			    [field (syntax-object->datum fieldsyn)]
			    [var (datum->syntax-object
				   syn
				   (if (pair? field) (car field) field))]
			    
			    [field (if (pair? field) (cadr field) field)]
			    [field (datum->syntax-object syn `(quote ,field))]
			    )
		       `([,var
			   ((,(datum->syntax-object syn record-field-accessor)
			     (,(datum->syntax-object syn
				 record-type-descriptor) ,obj)
			     ,field)
			     ,obj)]
			 ,@(f (cdr fields)))))))]
	   )
    (lambda (x)
      (syntax-case x (ignorem else)
	[(_ x (else e1 e2 ...))
	 (syntax (begin e1 e2 ...))]
	[(_ x)
	 (syntax (error '_ "unexpected record ~s (~s)" 'x x))]
	[(_ (e1 e2 ...) more ...)
	 (syntax (let ([x (e1 e2 ...)])
		   (_ x more ...)))]
	
	[(_ x ((key ...) ignorem e1 e2 ...) more ...)
	 (andmap
	   identifier?
	   (syntax (key ...)))
	 (with-syntax ([(pred? ...)
			(map
			  (lambda (x)
			    (datum->syntax-object (syntax _)
			      (string->symbol
				(string-append
				  (symbol->string
				    (syntax-object->datum
				      x))
				  "?"))))
			  (syntax (key ...)))])
	   (syntax (if (or (pred? x) ...)
		       (begin e1 e2 ...)
		       (_ x more ...))))]
	
	[(_ x ((key) (id ...) e1 e2 ...) more ...)
	 ;(andmap identifier? (syntax (id ...)))
	 (with-syntax ([pred? (datum->syntax-object (syntax _)
				(string->symbol
				  (string-append
				    (symbol->string
				      (syntax-object->datum (syntax key)))
				    "?")))]
		       [lets (lets (syntax key)
			       (syntax (id ...))
			       (syntax x))])
	   (syntax (if (pred? x)
		       (let lets e1 e2 ...)
		       (_ x more ...))))]
	))))

(define member?
  (lambda (a ls)
    (call/cc
     (lambda (k)
       (letrec ([work (lambda (ls)
			(cond 
			 [(null? ls) (k #f)]
			 [(eq? (car ls) a) (k #t)]
			 [else (work (cdr ls))]))])
	 (work ls))))))

(define our-primitives
  (list ;; robot primitives
   ;; sensor primitives
   'read-from-sensor 'setup-sensor 'clear-sensor
   ;; engine primitives
   'set-engine-speed 'set-engine-direction 
   'flip-engine-direction 'set-engine-on/off  
   ;; ir primitives
   'incoming-message? 'receive-message 'send-message
   ;; sound primitives
   'play-system-sound 'play-tone
   ;; timer primitives
   'clear-timer 'read-from-timer
   ;; other primitives
   'wait
   ))

(define primitives
  (append
   '(
    ;; Math primitives
    + - * / abs and or sign
    ;; conditionals
    < > = <>
    ;; list primitives
    cons car cdr cadr caar cdar cddr list map append null?
    ;; other, uninvited, primitives
    void
    ) our-primitives))

