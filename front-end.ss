;; Lego Compiler - Front end procedures, those which read from
;;  the .lss file and:
;;    o Turn the statements into records
;;    o Do a little initial error/syntax checking
;; Version 0.5.2
;; Copyleft (C) 1999 by Kasey Klipsch, Mitch Wagner, and Adam Wick

(define read-file
  (case-lambda
   [(file) (read-file (open-input-file file) '())]
   [(port acc) (let ((instuff (read port)))
		 (if (eof-object? instuff) acc
		     (read-file port `(,@acc ,instuff))))]))

(define recordify-exp
  (lambda (exp)
    (cond
     ;; simple constants
     ((or (number? exp) (string? exp))
      (make-quote exp))
     ;; booleans need to be converted to their numeric counterparts first
     ((and (boolean? exp) exp)
      (make-quote 1))
     ;; and false ...
     ((boolean? exp)
      (make-quote 0))
     ;; quoted symbols are constants, too!
     ((and (list? exp) (eq? (car exp) 'quote) (= 1 (length (cdr exp)) 1))
      (make-quote (cadr exp)))
     ;; see if its a primitive
     ((member? exp primitives)
      (make-primref exp))
     ;; sometimes expand dumps out optimized primitives, so yank
     ;; those out while we're at it.
     ((and (list? exp) (eq? (car exp) '|#primitive|) (= 3 (length exp)))
      (make-primref (caddr exp)))
     ;; all other symbols better be variable names
     ((symbol? exp)
      (make-var exp #f))
     ;; if its gotten here, and we're at a non-language-construct, then
     ;; somebody's being stupid. So error out.
     ((or (atom? exp) (null? exp))
      (error 'recordify-exp "Illegal syntax ~s" exp))
     ;; now deal with language constructs ...
     ;; first do define. Define has a number of special things
     ;; about it in LEGO/Scheme:
     ;;   o It can only be used at top-level
     ;;   o Anything defined is automatically a top-level variable
     ;;   o There must be a (define task_zero ...)
     ;; the first property is not checked yet. It will be in the next
     ;; pass.
     ((and (eq? (car exp) 'set!) (= 3 (length exp)) (symbol? (cadr exp)))
      (make-define (make-var (cadr exp) #t) (recordify-exp (caddr exp))))
     ;; next is lambda
     ((and (eq? (car exp) 'lambda) (list? (cadr exp)) (> (length exp) 2))
;      (begin
;	(printf "ids = ~s~n" (cadr exp))
;	(printf "body = ~s~n" (caddr exp))
      (make-lambda (map recordify-exp (cadr exp)) 
		   (recordify-exp (cons 'begin (cddr exp)))));)
     ;; the lambda didn't have enough parts to it, so error.
     ((eq? (car exp) 'lambda)
      (error 'recordify-exp "Illegal lambda expression: ~s" exp))
     ;; then if, with only one arm. (i.e., has no else)
     ((and (eq? (car exp) 'if) (= 3 (length exp)))
      (make-if (recordify-exp (cadr exp)) (recordify-exp (caddr exp))
	       (make-call (make-primref void) '())))
     ;; if with two arms (i.e., has an else)
     ((and (eq? (car exp) 'if) (= 4 (length exp)))
      (make-if (recordify-exp (cadr exp)) (recordify-exp (caddr exp))
	       (recordify-exp (cadddr exp))))
     ;; letrec. Note that expand removes let from the syntax, which
     ;; we'll re-introduce in the next pass
     ((and (eq? (car exp) 'letrec) (> 2 (length exp)))
      (make-letrec (map recordify-exp (map car (cadr exp))) ;; fix the ids
		   (map recordify-exp (map cadr (cadr exp))) ;; fix the values
		   (recordify-exp (cons 'begin (caddr exp))))) ;; fix the body
     ;; begin, with only one statement. This is a base case to the
     ;; next version of begin. Our 'seq' construct takes exactly two
     ;; expressions (for convenience, mostly). So a begin with one
     ;; expression is really just that expression
     ((and (eq? (car exp) 'begin) (= 2 (length exp)))
      (recordify-exp (cadr exp)))
     ;; begin with exactly two statements.
     ((and (eq? (car exp) 'begin) (= 3 (length exp)))
      (make-seq (recordify-exp (cadr exp)) (recordify-exp (caddr exp))))
     ;; begin with more than one statement. Note the recursion, building
     ;; begin statements with exactly two bodies
     ((and (eq? (car exp) 'begin) (> (length exp) 3))
      (make-seq (recordify-exp (cadr exp))
		  (recordify-exp (cons 'begin (cddr exp)))))
     ;; calls. we assume that anything that did not hit the above
     ;; statements is a call
     (else
      (make-call (recordify-exp (car exp)) (map recordify-exp (cdr exp)))))))
     
(define global-task-list '()) 
(define check-syntax/link-vars
  (lambda (x repls top-level)
    (c-record-case x
      ((var) (name top-level?)
       (cond
	((assq name repls) =>
         (lambda (x) (cdr x)))
	((assq name global-task-list) =>
         (lambda (x) (cdr x)))
;	((is-task? (make-define x '())) =>
;	 (lambda (x) (make-var x #t)))
	(else 
	 (let ((newname (make-var name #t)))
	   (set! global-task-list (cons (cons name newname) global-task-list))
	   newname))))
      ((define) (name exp)
       (if top-level 
	   (make-define name 
			(check-syntax/link-vars exp 
						(cons
						 (cons (var-name name) name)
						 repls) #f))
	   (error 'check-syntax/link-vars
		  "Syntax error: Define only allowed at top level.")))
      ((seq) (exp1 exp2)
       (make-seq (check-syntax/link-vars exp1 repls #f)
		 (check-syntax/link-vars exp2 repls #f)))
      ((letrec) (ids vals body)
       (let ((newrepls (append (map cons (map var-name ids) ids) repls)))
	 (make-letrec ids (map (lambda (x) 
				 (check-syntax/link-vars x newrepls #f))
			       vals) 
		      (check-syntax/link-vars body newrepls #f))))
      ((call) (fun args)
       (make-call (check-syntax/link-vars fun repls #f)
		  (map (lambda (x) (check-syntax/link-vars x repls #f)) args)))
      ((lambda) (ids body)
       (make-lambda ids 
		    (check-syntax/link-vars body
					 (append (map cons 
						      (map var-name ids) 
						      ids) repls) #f)))
      ((quote) (const) x)
      ((if) (test conseq altern)
       (make-if (check-syntax/link-vars test repls #f)
		(check-syntax/link-vars conseq repls #f)
		(check-syntax/link-vars altern repls #f)))
      ((primref) (prim) x)
      (else
       (error 'check-syntax/link-vars
	      "Invalid record or expression reached.")))))
     
(define letify
  (lambda (x)
    (c-record-case x
     ;; ignore simple structures
     ((quote var primref) ignorem x)
     ;; defines: just recurse and rebuild
     ((define) (name exp)
      (make-define name (letify exp)))
     ;; lambdas: Again, just recurse and rebuild
     ((lambda) (ids body)
      (make-lambda ids (letify body)))
     ;; seqs: recurse and rebuild
     ((seq) (exp1 exp2)
      (make-seq (letify exp1) (letify exp2)))
     ;; ifs: recurse and rebuild
     ((if) (test conseq altern)
      (make-if (letify test) (letify conseq) (letify altern)))
     ;; letrec: recurse and rebuild
     ((letrec) (ids vals body)
      (make-letrec ids (map letify vals) (letify body)))
     ;; call: This is the tricky one. If we see a lambda in the
     ;; function position, then this is really just a let. 
     ;; Otherwise its a true call, and we don't want to do 
     ;; anything. 
     ((call) (fun args)
      (cond
       ;; See if its a lambda with the prober number of arguments
       ((and (lambda? fun) (= (length (lambda-ids fun)) (length args)))
	(make-let (lambda-ids fun) (map letify args) 
		  (letify (lambda-body fun))))
       ;; if its a lambda here, then someone screwed up and the
       ;; number of formal arguments is != the number of actual
       ;; arguments
       ((lambda? fun)
	(error 'letify "Invalid argument count in call."))
       ;; otherwise its just a normal call
       (else
	(make-call (letify fun) (map letify args)))))
     )))
       
(define is-task?
  (lambda (x)
    (case (var-name (define-name x))
      ((task_zero) 0)
      ((task_one) 1)
      ((task_two) 2)
      ((task_three) 3)
      ((task_four) 4)
      ((task_five) 5)
      ((task_six) 6)
      ((task_seven) 7)
      ((task_eight) 8)
      ((task_nine) 9)
      (else  #f))))

(define split-definitions
  (lambda (ls k)
    (cond
     ((null? ls) (k '() '()))
     ((call? (car ls)) (split-definitions (cdr ls) k))
     (else
      (split-definitions (cdr ls)
        (lambda (tasks others)
	  (cond
	   ((is-task? (car ls)) =>
	    (lambda (num)
	      (k (cons (make-task num (define-exp (car ls))) tasks) 
		 others)))
	   (else (k tasks (cons (car ls) others))))))))))
	    
     
(define recordify
  (lambda (fn)
    (let* (;; read the file in
	   (exp (read-file fn))
	   ;; make sure they didn't do anything stupid, like use 
	   ;; anything but define at the top level.
	   (check1 (if (not (andmap (lambda (x) 
				      (and (list? x) (eq? (car x) 'define)))
				    exp))
		       (error 'recordify
			      "Syntax error: Only defines are allowed at~s"
			      " top level.")))
	   ;; expand the fuck out of it
	   (exp (cdr (expand (cons 'begin exp))))
	   ;; recordify the define statements
	   (record-list (map recordify-exp exp))
	   ;; check some of the syntax and link variables together, so
	   ;; that lexically equal variable references are equal in the
	   ;; sense of eq
	   (record-list (map (lambda (x) 
			       (check-syntax/link-vars x '() #t))
			     record-list))
	   ;; now, expand yanked out let from the syntax. Thats not
	   ;; good for us as we have a very limited number of functions
	   ;; and tasks to work with. So re-introduce those.
	   (record-list (map letify record-list))
	   )
      ;; the last thing we need to do is to seperate the tasks from
      ;; the other definitions. 
      (split-definitions record-list
        (lambda (tasks others)
	  (make-program tasks others))))))
	   
