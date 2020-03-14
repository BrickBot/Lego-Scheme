;; Lego Compiler - Establishes subroutines (beginofsub) out of
;;  define statements.
;; Version 0.5.2
;; Copyleft (C) 1999 by Kasey Klipsch, Mitch Wagner, and Adam Wick

(define yank-subroutines
  (let ()

    (define remove*
      (lambda (ls1 ls2)
	(cond
	 ((null? ls2) '())
	 ((member? (car ls2) ls1)
	  (remove* ls1 (cdr ls2)))
	 (else
	  (cons (car ls2) (remove* ls1 (cdr ls2)))))))

    (define get-deps
      (lambda (x)
	(c-record-case x
          ((quote primref) ignorem '())
	  ((var) (name)
           (list x))
	  ((define) (name exp)
           (get-deps exp))
	  ((lambda) (ids body)
           (remove* ids (get-deps body)))
	  ((seq) (exp1 exp2)
           (append (get-deps exp1) (get-deps exp2)))
	  ((if) (test conseq altern)
           (append (get-deps test) (get-deps conseq) (get-deps altern)))
	  ((let) (ids vals body)
           (append (apply append (map get-deps vals))
		   (remove* ids (get-deps body))))
	  ((call) (fun args)
           (append (get-deps fun) (apply append (map get-deps args)))))))

    (define replace-calls
      (lambda (x repls)
	(c-record-case x
          ((task) (num body)
           (make-task num (replace-calls body repls)))
          ((quote primref var) ignorem x)
	  ((define) (name exp)
           (make-define name (replace-calls exp repls)))
	  ((lambda) (ids body)
           (make-lambda ids (replace-calls body repls)))
	  ((seq) (exp1 exp2)
	   (make-seq (replace-calls exp1 repls)
		     (replace-calls exp2 repls)))
	  ((if) (test conseq altern)
           (make-if (replace-calls test repls)
		    (replace-calls conseq repls)
		    (replace-calls altern repls)))
	  ((let) (ids vals body)
           (make-let ids (map (lambda (x) (replace-calls x repls)) vals)
		     (replace-calls body repls)))
	  ((call) (fun args)
;           (printf "fun=~s~n" fun)
           (cond
	    ((and (var? fun) (assq (var-name fun) repls)) =>
             (lambda (x)
	       (make-call (make-var (cdr x) #t)
			  (map (lambda (x)
				 (replace-calls x repls))
			       args))))
	    (else
	     (make-call (replace-calls fun repls)
			(map (lambda (x) (replace-calls x repls)) args))))))))

;define-record quote (const))
;(define-record primref (prim))
;(define-record var (name top-level?))
;
;(define-record define (name exp))
;(define-record lambda (ids body))
;(define-record seq (exp1 exp2))
;(define-record if (test conseq altern))
;(define-record let (ids vals body))
;(define-record letrec (ids vals body))
;(define-record call (fun args))
    (define task-names '(task_zero task_one task_two task_three task_four
				   task_five task_six task_seven task_eight
				   task_nine))

    (define get-new-fun-num
      (let ((x -1))
	(case-lambda
	 [() (begin (set! x (+ x 1)) x)]
	 [(reset) (set! x -1)])))

    (lambda (x)
      (get-new-fun-num #t)
      (let* ((tasks (program-tasks x))
	     (unbound-defines (program-others x))
	     (dependencies (map get-deps unbound-defines))
	     (clean-list (map (lambda (x)
				(andmap (lambda (x)
					  (member? x task-names)) x))
			      dependencies))
	     (global-functions (let loop ([funs unbound-defines]
					  [cleans clean-list])
				 (cond
				  ((null? funs) '())
				  ((car cleans)
				   (cons (make-func (get-new-fun-num)
						    (define-exp (car funs)))
					 (loop (cdr funs) (cdr cleans))))
				  (else (loop (cdr funs) (cdr cleans))))))
	     (replace-list (let loop ([funs unbound-defines]
				      [cleans clean-list]
				      [newfuns global-functions])
			     (cond
			      ((null? funs) '())
			      ((not (car funs))
			       (loop (cdr funs) (cdr cleans) newfuns))
			      (else
			       (cons (cons (var-name (define-name (car funs)))
					   (func-num (car newfuns)))
				     (loop (cdr funs) (cdr cleans) 
					   (cdr newfuns)))))))
	     (new-tasks (map (lambda (x)
			       (replace-calls x replace-list)) tasks))
	     )
	(make-program new-tasks global-functions)))))

	    
				    