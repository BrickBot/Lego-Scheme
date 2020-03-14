;; Lego Compiler - Numerize variables: A very, very stupid register
;;   allocation pass, which numbers variables according to Spirit.OCX
;;   mandates.
;; Version 0.5.2
;; Copyleft (C) 1999 by Kasey Klipsch, Mitch Wagner, and Adam Wick

(define numerize-variables
  (let ()

    (define used-regs
      (vector #t #t #f #f #f #f #f #f #f #f 
	      #f #f #f #f #f #f #f #f #f #f
	      #f #f #f #f #f #f #f #f #f #f
	      #f #f))

    (define number-of-variables
      (lambda (x)
	(c-record-case x
	 ((task) (num body)
          (number-of-variables body))
	 ((func) (body)
          (number-of-variables body))
	 ((quote primref var) ignorem 0)
	 ((lambda) (ids body)
          (+ (length ids) (number-of-variables body)))
	 ((seq) (exp1 exp2)
	  (+ (number-of-variables exp1) (number-of-variables exp2)))
	 ((if) (test conseq altern)
	  (+ (number-of-variables test) (number-of-variables conseq)
	     (number-of-variables altern)))
	 ((let) (ids vals body)
          (+ (length ids) (apply + (map number-of-variables vals))
	     (number-of-variables body)))
	 ((letrec) (ids vals body)
          (+ (length ids) (apply + (map number-of-variables vals))
	     (number-of-variables body)))
	 ((call) (fun args)
          (+ (number-of-variables fun) 
	     (apply + (map number-of-variables args)))))))

    (define get-new-reg
      (let ((x 1))
	(case-lambda
	 [() (begin (set! x (+ x 1)) x)]
	 [(reset) (set! x 1)])))
	

    (define bad-regalloc
      (lambda (x repls)
	(c-record-case x
	 ((task) (num body)
          (make-task num (bad-regalloc body repls)))
	 ((func) (num body)
          (make-func num (bad-regalloc body repls)))
	 ((quote primref) ignorem x)
	 ((var) ignorem
	  (cond
	   ((assq x repls) =>
            (lambda (x) (cdr x)))
	   (else x)))
	 ((lambda) (ids body)
	  (let* ((new-names (map (lambda (x) (get-new-reg)) ids))
		 (new-names (map (lambda (x) (make-var x #f)) new-names))
		 (newrepls (map cons ids new-names)))
	    (make-lambda new-names (bad-regalloc body newrepls))))
	 ((seq) (exp1 exp2)
          (make-seq (bad-regalloc exp1 repls) (bad-regalloc exp2 repls)))
	 ((if) (test conseq altern)
	  (make-if (bad-regalloc test repls) (bad-regalloc conseq repls)
		   (bad-regalloc altern repls)))
	 ((let) (ids vals body)
	  (let* ((new-names (map (lambda (x) (get-new-reg)) ids))
		 (new-names (map (lambda (x) (make-var x #f)) new-names))
		 (newrepls (map cons ids new-names)))
	    (make-let new-names (map (lambda (x) (bad-regalloc x repls)) vals)
		      (bad-regalloc body newrepls))))
	 ((letrec) (ids vals body)
	  (let* ((new-names (map (lambda (x) (get-new-reg)) ids))
		 (new-names (map (lambda (x) (make-var x #f)) new-names))
		 (newrepls (map cons ids new-names)))
	    (make-letrec new-names 
			 (map (lambda (x) (bad-regalloc x newrepls)) vals)
			 (bad-regalloc body newrepls))))
	 ((call) (fun args)
          (make-call (bad-regalloc fun repls) 
		     (map (lambda (x) (bad-regalloc x repls)) args)))
	 )))

;    (define find-arg-regs
;      (lambda (fun tasks funcs)
;	(let* ((task-names (map (lambda (x) (task-num x)) tasks))
;	       (func-names (map (lambda (x) (func-num x)) funcs))
;	       (tasks? (member (var-name fun) task-names))
;	       (ls (if tasks? tasks funcs))
;	       (name-accessor (if tasks? task-num func-num))
;	       (body-accessor (if tasks? task-body func-body))
	;;       (thingy (let loop ([ls ls])
	;		 (cond
	;		  ((null? ls) 
	;		   (error 'find-arg-regs "Internal error."))
	;		  ((eq? (var-name fun) (name-accessor (car ls)))
	;		   (body-accessor (car ls)))
	;		  (else (loop (cdr ls))))))
	;       (args (lambda-ids thingy)))
	;  args)))
		 

;    (define fixcalls
;      (lambda (x tasks funcs)
;	(c-record-case x
;	  ((task) (num body)
;           (make-task num (fixcalls body tasks funcs)))
;	  ((func) (num body)
;           (make-funcs num (fixcalls body tasks funcs)))
;          ((quote primref var) ignorem x)
;	  ((lambda) (ids body)
;           (make-lambda ids (fixcalls body tasks funcs)))
;	  ((seq) (exp1 exp2)
;	   (make-seq (fixcalls exp1 tasks funcs) (fixcalls exp2 tasks funcs)))
;	  ((if) (test conseq altern)
;           (make-if (fixcalls test tasks funcs) (fixcalls conseq tasks funcs) 
;		    (fixcalls altern tasks funcs)))
;	  ((let) (ids vals body)
;           (make-let ids (map (lambda (x) (fixcalls x tasks funcs)) vals)
;		     (fixcalls x tasks funcs)))
;	  ((call) (fun args)
;           (cond
;	    ((var? fun)
;	     (let ((regs (find-arg-regs fun tasks funcs)))
;	       (make-call (fixcalls fun tasks funcs)
;			  (map cons 
;			       (map (lambda (x) (fixcalls x tasks funcs)) args)
;			       regs))))
;	    (else 
;	     (make-call (fixcalls fun tasks funcs)
;			(map (lambda (x) (fixcalls x tasks funcs)) args))))))))

;(define-record quote (const))
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
	 

    (lambda (x)
      (let* ((tasks (program-tasks x))
	     (funcs (program-others x))
	     (regsneeded (map number-of-variables tasks))
	     (regsneeded (append regsneeded (map number-of-variables funcs)))
	     (worthless (get-new-reg 'clear))
	     (need-help
	      (if (> (apply + regsneeded) 31)
		  (begin
		    (printf "Warning: You program needs ~s variables.~n") #t)
		  #f))
	     (regalloc (if need-help good-regalloc bad-regalloc))
	     (new-tasks (map (lambda (x) (regalloc x '())) tasks))
	     (new-funcs (map (lambda (x) (regalloc x '())) funcs))
;	     (new-tasks (map (lambda (x) 
;			       (fixcalls x new-tasks new-funcs)) 
;			     new-tasks))
;	     (new-funcs (map (lambda (x) 
;			       (fixcalls x new-tasks new-funcs)) 
;			     new-funcs))
	     )
	(make-program new-tasks new-funcs)))))
		 
	