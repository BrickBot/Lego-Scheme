;; Lego Compiler - Back end for the compiler: takes records in
;;   a standard format and generates Spirit.OCX code
;; Version 0.5.2
;; Copyleft (C) 1999 by Kasey Klipsch, Mitch Wagner, and Adam Wick

(define function-ok?
  (lambda (x)
    (c-record-case x
      ((primref) (prim)
       (case prim
	 ((> < = <>) #t)
	 (else #f)));)
      (else #f))))

(define task-ids '())
(define dump-if-exp
  (lambda (test conseq altern dest)
    (c-record-case test
      ((call) (fun args)
       (cond
	((not (= (length args) 2))
	 (error 'dump-exp
		"Tests must contain exactly two numbers."))
	((not (function-ok? fun))
	 (error 'dump-exp
		"Test must be '>', '<', '=', or '<>'."))
	(else
	 (let ((op (cond
		    ((eq? (primref-prim fun) '>) 0)
		    ((eq? (primref-prim fun) '<) 1)
		    ((eq? (primref-prim fun) '=) 2)
		    ((eq? (primref-prim fun) '<>) 3))))
	   `(,@(dump-exp (car args) 0)
	     ,@(dump-exp (cadr args) 1)
	     (if 0 0 ,op 0 1)
	     ,@(dump-exp conseq)
	     (else)
	     ,@(dump-exp altern)
	     (endif))))))
      (else
       (error 'dump-exp "Unexpected Test Error")))))

(define dump-robot-prim
  (lambda (fun args prim dest)
    (case prim
      ;; sensor primitives
      ((read-from-sensor)
       (cond
	((not (and (quote? (car args)) (number? (quote-const (car args)))))
	 (error 'dump-exp "Read-from-sensor takes a _numeric_ sensor #"))
	((or (< (quote-const (car args)) 0) (> (quote-const (car args)) 2))
	 (error 'dump-exp "Arg to read-from-sensor out of bounds."))
	(else
	 `((setvar ,dest 9 ,(quote-const (car args)))))))
      ((setup-sensor)
       (cond
	((not (and (quote? (car args)) (number? (quote-const (car args)))))
	 (error 'dump-exp "Setup-sensor takes a _numeric_ sensor #"))
	((or (< (quote-const (car args)) 0) (> (quote-const (car args)) 2))
	 (error 'dump-exp "Arg to setup-sensor out of bounds."))
	((not (and (quote? (cadr args)) (symbol? (quote-const (cadr args)))))
	 (error 'dump-exp "Setup sensor requires symbolic type."))
	((not (and (quote? (caddr args)) (symbol? (quote-const (caddr args)))))
	 (error 'dump-exp "Setup sensor requires symbolic mode."))
	(else
	 (let* ((sensor/num (quote-const (car args)))
		(type/sym (quote-const (cadr args)))
		(mode/sym (quote-const (caddr args)))
		(type/num (case type/sym
			    ((none) 0)
			    ((switch) 1)
			    ((temperature) 2)
			    ((reflection) 3)
			    ((angle) 4)
			    (else
			     (error 'dump-exp "Invalid type in setup-sensor"))))
		(ok-types (vector '(raw boolean transition-count
					periodic-count percent
					celcius fahrenheit angle)
				  '(raw boolean transition-count
					periodic-count)
				  '(raw celcius fahrenheit percent)
				  '(raw boolean percent)
				  '(raw angle percent)))
		(checkmode (if (not (member? mode/sym 
					     (vector-ref ok-types type/num)))
			       (error 'dump-exp
				      "Mode not allowed with given type.")))
		(mode/num (case mode/sym
			    ((raw) 0)
			    ((boolean) 1)
			    ((transition-count) 2)
			    ((periodic-count) 3)
			    ((percent) 4)
			    ((celcius) 5)
			    ((fahrenheit) 6)
			    ((angle) 7))))
	   `((setsensortype ,sensor/num ,type/num)
	     (setsensormode ,sensor/num ,mode/num 0))))))
      ((clear-sensor)
       (cond 	
	((not (and (quote? (car args)) (number? (quote-const (car args)))))
	 (error 'dump-exp "Clear-sensor requires a numeric frequency."))
	((or (< (quote-const (car args)) 0) (> (quote-const (car args)) 2))
	 (error 'dump-exp "Clear-sensor argument is out of bounds."))
	(else
	 `((clearsensor ,(quote-const (car args)))))))
      ;; engine primitives
      ((set-engine-speed)
       (let ((motors (cond
		      ((not (quote? (car args)))
		       (error 'dump-exp "Motors must be constants."))
		      ((not (string? (quote-const (car args))))
		       (error 'dump-exp
			      "Motor list must be a string."))
		      (else (quote-const (car args)))))
	     (type (cond
		    ((var? (cadr args)) 0)
		    ((and (quote? (cadr args)) 
			  (eq? (quote-const (car args)) 'random)) 4)
		    ((quote? (cadr args)) 2)
		    (else 
		     (error 'dump-exp 
			    "Invalid type for set-engine-speed."))))
	     (number (cond
		      ((and (quote? (cadr args))
			    (eq? (quote-const (cadr args)) 'random)) 7)
		      ((and (quote? (cadr args))
			    (>= (quote-const (cadr args)) 0)
			    (<= (quote-const (cadr args)) 7))
		       (quote-const (cadr args)))
		      ((var? (cadr args)) (var-name (cadr args)))
		      (else 
		       (error 'dump-exp
			      "Number out of bounds for set-engine-speed.")))))
	 `((setpower ,motors ,type ,number))))
      ((set-engine-direction)
       (cond
	((not (and (quote? (cadr args))
		   (or (eq? (quote-const (cadr args)) 'forward)
		       (eq? (quote-const (cadr args)) 'backward))))
	 (error 'dump-exp "Set-engine-direction needs forward/backward"))
	((not (quote? (cadr args)))
	 (error 'dump-exp "Second argument to set-engine-direction must be symbol"))
	(else
	 (let ((command (if (eq? (quote-const (cadr args))
				 'forward) 'setfwd 'setrwd))
	       (motors (quote-const (car args))))
	   ;; needs error checking on motors
	   `((,command ,motors))))))
      ((flip-engine-direction)
       (cond
	((not (and (quote? (car args)) 
		   (string? (quote-const (car args)))))
	 (error 'dump-exp
		"Flip-engine-direction requires motor list (string)"))
	(else
	 (let ((motors (quote-const (car args))))
	   `((alterdir ,motors))))))
      ((set-engine-on/off)
       (cond 
	((not (and (quote? (cadr args))
		   (or (eq? (quote-const (cadr args)) 'on)
		       (eq? (quote-const (cadr args)) 'off))))
	 (error 'dump-exp "Set-engine-on/off needs on or off"))
	((not (quote? (cadr args)))
	 (error 'dump-exp 
		"Second argument to set-engine-on/off must be symbol"))
	(else
	 (let ((command (if (eq? (quote-const (cadr args))
				 'on) 'on 'off))
	       (motors (quote-const (car args))))
	   ;; needs error checking on motors
	   `((,command ,motors))))))
      ;; ir primitves
      ;; sound primitives
      ((play-system-sound)
       (cond
	((not (and (quote? (car args)) (symbol? (quote-const (car args)))))
	 (error 'dump-exp "Play-system-sound takes a symbol. See documentation."))
	(else
	 (let* ((type/sym (quote-const (car args)))
		(type/num (case type/sym
			    ((click) 0)
			    ((beep) 1)
			    ((slide-down) 2)
			    ((slide-up) 3)
			    ((sys-error) 4)
			    ((fast-slide-up) 5))))
	   `((playsystemsound ,type/num))))))
      ((play-tone)
       (cond 
	((not (and (quote? (car args)) (number? (quote-const (car args)))))
	 (error 'dump-exp "Play-tone requires a numeric frequency."))
	((not (and (quote? (cadr args)) (number? (quote-const (cadr args)))))
	 (error 'dump-exp "Play-tone requires a numeric duration."))
	(else
	 (let ((freq (quote-const (car args)))
	       (duration (quote-const (cadr args)))
	       (worthless1 (if (or (< freq 1) (> freq 20000))
			       (error 'dump-exp
				      "Frequency out-of-bounds in play-tone")))
	       (worthless2 (if (or (< duration 1) (> duration 255))
			       (error 'dump-exp
				      "Duration out-of-bounds in play-tone"))))
	   `((playtone ,freq ,duration))))))
      ;; timer primitives
      ((clear-timer)
       (cond 	
	((not (and (quote? (car args)) (number? (quote-const (car args)))))
	 (error 'dump-exp "Clear-timer requires a numeric frequency."))
	((or (< (quote-const (car args)) 0) (> (quote-const (car args)) 3))
	 (error 'dump-exp "Clear-timer argument is out of bounds."))
	(else
	 `((cleartimer ,(quote-const (car args)))))))
      ((read-from-timer)
       (cond
	((not (and (quote? (car args)) (number? (quote-const (car args)))))
	 (error 'dump-exp "Read-from-timer takes a numeric timer #"))
	((or (< (quote-const (car args)) 0) (> (quote-const (car args)) 3))
	 (error 'dump-exp "Arg to read-from-timer out of bounds."))
	(else
	 `((setvar ,dest 1 ,(quote-const (car args)))))))
      ;; other primitives
      ((wait)
       (let ((type (cond
		    ((var? (car args)) 0)
		    ((and (quote? (car args)) 
			  (eq? (quote-const (car args)) 'random)) 4)
		    ((quote? (car args)) 2)
		    (else 
		     (error 'dump-exp 
			    "Invalid type for set-engine-speed."))))
	     (number (cond
		      ((and (quote? (car args))
			    (eq? (quote-const (car args)) 'random)) 7)
		      ((and (quote? (car args))
			    (>= (quote-const (car args)) 1)
			    (<= (quote-const (car args)) 32767))
		       (quote-const (car args)))
		      ((var? (car args)) (var-name (car args)))
		      (else 
		       (error 'dump-exp
			      "Number out of bounds for s-e-s.")))))
	 `((wait ,type ,number))))
      )))

(define dump-math-prim-exp
  (lambda (fun args prim dest)
    (let ([command 
	   (case prim
	     ((+) 'sumvar)
	     ((-) 'subvar)
	     ((*) 'mulvar)
	     ((/) 'divvar)
	     ((abs) 'absvar)
	     ((sign) 'sgnvar)
	     ((and) 'andvar)
	     ((or) 'orvar))])
      `(,@(dump-exp (car args) dest)
	,@(dump-exp (cadr args) 0)
	(,command ,dest 0 0)))))

(define task->number
  (lambda (x)
    (case x
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

(define cur-program-tasks '())
(define cur-program-funcs '())

(define get-task-regs
  (lambda (x ls)
    (cond
     ((null? ls) (error 'get-task-regs "System error."))
     ((= x (task-num (car ls)))
      (map var-name (lambda-ids (task-body (car ls)))))
     (else (get-task-regs x (cdr ls))))))

(define get-func-regs
  (lambda (x ls)
    (cond
     ((null? ls) (error 'get-func-regs "System error."))
     ((= x (func-num (car ls)))
      (map var-name (lambda-ids (func-body (car ls)))))
     (else (get-func-regs x (Cdr ls))))))

(define dump-standard-call-exp
  (lambda (fun args dest)
    (cond
     ((task->number (var-name fun)) =>
      (lambda (x) 
	(let ((regs (get-task-regs x cur-program-tasks)))
	  `(,@(apply append (map (lambda (x r) (dump-exp x r)) args regs))
	    (starttask ,(task->number (var-name fun)))))))
     (else 
      (let ((regs (get-func-regs (var-name fun) cur-program-funcs)))
	`(,@(apply append (map (lambda (x r) (dump-exp x r)) args regs))
	  (gosub ,(var-name fun))))))))

(define dump-exp
  (case-lambda 
   [(x) (dump-exp x 0)]
   [(x dest)
    (c-record-case x
      ((quote) (const)
       `((setvar ,dest 2 ,const)))
      ((var) (name top-level?)
       `((setvar ,dest 0 ,name)))
      ((seq) (exp1 exp2)
       (append (dump-exp exp1) (dump-exp exp2)))
      ((if) (test conseq altern)
       (dump-if-exp test conseq altern dest))
      ;; we're done with ids at this point. We just haven't
      ;; taken out the lambda expression yet.
      ((lambda) (ids body) 
       (dump-exp body))
      ((let) (ids vals body)
       (append
	(apply append (map (lambda (id val)
			     (dump-exp val (var-name id)))
			   ids vals))
	(dump-exp body)))
      ((call) (fun args)
       (c-record-case fun
         ((primref) (prim)
          (cond
	   ((member? prim our-primitives)
	    (dump-robot-prim fun args prim dest))
	   (else 
	    (case (primref-prim fun)
	      ((+ - * / abs and or sign)
	       (dump-math-prim-exp fun args prim dest))
	      ((void) `((setvar 0 0 0)))
	      (else
	       (error 'dump-exp 
		      "Scheme primitive ~s not implemented yet. Sorry."
		      (primref-prim fun)))))))
	 (else
	  (cond
	   ((not (var? fun))
	    (error 'dump-exp 
		   "Call requires primitive or variable name."))
	   ((= (length args) 0)
	    (cond 
	     ((task->number (var-name fun)) =>
              (lambda (x) `((starttask ,(task->number (var-name fun))))))
	     (else
	      `((gosub ,(var-name fun))))))
	   ((> (length args) 0)
	    (dump-standard-call-exp fun args dest))))))
	    )]))
      
	      

;		(define our-primitives
;  (list ;; robot primitives
;   'read-from-sensor 'set-engine-speed 'set-engine-direction 'send-message
;   'incoming-message? 'receive-message 'set-engine-on/off 'wait 
;   'clear-tachometer 'flip-engine-direction))

(define build-function
  (lambda (x)
    (let ((num (func-num x))
	  (body (func-body x)))
      `((beginofsub ,num)
	,@(dump-exp (lambda-body body))
	(endofsub)))))

(define build-task
  (lambda (x)
    (let ((num (task-num x))
	  (body (task-body x)))
      `((beginoftask ,num)
	,@(dump-exp (lambda-body body))
	(endoftask)))))

;; needs killing

(define dump
  (lambda (x)
    (let* ((tasks (program-tasks x))
	   (funcs (program-others x))
	   (prepwork (begin
		       (set! cur-program-tasks tasks)
		       (set! cur-program-funcs funcs)))
	   (code (append
		  (apply append (map build-task tasks))
		  (apply append (map build-function funcs)))))
      code)))
