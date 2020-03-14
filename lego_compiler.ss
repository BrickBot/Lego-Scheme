;; Lego Compiler - Main Driver for Compiler
;; Version 0.5.2
;; Copyleft (C) 1999 by Kasey Klipsch, Mitchell Wagner, and Adam Wick

(printf "LEGO/Scheme Version 0.5.2~n")
(printf "Copyleft (C) 1999 by K. Klipsch, M. Wagner, and A. C. Wick~n~n")

(printf "Loading shared structures/functions ...~n")
(load "shared.ss")
(printf "Loading front end ...~n")
(load "front-end.ss")
;(printf "Loading variable linking pass ...~n")
;(load "link-same-variables.ss")
(printf "Loading subroutine builder ...~n")
(load "yank-subroutines.ss")
(printf "Loading register allocation pass ...~n")
(load "numerize-variables.ss")
(printf "Loading back end ...~n")
(load "back-end.ss")
(printf "Loading final printing functions ...~n")
(load "to-ocx.ss")
(printf "~nBy using this software, you are agreeing to the terms of the~n")
(printf "License included the file 'LICENSE'. Please read this file if~n")
(printf "you have not done so allready.~n") 
(printf "~nUSAGE: (lego-compile <input file> <output file>)~n~n")


(define lego-compile
  (lambda (file . outfile)
    (let* (
;          (x (cons 'begin (read-file file)))
;	   (x (expand x))
	   (x (recordify file))
;	   (x (link-same-variables x '()))
	   (x (yank-subroutines x))
	   (x (numerize-variables x))
	   (x (dump x))
	   (x (to->ocx x (if (null? outfile) "test.ocx" (car outfile))))
	   )
      x)))
	   


