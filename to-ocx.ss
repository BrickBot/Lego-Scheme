;;   
;;   Lego/Scheme Compiler
;;   Version 0.5.2
;;
;;   Date:
;;     May 05, 1999
;;
;;   Authors:
;;     K. Klipsch
;;     A. C. Wick
;;     M. t. Wagner
;;   
;;   file:
;;     toocx.ss
;;
;;   to->ocx takes two arguments.
;;     | the output list from the main compiler and
;;     | the name of output file 
;;     | eg. (to->ocx compiler_output "program.spi")
;;
;;     It parses the list (which is a simple list of lists) 
;;     into the proper format for Spirit.ocx and outputs it
;;     to the designated output file. It is writen in such 
;;     that if it needs to have spirit.ocx primitives added
;;     to it, they can be by the addition of a additional cond
;;     clause.


;;; Helper functions-----------------------------------------
(define cadddar
  (lambda (ls)
    (car (cdr (cdr (cdr (car ls)))))))

(define caddddar
  (lambda (ls)
    (car (cdr (cdr (cdr (cdr (car ls))))))))

(define cadddddar
  (lambda (ls)
    (car (cdr (cdr (cdr (cdr (cdr (car ls)))))))))


;;; Main function--------------------------------------------
(define to->ocx
 (lambda (inputls outname)
  (let ([out-file (open-output-file outname)])
    (letrec ([ocx-it  (lambda (inls)
			  (cond 
     			   ((null? inls)  
			    (begin
			      (close-output-port out-file `replace)
			      (void)))

			   ((eq? (caar inls) 'setfwd)
			    (begin 
   			      (display #\. out-file)
			      (display "SetFwd" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'setrwd)
			    (begin 
			      (display #\. out-file)
			      (display "SetRwd" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'alterdir)
			    (begin 
			      (display #\. out-file)
			      (display "AlterDir" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'beginoftask)	
			    (begin 
			      (display #\. out-file)
			      (display "BeginOfTask" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'setpower)
			    (begin
			      (display #\. out-file)
			      (display "SetPower" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'setvar)
			    (begin
			      (display #\. out-file)
			      (display "SetVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'sumvar)
			    (begin
			      (display #\. out-file)
			      (display "SumVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))
           
			   ((eq? (caar inls) 'subvar)
			    (begin
			      (display #\. out-file)
			      (display "SubVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'divvar)
			    (begin
			      (display #\. out-file)
			      (display "DivVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'mulvar)
			    (begin
			      (display #\. out-file)
			      (display "MulVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'orvar)
			    (begin
			      (display #\. out-file)
			      (display "OrVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'andvar)
			    (begin
			      (display #\. out-file)
			      (display "AndVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'absvar)
			    (begin
			      (display #\. out-file)
			      (display "AbsVar" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'on)
			    (begin 
			      (display #\. out-file)
			      (display "On" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))
			   
			   ((eq? (caar inls) 'off)
			    (begin 
			      (display #\. out-file)
			      (display "Off" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'wait)
			    (begin 
			      (display #\. out-file)
			      (display "Wait" out-file)
			      (display #\( out-file)
			      (display (cadar inls) out-file)
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'starttask)
			    (begin 
			      (display #\. out-file)
			      (display "StartTask" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'stoptask)
			    (begin 
			      (display #\. out-file)
			      (display "StopTask" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'clearsensorvalue)
			    (begin 
			      (display #\. out-file)
			      (display "ClearSensorValue" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'setsensortype)
			    (begin 
			      (display #\. out-file)
			      (display "SetSensorType" out-file)
			      (display #\( out-file)
			      (display (cadar inls) out-file)
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'setsensormode)
			    (begin
			      (display #\. out-file)
			      (display "SetSensorMode" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'playtone)
			    (begin 
			      (display #\. out-file)
			      (display "PlayTone" out-file)
			      (display #\( out-file)
			      (display (cadar inls) out-file)
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'playsystemsound)
			    (begin 
			      (display #\. out-file)
			      (display "PlaySystemSound" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'else)
			    (begin 
			      (display #\. out-file)
			      (display "Else" out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'gosub)
			    (begin 
			      (display #\. out-file)
			      (display "GoSub" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'beginofsub)
			    (begin 
			      (display #\. out-file)
			      (display "BeginOfSub" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'cleartimer)
			    (begin 
			      (display #\. out-file)
			      (display "ClearTimer" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))


			   ((eq? (caar inls) 'endoftask)
			    (begin 
			      (display #\. out-file)
			      (display "EndOfTask" out-file)
			      (newline out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'endofsub)
			    (begin 
			      (display #\. out-file)
			      (display "EndOfSub" out-file)
			      (newline out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))


			   ((eq? (caar inls) 'endif)
			    (begin 
			      (display #\. out-file)
			      (display "EndIf" out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))

			   ((eq? (caar inls) 'if)
			    (begin
			      (display #\. out-file)
			      (display "If" out-file)
			      (display #\( out-file)
			      (write (cadar inls) out-file) 
			      (display #\, out-file)
			      (display (caddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddar inls) out-file)
			      (display #\, out-file)
			      (display (caddddar inls) out-file)
			      (display #\, out-file)
			      (display (cadddddar inls) out-file)
			      (display #\) out-file)
			      (newline out-file)
			      (ocx-it (cdr inls))))
			   
			   (else 
			    (error 
			     `to->ocx 
			     "Malformed list, cannot convert to .ocx"))))])
      (ocx-it inputls)))))




