
(defun cout (code)
  (getline code 1)
)


(defun getline (code line)
  (if (null code)
    NIL
    (progn
      (print (string-concat (write-to-string line) " : " (write-to-string (car code))))
      (getline (cdr code) (+ 1 line))))
)


(defun printem (&rest args)
  (format t "~{~a~^ ~}~%" args)
)




(setq comp-if-i 0)


(defun generate-label (base)
 
    (incf comp-if-i)

    (intern (concatenate 'string base (write-to-string comp-if-i))))



(defun compile-section (code env label)
    (append 
      
        (comp-expr code env)

        `((JMP (LABEL ,label)))
    ))



(defun comp-if (code &optional env)
 
    (let ((then-label (generate-label "THEN"))
          (else-label (generate-label "ELSE"))
          (end-label (generate-label "ENDIF")))
        (append 
         
            (comp-expr (second code) env)
           
            `((CMP (LIT 0) R0))
       
            `((JEQ (LABEL ,else-label)))
         
            (compile-section (third code) env end-label)
          
            `((LABEL ,else-label))
           
            (compile-section (fourth code) env end-label)
         
            `((LABEL ,end-label))
        )
    )
)



(defun comp-var (var  &optional env)
    
	(let ((lib (assoc var env))) 
	    
		(if lib
			`( (MOVE FP R0)               
			   (SUB (LIT ,(cdr lib)) R0)  
			   (LOAD R0 R0) )             

			`((MOVE (VAR ,var) RO)) 
		)
	)
)





(defun comp-defun (code &optional env) 
  
	(let ((positionPile 0)) 
		(progn
			(map
				'list
				(lambda (param)
					(progn 
						(setf positionPile (+ positionPile 1)) 
						(setf env (acons param positionPile env)) 
					)
				)
				(caddr code) 
			)
			(append
				`((JMP (LABEL ,(intern (string-concat "END" (string (cadr code))))))) 
				`((LABEL ,(cadr code))) 
				(comp-expr (cadddr code) env) 
				`((MOVE FP R1))
				`((ADD (LIT 4) R1)) 
				'((MOVE R1 SP)) 
				`((RTN))
				`((LABEL ,(intern (string-concat "END" (string (cadr code))))))
			)
		)
	)
)


(defun comp-expr (expr &optional env)
  (if (consp expr) 
    (let ((car-expr (car expr))) 
        (cond
            ((eq 'if car-expr) (comp-if expr env)) 
            ((eq 'defun car-expr) (comp-defun expr env)) 
            ((eq 'halt car-expr) `((HALT))) 
            ((eq 'nop car-expr) `((NOP))) 
            (t (comp-call expr env)))) 
    (if (constantp expr) 
        (comp-cons expr)  
        (if (symbolp expr) 
            (comp-var expr env) 
            (error "Expression ~s mal formée" expr)))))  





(defun comp-list (vlist)

  (if (null vlist)
    NIL

    (append
     
      (comp-expr (car vlist))
 
      (comp-list (cdr vlist)))))




(defun comp-cons (cons)
  
	`((MOVE (LIT ,cons) R0))
)


(setf comp-i 0)


(defun comp-call (code  &optional env) 
   
    (append  
        (apply 'append  
            (map 'list  
                (lambda (param) 
                    (append 
                        (comp-expr param env)  
                        `((PUSH R0))  
                    )
                )
                (reverse (cdr code))  
            )
        )
        `((MOVE FP R1)) 
        `((MOVE SP FP))  
        `((PUSH (LIT ,(list-length (cdr code))))) 
        `((MOVE SP R2)) 
        `((SUB (LIT ,(+ (list-length (cdr code)) 1)) R2))  
        `((PUSH R2)) 
        `((PUSH R1))  
        (comp-primitive-call (car code))  
        `((POP R1))  
        `((POP R2)) 
        `((MOVE R1 FP)) 
        `((MOVE R2 SP))  
    )
)


(defun comp-primitive-call (functionName)

	(cond 
		((member functionName '(+ - * /))
			(append 
		
				'(
					(MOVE FP R0) 
					(SUB (LIT 1) R0)
					(LOAD R0 R0) 
					(MOVE FP R1) 
					(SUB (LIT 2) R1)
					(LOAD R1 R1)   
				)
			  
				(case functionName 
					('+ '((ADD R1 R0)))
					('- '((SUB R1 R0)))
					('* '((MUL R1 R0)))
					('/ '((DIV R1 R0)))
				)
			)
		)
	
		((member functionName '(= <= < > >=))
			(setf comp-i (+ comp-i 1))
		   
			(let ((finCond (intern (string-concat (string "FINCOMP") (write-to-string comp-i)))))
				(append 
		       
					'(
						(MOVE FP R0) 
						(SUB (LIT 1) R0)
						(LOAD R0 R0) 
						(MOVE FP R1)  
						(SUB (LIT 2) R1)
						(LOAD R1 R1) 
						(CMP R0 R1)  
						(MOVE (LIT 1) R0)
					)
		          
					(case functionName
						('= `((JEQ (LABEL ,finCond))))
						('<= `((JPE (LABEL ,finCond))))
						('< `((JPG (LABEL ,finCond))))
						('> `((JPP (LABEL ,finCond))))
						('>= `((JGE (LABEL ,finCond))))
					)
					'((MOVE (LIT 0) R0))
					`((LABEL ,finCond)) 
				)
			)
		)
		(t `((JSR (LABEL ,functionName))))
	)
)




