(require "Fonctions/fonctionsVM.lisp")




(defun vm-creation (&optional (nom 'VM) (taille 300000))

	(setf (get nom 'memoire) (make-array taille))
	(setf (get nom 'R0) 0) 
    (setf (get nom 'R1) 0) 
    (setf (get nom 'R2) 0) 
    (setf (get nom 'BP) 10) 
    (setf (get nom 'SP) 10) 
    (setf (get nom 'PC) 100001)
    (setf (get nom 'FP) 10)
    (setf (get nom 'FLT) 0)
    (setf (get nom 'FEQ) 0)
    (setf (get nom 'FGT) 0)
	(setf (get nom 'symboleR) (make-hash-table))
	(setf (get nom 'referenceNR) (make-hash-table))
	(setf (get nom 'exitVM) 0)
	(setf (get nom 'maxStack) 250000)
	(format t "Création réussie de la VM : ~A, de taille : ~A~%" nom taille)
)


(defun vm-exec-inst (nom instr)
	(let ((src (cadr instr))
		  (dest (caddr instr)))
		(case (car instr)
			('MOVE	 (vm-exec-move nom src dest))
			('LOAD	 (vm-exec-load nom src dest))
			('STORE	 (vm-exec-store nom src dest))

			('ADD	 (vm-exec-add nom src dest))
			('MUL	 (vm-exec-mul nom src dest))
			('SUB	 (vm-exec-sub nom src dest))
			('DIV	 (vm-exec-div nom src dest))

			('INCR	 (vm-exec-incr nom src))
			('DECR	 (vm-exec-decr nom src))

			('PUSH	 (vm-exec-push nom src))
			('POP	 (vm-exec-pop nom src))

			('CMP	 (vm-exec-cmp nom src dest))
			('JPG	 (vm-exec-jgt nom src))
			('JEQ	 (vm-exec-jeq nom src))
			('JPP	 (vm-exec-jlt nom src))
			('JGE	 (vm-exec-jge nom src))
			('JPE	 (vm-exec-jle nom src))

			('JMP	 (vm-exec-jmp nom src))
			('JSR	 (vm-exec-jsr nom src))
			('RTN	 (vm-exec-rtn nom))
			('NOP	 (vm-exec-nop nom))
			('HALT	 (vm-exec-halt nom))

			('CONS	 (vm-exec-cons nom src dest))
			('CAR	 (vm-exec-car nom src ))
			('CDR	 (vm-exec-cdr nom src ))
		)
	)
)


(defun vm-run-code (nom)
	
	(loop while (= (get nom 'exitVM) 0) do
		
		(let* ((pc (get-registre nom 'PC)) (instr (get-memoire nom pc)))
		
			(progn (vm-exec-inst nom instr)
				
				(if (= (get-registre nom 'PC) pc)
					(set-registre nom 'PC (+ pc 1))
					nil
				)
			)
		)	
	)
	
	(printemvm "---- Résultat = " (get-registre nom 'R0))
)


(defun vm-load (nom nomfichier &optional (co 100001))
  (with-open-file (fichier nomfichier :direction :input)
    (let ((instructions (read fichier)))
      (vm-load-code nom instructions co)))
  "Succès de chargement"
)

(defun vm-load-code (nom instructions &optional (co 100001))
  
    (loop while (not (null instructions)) do
     
        (let ((instr (car instructions)))
          
            (if (null instr)
                nil
               
                (if (eql 'LABEL (car instr))
                   
                    (vm-exec-charger-symb nom (cadr instr) co)
                  
                    (progn
                      
                        (set-memoire nom co (vm-exec-resoudre-symb nom instr co))
                  
                        (setf co (+ co 1))
                    )
                )
            )
        )
     
        (setf instructions (cdr instructions))
    )
)

(defun printemvm (&rest args)
  (format t "~{~a~^ ~}~%" args)
)
