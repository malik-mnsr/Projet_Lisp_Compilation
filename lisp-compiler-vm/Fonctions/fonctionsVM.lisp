

(defun get-taille-memoire (nom)
	(array-total-size (get nom 'memoire))
)

(defun get-memoire (nom adresse)
  (let* ((taille (get-taille-memoire nom))
         (memoire (get nom 'memoire)))
    (if (>= adresse taille)
        (error "get-memoire : adresse ~s hors limites" adresse)
        (or (aref memoire adresse) 0)))
)

(defun set-memoire (nom adresse valeur)
  (let* ((taille (get-taille-memoire nom))
         (memoire (get nom 'memoire)))
    (if (>= adresse taille)
        (error "set-memoire : adresse ~s hors limites" adresse)
        (setf (aref memoire adresse) valeur))))



(defun get-registre (nom registre)
  (let ((valeur (get nom registre)))
    (if (null valeur)
        (error "get-registre : registre ~s incorrect" registre)
        valeur)))


(defun set-registre (nom registre valeur)
  (let ((ancienne-valeur (get nom registre)))
    (if (null ancienne-valeur)
        (error "set-registre : registre ~s incorrect" registre)
        (setf (get nom registre) valeur))))



(defun setSymbole (nom symb adresse)
	(setf (gethash symb (get nom 'symboleR)) adresse)
)

(defun getSymbole (nom symb)
	(gethash symb (get nom 'symboleR))
)

(defun isSymboleSet (nom symb)
	(if (getSymbole nom symb)
		t
		nil
	)
)


(defun setReferenceNR (nom ref adresse) 
  (if (isReferenceSet nom ref)
      (push adresse (gethash ref (get nom 'referenceNR)))
      (setf (gethash ref (get nom 'referenceNR)) (list adresse))))


(defun getReferenceNR (nom ref)
	(gethash ref (get nom 'referenceNR))
)

(defun isReferenceSet (nom ref)
	(if (getReferenceNR nom ref)
		t
		nil
	)
)


(defun is-litteral (arg)
	(and (consp arg) (eql (car arg) 'LIT))
)



(defun vm-exec-move (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (cadr src))
      (set-registre nom dest (get-registre nom src)))
	  )


(defun vm-exec-load (nom src dest)
  (if (is-litteral src)
      (vm-exec-move nom `(LIT ,(get-memoire nom src)) dest)
      (vm-exec-move nom `(LIT ,(get-memoire nom (get-registre nom src))) dest))
	  )




(defun vm-exec-store (nom dest src)
  (if (is-litteral src)
      (set-memoire nom src (get-registre nom arg))
      (set-memoire nom (get-registre nom src) (get-registre nom dest)))
	  )



(defun vm-exec-add (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (+ (get-registre nom dest) (cadr src)))
      (set-registre nom dest (+ (get-registre nom dest) (get-registre nom src)))))


(defun vm-exec-sub (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (- (get-registre nom dest) (cadr src)))
      (set-registre nom dest (- (get-registre nom dest) (get-registre nom src)))))

(defun vm-exec-mul (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (* (get-registre nom dest) (cadr src)))
      (set-registre nom dest (* (get-registre nom dest) (get-registre nom src)))))



(defun vm-exec-div (nom src dest)
  (if (is-litteral src)
        (cond   ((= (get-const nom src) 0) (error "vm-exec-div : div par 0 "))
                 (t (set-registre nom dest (/ (get-registre nom dest) (cadr src))) )   
        )
        (cond   ((= (get-registre nom src) 0) (error "vm-exec-div : div par 0 "))
                 (t (set-registre nom dest (/ (get-registre nom dest) (get-registre nom src))) ) 
        )      
    )
)


(defun vm-exec-incr (nom dest)

  (set-registre nom dest (+ (get-registre nom dest) 1)))


(defun vm-exec-decr (nom dest)
  (set-registre nom dest (- (get-registre nom dest) 1)))




(defun vm-exec-push (nom src)
  (if (> (get-registre nom 'SP) (get-registre nom 'maxStack))
		(error "vm-exec-push : depassementpile")
		(progn
			(if (is-litteral src)
				(set-memoire nom (get-registre nom 'SP) (cadr src))
				(set-memoire nom (get-registre nom 'SP) (get-registre nom src)))
			(set-registre nom 'SP (+ (get-registre nom 'SP) 1))
		)
	)
)


(defun vm-exec-pop (nom dest)
  (if (<= (get-registre nom 'SP) (get-registre nom 'BP))
      (error "vm-exec-pop : pile vide")
      (progn 
        (set-registre nom 'SP (- (get-registre nom 'SP) 1))
        (set-registre nom dest (get-memoire nom (get-registre nom 'SP)))
        )
    )
  )



(defun is-label (arg)
	(and (consp arg) (eql (car arg) 'LABEL))
)



(defun vm-exec-jmp (nom label)
  (if (integerp label) 
      (set-registre nom 'PC label) 
      (error "vm-exec-jmp : ~s n'est pas une adresse" label)))


(defun vm-exec-jsr (nom label)
  (set-memoire nom (get-registre nom 'SP) (+ (get-registre nom 'PC) 1)) 
	(set-registre nom 'SP (+ (get-registre nom 'SP) 1))
  	(vm-exec-jmp nom label) 
  )


(defun vm-exec-rtn (nom)
	(set-registre nom 'SP (- (get-registre nom 'SP) 1)) 
	(vm-exec-jmp  nom (get-memoire nom (get-registre nom 'SP))) 
)

(defun vm-exec-cmp (nom src dest)
  (let* ((src-content (if (is-litteral src) (cadr src) (get-registre nom src)))
         (dest-content (get-registre nom dest)))

    (cond
      ((eql src-content dest-content) 
       (set-registre nom 'FEQ 1)     
       (set-registre nom 'FGT 0)
       (set-registre nom 'FLT 0))
      ((< src-content dest-content) 
       (set-registre nom 'FEQ 0)
       (set-registre nom 'FGT 0)
       (set-registre nom 'FLT 1))   
      (t                            
       (set-registre nom 'FEQ 0)    
       (set-registre nom 'FGT 1)    
       (set-registre nom 'FLT 0))))
)




(defun vm-exec-jgt (nom label)
  (if (= (get-registre nom 'FGT) 1)
      (vm-exec-jmp nom label)))


(defun vm-exec-jge (nom label)
  (if (or (= (get-registre nom 'FGT) 1) (= (get-registre nom 'FEQ) 1))
      (vm-exec-jmp nom label)))


(defun vm-exec-jlt (nom label)
  (if (= (get-registre nom 'FLT) 1)
      (vm-exec-jmp nom label)))


(defun vm-exec-jle (nom label)
  (if (or (= (get-registre nom 'FLT) 1) (= (get-registre nom 'FEQ) 1))
      (vm-exec-jmp nom label)))

(defun vm-exec-jeq (nom label)
  (if (= (get-registre nom 'FEQ) 1)
      (vm-exec-jmp nom label)))



(defun vm-exec-halt (nom)
  (set-registre nom 'exitVM 1))


(defun vm-exec-cons (nom src dest)
  (set-registre nom dest (cons (get-registre nom src) (get-registre nom dest)))) 

(defun vm-exec-car (nom arg)
  (set-registre nom arg (car (get-registre nom arg))))


(defun vm-exec-cdr (nom arg) 
  (set-registre nom arg (cdr (get-registre nom arg))))




(defun vm-exec-resoudre-symb (nom instr co)

  (if (or (eql 'JMP (car instr))
          (eql 'JSR (car instr))
          (eql 'JPG (car instr))
          (eql 'JEQ (car instr))
          (eql 'JPP (car instr))
          (eql 'JGE (car instr))
          (eql 'JPE (car instr)))

     
      (if (is-label (cadr instr))

         
          (if (isSymboleSet nom (cadadr instr))

           
              (cons (car instr) (list (getSymbole nom (cadadr instr))))

            
              (progn
                (setReferenceNR nom (cadadr instr) co)
                instr
                )
              )

         
          instr
          )

     
      instr
    )
  )



(defun vm-exec-charger-symb (nom symb co)
 
  (if (isSymboleSet nom symb)
    
      (error "vm-exec-charger-symb : le symbole existe déjà")
      
     
      (progn
        
        (setSymbole nom symb co)

        
        (vm-exec-resoudre-refNR nom symb)
        )
   )
  )



(defun vm-exec-resoudre-refNR (nom symb)
  (if (isReferenceSet nom symb)
      (map 'list
           (lambda (co) (set-memoire nom co `(,(car (get-memoire nom co)) ,(getSymbole nom symb))))
           (getReferenceNR nom symb))
    )
  )


