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