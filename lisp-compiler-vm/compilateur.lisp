(require "Fonctions/fonctionsCOMP.lisp")


(defun compilation (fichier &optional output)
	(printem "#### Compilation du fichier  " fichier)
	(let ((file (open fichier)) (code '()) (bytecode '())) 
		(loop for expr = (read file nil) while expr do		 
			(setf code (append code (list expr)))
		)
		(close file)
		(setf bytecode (comp-list (append code '((HALT)))))	    
		(if (not (null output))
			(with-open-file (str (string-concat "ASMFILES/" output)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  			(format str (write-to-string bytecode)))
		)
		
		
	)
)
