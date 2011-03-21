;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs - Initialisation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *files* 
  '(#p"/home/varoun/Code/groklogs-ng/packages.lisp"
    #p"/home/varoun/Code/groklogs-ng/utils.lisp"
    #p"/home/varoun/Code/groklogs-ng/schema.lisp"
    #p"/home/varoun/Code/groklogs-ng/preprocess.lisp"
    #p"/home/varoun/Code/groklogs-ng/learn.lisp"
    #p"/home/varoun/Code/groklogs-ng/dnf.lisp"))

(defun initialise-system ()
  (dolist (file *files*)
    (format t "~&Loading: ~a~%" file)
    (load file)))
