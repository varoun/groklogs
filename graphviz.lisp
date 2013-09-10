;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs - Graphviz interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :com.groklogs.learn)

(defun dot-name (str)
  "Graphviz reacts badly to characters that are not alphanumeric or underbars. Replace all non
alphanumeric characters with underbars."
  (substitute-if-not #\_ #'alphanumericp str))
#|
LEARN> (dot-name "www.yahoo.com:ysar")
"www_yahoo_com_ysar"
LEARN> 
|#

(defun node-param->dot-nodes (nodeid paramid)
  (let ((names (concatenate 'string (node-name nodeid) ":" (param-name paramid))))
    (format nil "~a[label=\"~a\"];" 
	    (make-nodeparam nodeid paramid)
	    (dot-name names))))

(defun number-of-ands (dnf-expr)
  "Compute the number of conjunctions in the DNF expression."
  (loop
     for conjunction in dnf-expr 
     unless (null conjunction)
     when (> (length conjunction) 1)
     count conjunction into total
     finally (return total)))
#|
LEARN> (number-of-ands '((-4 -7 -8) (8 -3 -4 -7) (5) (4 2) (3) (1)))
3
LEARN> 
|#

(defun number-of-nots (dnf-expr)
  (let ((features (apply #'append dnf-expr)))
    (loop for feature in features
	 when (minusp feature)
	 count feature into total
	 finally (return total))))
#|
LEARN> (number-of-nots '((-4 -7 -8) (8 -3 -4 -7) (5) (4 2) (3) (1)))
6
LEARN> 
|#

(defun and->dot-nodes (index)
  (format nil "~a[label=\"~a\"];" 
	  (concatenate 'string "AND" "_" (prin1-to-string index))
	  "AND"))
#|
LEARN> (and->dot-nodes 1)
"AND_1[label=\"AND\"];"
LEARN> 
|#

(defun not->dot-nodes (index)
  (format nil "~a[label=\"~a\"];" 
	  (concatenate 'string "NOT" "_" (prin1-to-string index))
	  "NOT"))
#|
LEARN> (not->dot-nodes 2)
"NOT_2[label=\"NOT\"];"
LEARN> 
|#

(defun or->dot-nodes ()
  (format nil "~a[label=\"~a\"];" 
	  "OR" "OR"))

(defun dependency->dot-nodes (nodeid paramid dnf-str)
  (let* ((dnf-expr (read-from-string dnf-str))
	 (nodeparam (make-nodeparam nodeid paramid))
	 (fvector (read-from-string
		   (caar 
		    (query (format nil 
				   "select fvector from featureindex where nodeparam=\"~a\""
				   nodeparam)))))
	 (total-ands (number-of-ands dnf-expr))
	 (total-nots (number-of-nots dnf-expr))
	 (result nil))
    (push (node-param->dot-nodes nodeid paramid) result)
    (dotimes (n total-ands)
      (push (and->dot-nodes n) result))
    (dotimes (n total-nots)
      (push (not->dot-nodes n) result))
    (push (or->dot-nodes) result)
    (dolist (index (remove-duplicates (mapcar #'abs (apply #'append dnf-expr))))
      (let ((node-param-ids (elt fvector (1- index))))
	(push (node-param->dot-nodes (first node-param-ids) (second node-param-ids))
	      result)))
    result))
      
	   