;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs - Learn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :com.groklogs.learn)

;;; By default, we do not treat "Warnings" as "Criticals".
 (defparameter *treat-warnings-as-critp* nil
   "Should we treat WARN as CRIT")

;;; Training set / test set split. 
(defparameter *training-set-size* 70
  "Percentage of the input data that should be reserved for training.")

 ;;;; We begin by building the current set.

 ;;; If at any time, the timestamp is not 'current', we need to be able to signal it.
 (define-condition time-went-backwards (error)
   ((current-event :initarg :error-event :reader error-event)
    (timestamp :initarg :timestamp :reader error-timestamp)))

 ;;; The current-set of CRIT events.
 (defclass current-set ()
   ((timestamp :initform 0 :accessor current-set-timestamp)
    (crits :initform nil :accessor current-set-crits)))

 (defgeneric update-set (set timestamp host param status)
   (:documentation "Update the current set based on the current status of the host/parameter"))

 (defmethod update-set :before ((set current-set) timestamp host param status)
   (cond ((< timestamp (current-set-timestamp set))
	  (error 'time-went-backwards 
		 :error-event (list timestamp host param status)
		 :timestamp (current-set-timestamp set)))
	 ((> timestamp (current-set-timestamp set))
	  (setf (current-set-timestamp set) timestamp))))

 (defmethod update-set ((set current-set) timestamp host param (status (eql #\C)))
   (declare (ignore timestamp status))
   (let ((crit-events (current-set-crits set)))
     (if (member `(,host ,param) crit-events :test #'equal)
	 crit-events
	 (push `(,host ,param) (current-set-crits set)))))

 (defmethod update-set ((set current-set) timestamp host param (status (eql #\O)))
   (declare (ignore timestamp status))
   (let ((crit-events (current-set-crits set)))
     (if (member `(,host ,param) crit-events :test #'equal)
	 (progn
	   (when (> *debug* 0)
	     (format t "~&~a Went OK!~%" `(,host ,param)))
	   (setf (current-set-crits set)
		 (delete (list host param) crit-events :test #'equal))
	 crit-events))))

 (defmethod update-set ((set current-set) timestamp host param (status (eql #\W)))
   "We treat a WARN as a CRIT if *treat-warnings-as-critp* is true. We never treat WARNS as OK."
   (declare (ignore timestamp status))
   (let ((crit-events (current-set-crits set)))
     (if (and *treat-warnings-as-critp*
	      (not (member `(,host ,param) crit-events :test #'equal)))
	 (push `(,host ,param) (current-set-crits set))
	 crit-events)))

 ;;; Populating the current-set.

 (defun build-current-set ()
   #.(locally-enable-sql-reader-syntax)
   (let ((set (make-instance 'current-set)))
     (do-query ((timestamp host param status)
		[select [*] :from [alerts_ordered]])
       (when (> timestamp (current-set-timestamp set))
	 (execute-command
	  (format nil "insert into currentset values (~a, \"~a\")"
		  timestamp ; this is not (current-set-timestamp set) because it starts as 0.
		  (prin1-to-string (current-set-crits set)))))
       (update-set set timestamp host param (coerce status 'character))))
   #.(locally-disable-sql-reader-syntax))

;;; The current-set is split into 2, one to train with and one to test with. By default, we use
;;; a 70:30 split, as defined by *training-set-size*. We create two views on the CURRENTSET
;;; table, one for training and one for testing.
(defun partition-data (&optional (split *training-set-size*))
  "Partition the input data into a training and a test set."
  #.(locally-enable-sql-reader-syntax)
  (let* ((end-time (car (select [max [time]] :from [currentset] :flatp t)))
	 (start-time (car (select [min [time]] :from [currentset] :flatp t)))
	 (boundary (+ start-time
		      (round (/ (* (- end-time start-time) split)
				100)))))
    (format t "~&Splitting at time: ~a" boundary)
    (execute-command 
     "create view currentset_training as 
      select * from currentset where time<=1296533899 order by time")
    (execute-command
     "create view currentset_test as 
      select * from currentset where time>1296533899 order by time")
    (format t "~&View for training: ~a~&View for testing: ~a~%"
	    "CURRENTSET_TRAINING" "CURRENTSET_TEST")
	    
  #.(locally-disable-sql-reader-syntax)))

;;; Creating a nodeparam string.
(defun make-nodeparam (nodeid paramid)
  (format nil "~2,'0d~2,'0d" nodeid paramid))

;;; Datapoints - We collapse all successive critical alerts for a node+service into one time
;;; slice. The datapoint class stores the timestamp when the alert went from CRIT to OK, i.e
;;; when it disappeared from the critical set, node and parameter ids and the set of features
;;; for the positive and negetive example for that timeslice. 
(defclass datapoint ()
  ((timestamp :initarg :timestamp :accessor datapoint-timestamp)
   (nodeid :initarg :nodeid :reader datapoint-nodeid)
   (paramid :initarg :paramid :reader datapoint-paramid)
   (positive-example :initarg :positive-example :accessor datapoint-positive)
   (negetive-example :initform nil :accessor datapoint-negetive)))

;;; Initialising and updating datapoints.

(defgeneric update-datapoint (datapoint timestamp crits output-class)
  (:documentation 
"Update the datapoint example with the correct set of criticals"))

;;; We need to update the timestamp only when we encounter the negetive example after the
;;; sequence of positive examples.
(defmethod update-datapoint :before ((dp datapoint) timestamp crits (class (eql 0)))
  (declare (ignore crits class))
  (let ((ts (datapoint-timestamp dp)))
    (if (< timestamp ts)
	(error 'time-went-backwards :timestamp timestamp)
	(setf (datapoint-timestamp dp) timestamp))))

(defmethod update-datapoint ((dp datapoint) timestamp crits (class (eql 0)))
  (declare (ignore timestamp class))
  (setf (datapoint-negetive dp) crits))

(defmethod update-datapoint :after ((dp datapoint) timestamp crits (class (eql 0)))
  "Update the database."
  (declare (ignore timestamp crits))
  (let* ((timestamp (datapoint-timestamp dp))
	 (nodeid (datapoint-nodeid dp))
	 (paramid (datapoint-paramid dp))
	 (positives (prin1-to-string (datapoint-positive dp)))
	 (negetives (prin1-to-string (datapoint-negetive dp)))
	 (nodeparam (make-nodeparam nodeid paramid)))
    (execute-command 
     (format nil "insert into datapoints values (~a, \"~a\", \"~a\", ~a)"
	     timestamp nodeparam positives 1))
    (execute-command 
     (format nil "insert into datapoints values (~a, \"~a\", \"~a\", ~a)"
	   timestamp nodeparam negetives 0))))



(defmethod update-datapoint ((dp datapoint) timestamp crits (class (eql 1)))
  (declare (ignore timestamp class))
  (let ((new-crits (union crits (datapoint-positive dp) :test #'equal)))
    (setf (datapoint-positive dp) new-crits)))

;;; Constructor for datapoints.
(defun make-datapoint (timestamp nodeid paramid crits)
  (make-instance 'datapoint 
		 :timestamp timestamp
		 :nodeid nodeid
		 :paramid paramid
		 :positive-example crits))

;;; Building all datapoints.
(defun build-datapoints ()
  #.(locally-enable-sql-reader-syntax)
  (let ((object-buffer (make-array 1 :fill-pointer 0 :adjustable t)))
    (do-query ((timestamp criticals)
	       [select [*] :from [currentset]])
      (let ((crits (read-from-string criticals))
	    (object-list nil))
	;; First update existing objects.
	(loop 
	   for dp-object across object-buffer while dp-object do ;dont remove 'while dp-obj...
	     (let ((event (list (datapoint-nodeid dp-object) (datapoint-paramid dp-object))))
	       (push event object-list)
	       (if (member event crits :test #'equal)
		   (update-datapoint dp-object 
				     timestamp 
				     (remove event crits :test #'equal)
				     1)
		   (progn
		     (update-datapoint dp-object timestamp crits 0)
		     (delete dp-object object-buffer :test #'equal)))))
	;; Add objects for new crits.
	(dolist (crit crits)
	  (unless (member crit object-list :test #'equal)
	    (vector-push-extend (make-datapoint timestamp
						(first crit)
						(second crit)
						(remove crit crits :test #'equal))
				object-buffer))))))
  #.(locally-disable-sql-reader-syntax))

;;; Building the feature space.

;; This creates the elements of the featurespace by using UNION on all the datapoint criticals
;; for a particular node and parameter. The elements are stored in a feature vector so that it
;; may be referrenced later.
;; MAKE-FEATUREINDEX will error out if called with the same node-param twice because it is the
;; table's primary key. 
(defun make-featureindex (node-param)
  #.(locally-enable-sql-reader-syntax)
  (let ((features nil))
    (do-query ((crits)
	       [select [crits] :from [datapoints] :where [= [nodeparam] node-param]])
      (setf features (union features (read-from-string crits) :test #'equal)))
    (execute-command (format nil
			     "insert into featureindex values (\"~a\", \"~a\")"
			     node-param
			     (prin1-to-string (coerce features 'vector))))
    (coerce features 'vector))
  #.(locally-disable-sql-reader-syntax))


;; Build a row of featurespace for the node-param. This takes a list of crits for the
;; node-param, output-class and the feature-vector and returns a list of 1' and 0's that is the
;; datapoint-class and datapoint-features for that row.
(defun make-datapoint-features-class (crits class fvector)
  (let ((output nil))
    (loop 
       for element across fvector
       do
	 (if (member element crits :test #'equal)
	     (push 1 output)
	     (push 0 output))
       finally (return (cons (nreverse output) class)))))

;;; Make the featurespace for a node and parameter.
(defun make-featurespace (node-param)
  #.(locally-enable-sql-reader-syntax)
  (let ((fvector (make-featureindex node-param))
	(result nil))
    (do-query ((crits class)
	       [select [crits] [class] :from [datapoints] :where [= [nodeparam] node-param]])
      (push (make-datapoint-features-class (read-from-string crits)
					   class
					   fvector)
	    result)))
  #.(locally-disable-sql-reader-syntax))

;;; Building the complete featurespace for all nodes/parameters
(defun build-complete-featurespaces ()
  (dolist (node-param (remove-duplicates 
		       (apply #'append
			      (query "select nodeparam from datapoints"))
			      :test #'equal))
    (let ((feature-space (make-featurespace node-param)))
      (execute-command 
       (format nil "insert into featurespace values (\"~a\", \"~a\")"
	       node-param
	       (prin1-to-string feature-space))))))




;;; Selectors for the names of nodes and parameters.
(defun node-name (nid)
  (caar (query (format nil "select nname from nodes where nid=~a"  nid))))

(defun param-name (pid)
  (caar (query (format nil "select pname from parameters where pid=~a" pid))))

(defun node-and-param-name (fvector index)
  (let* ((node-param-ids 
	 (elt fvector (1- (abs index)))) ; features use 1 based index, fvectors are 0 based.
	(nname (node-name (first node-param-ids)))
	(pname (param-name (second node-param-ids))))
    (if (minusp index) ; The DNF code can negate features!
	(concatenate 'string "-" nname ":" pname)
	(concatenate 'string  nname ":" pname))))


(defun dependency-names (node-param dnf-expr)
  (let ((fvector (read-from-string
		  (caar 
		  (query (format nil "select fvector from featureindex where nodeparam=\"~a\""
				 node-param)))))
	(result nil))
    (dolist (conjunction dnf-expr)
      (when conjunction
	(let ((partial nil))
	  (dolist (feature conjunction)
	    (push (node-and-param-name fvector feature) partial))
	  (push partial result))))
    result))

