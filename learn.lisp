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
	   (format t "~&~a Went OK!~%" `(,host ,param))
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
  "Stub for the after-method. We update the database here!"
  (declare (ignore dp timestamp crits class))
  (values))

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
