;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs - Learn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :com.groklogs.learn)

 (defparameter *treat-warnings-as-critp* nil
   "Should we treat WARN as CRIT")

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
		  (current-set-timestamp set)
		  (prin1-to-string (current-set-crits set)))))
       (update-set set timestamp host param (coerce status 'character))))
   #.(locally-disable-sql-reader-syntax))








