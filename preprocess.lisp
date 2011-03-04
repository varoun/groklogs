;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Groklogs - Preprocessing logs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :com.groklogs.preprocess)

;;;; Debug level
(defparameter *debug* 0)

;;;; The REGEX parsers for the time, hostname, parameter and status.

(defvar *nagios-hostname-regex* 
  (cl-ppcre:create-scanner "snode=[a-z]*!([a-z0-9\.]*)"))
(defvar *nagios-parameter-regex* 
  (cl-ppcre:create-scanner "\\bservice=([a-z\-]*)\\b"))
(defvar *nagios-timestamp-regex* 
  (cl-ppcre:create-scanner "\\[([0-9]*)\\]"))
(defvar *nagios-status-regex* 
  (cl-ppcre:create-scanner "OK|ok|WARN|CRIT|UP"))

;;;; Objects for holding the indices for the hostnames and parameters.
(defparameter *hostnames* nil)
(defparameter *parameters* nil)


;;;; SQLite DB schemas.
(defun initialise-db ()
  (connect (list ":memory:") :database-type :sqlite3)
  ;(connect (list "/home/varoun/tmp/data/nagios/nagios.db") :database-type :sqlite3)
  ;; Table to store the raw alerts.
  (execute-command "create table alerts (time integer, node integer, parameter integer, status
char(1))") 
  ;; Table to map between the node id and the node name.
  (execute-command "create table nodes (nid integer, nname varchar(50))")
  ;; Table to map between the parameter id and the parameter name.
  (execute-command "create table parameters (pid integer, pname varchar(50))")
  ;; Table for the current set of criticals at a point in time.
  (execute-command "create table currentset (time integer, crits varchar(5000))")
  (execute-command "create view alerts_ordered as select * from alerts order by time")
  (format t "~&Finished initialising the DB.~%"))



;;;; When parsing a log line, the regex-fail condition is signalled if a match fails.
(define-condition regex-fail (error)
  ((regex-parser :initarg :regex-parser :reader regex-parser)
   (text :initarg :text :reader text)))

;;;; Parsing a log for an element is implemented as a generic function.
(defgeneric parse-element (element source-format line)
  (:documentation "Parse a line of the log in the specified source-format (example 'nagios), for
the element (example 'nodename)."))

(defmethod parse-element ((element (eql 'hostname)) (source-format (eql 'nagios)) line)
  (multiple-value-bind (match save)
      (cl-ppcre:scan-to-strings *nagios-hostname-regex* line)
    (declare (ignore match))
    (if save
	(elt save 0)
	(error 'regex-fail :regex-parser 'hostname :text line))))

(defmethod parse-element ((element (eql 'parameter)) (source-format (eql 'nagios)) line)
  (multiple-value-bind (match save)
      (cl-ppcre:scan-to-strings *nagios-parameter-regex* line)
    (declare (ignore match))
    (if save
	(elt save 0)
	(error 'regex-fail :regex-parser 'parameter :text line))))

(defmethod parse-element ((element (eql 'time)) (source-format (eql 'nagios)) line)
  (multiple-value-bind (match save)
      (cl-ppcre:scan-to-strings *nagios-timestamp-regex* line)
    (declare (ignore match))
    (if save
	(parse-integer (elt save 0))
	(error 'regex-fail :regex-parser 'time :text line))))

(defmethod parse-element ((element (eql 'status)) (source-format (eql 'nagios)) line)
  (multiple-value-bind (match save)
      (cl-ppcre:scan-to-strings *nagios-status-regex* line)
    (declare (ignore save))
    (cond ((or (equal match "OK")
               (equal match "ok")
               (equal match "UP"))
           "O")
          ((equal match "CRIT") "C")
          ((equal match "WARN") "W")
	  (t 
	   (error 'regex-fail :regex-parser 'status :text line)))))

(defun parse-line (line &key type)
  "Parse a line of the give type (example 'nagios) for the elements of interest."
  (list
   (parse-element 'time type line)
   (parse-element 'hostname type line)
   (parse-element 'parameter type line)
   (parse-element 'status type line)))



;;;; Create and return unique ids for hostnames/parameters.
(defclass key-value ()
  ((index :accessor key-value-index :initform 0)
   (data :accessor key-value-data :initform (make-hash-table :test #'equal))))

(defmethod create-or-find-index ((store key-value) datum)
  (let ((kv-data (gethash datum (key-value-data store))))
    (if kv-data
        kv-data
        (progn
          (incf (key-value-index store))
          (setf (gethash datum (key-value-data store)) (key-value-index store))
          (key-value-index store)))))
#|
PREPROCESS> (setf *foo1* (make-instance 'key-value))
#<KEY-VALUE #x18CED1EE>
PREPROCESS> (create-or-find-index *foo1* "string1")
1
PREPROCESS> (create-or-find-index *foo1* "string1")
1
PREPROCESS> (create-or-find-index *foo1* "string2")
2
PREPROCESS> 
|#


(defun parse-log (stream &key log-format)
  (let ((*hostnames* (make-instance 'key-value))
	(*parameters* (make-instance 'key-value))
	(good-lines 0)
	(bad-lines 0))
    (when (> *debug* 0)
      (format t "~&Parsing logs and populating the ALERTS table..."))
    (loop 
       for line = (read-line stream nil nil) while line
       for entry = (handler-case (parse-line line :type log-format)
		     (regex-fail (err-obj)
		       (progn
			 (incf bad-lines)
			 (if (> *debug* 1)
			     (format t "~&Parsing |~a| for ~a -- FAILED!~%"
				     (text err-obj) (regex-parser err-obj))
			   nil))))
       when entry 
	 do
	 (progn
	   (incf good-lines)
	   (destructuring-bind (timestamp hostname parameter status) entry
	     (execute-command
	      (format nil "insert into alerts values (~a, ~a, ~a, \"~a\")"
		      timestamp
		      (create-or-find-index *hostnames* hostname)
		      (create-or-find-index *parameters* parameter)
		      status)))))
    (when (> *debug* 0) 
      (format t "Finished.~%")
      (format t "~&Populating the NODES table..."))
    (loop 
       for k being the hash-keys in (key-value-data *hostnames*) using (hash-value v)
         do
         (clsql:execute-command 
          (format nil "insert into nodes values (~a, \"~a\")" v k)))
    (when (> *debug* 0)
      (format t "Finished.~%")
      (format t "~&Populating the PARAMETERS table..."))
    (loop 
       for k being the hash-keys in (key-value-data *parameters*) using (hash-value v)
         do
         (clsql:execute-command
          (format nil "insert into parameters values (~a, \"~a\")" v k)))
    (when (> *debug* 0)
      (format t "Finished.~%"))
    (format t "~&Lines parsed successfully: ~a~%" good-lines)
    (format t "~&Lines not parsed: ~a~%" bad-lines)
    (format t "~&Total Hosts: ~a~%" (key-value-index *hostnames*))
    (format t "~&Total Parameters: ~a~%" (key-value-index *parameters*))))
    

(defun parse-log-file (filename &key log-format)
  (with-open-file (in filename :direction :input)
    (parse-log in :log-format log-format)))

