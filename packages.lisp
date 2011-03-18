;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs-ng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Pull in the libraries.
(ql:quickload 'cl-ppcre)
(ql:quickload 'clsql-sqlite3)

;;;; All utility functions.
(defpackage :com.groklogs.utils
  (:use :cl :clsql))

;;;; Database schemas.
(defpackage :com.groklogs.schema
  (:use :cl :clsql :com.groklogs.utils))

;;;; Preprocessing logs.
(defpackage :com.groklogs.preprocess
  (:use 
   :cl
   :cl-ppcre
   :clsql
   :com.groklogs.utils)
  (:export :initialise-db 
	   :parse-log-file))

;;;; The learning code.
(defpackage :com.groklogs.learn
  (:use :cl :clsql :com.groklogs.utils)
  (:export :build-current-set
	   :partition-data
	   :build-datapoints
	   :build-complete-featurespaces))

;;;; The 'MAIN' entrypoint.
(defpackage :com.groklogs.main
  (:use :cl :clsql 
	:com.groklogs.utils
	:com.groklogs.schema
	:com.groklogs.preprocess
	:com.groklogs.learn))
