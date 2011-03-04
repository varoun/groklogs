;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs-ng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Pull in the libraries.
(ql:quickload 'cl-ppcre)
(ql:quickload 'clsql-sqlite3)

;;;; All utility functions.
(defpackage :com.groklogs.utils
  (:use :cl :clsql))

;;;; Preprocessing logs.
(defpackage :com.groklogs.preprocess
  (:use 
   :cl
   :cl-ppcre
   :clsql))

;;;; The main learning code.
(defpackage :com.groklogs.learn
  (:use :cl :clsql))
