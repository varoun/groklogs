;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs-ng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Pull in the libraries.
(ql:quickload 'cl-ppcre)
(ql:quickload 'clsql-sqlite3)

;;;; Preprocessing logs
(defpackage :com.groklogs.preprocess
  (:use 
   :cl
   :cl-ppcre
   :clsql))

