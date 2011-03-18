;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs - Database Schemas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :com.groklogs.schema)

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
  (execute-command "create table datapoints (time integer, nodeparam integer, crits
varchar(5000), class integer)")  
  (execute-command "create table featureindex (nodeparam integer primary key, fvector
varchar(5000))") 
  (execute-command "create table featurespace (nodeparam integer primary key, examples varchar(5000))")
  (format t "~&Finished initialising the DB.~%"))
