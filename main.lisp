;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GrokLogs - Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :com.groklogs.main)

(defun learn (file format)
  (initialise-db)
  (progn (in-package :com.groklogs.preprocess)
	 (parse-log-file file :log-format format))
  (build-current-set)
  (partitition-data)
  (build-datapoints)
  (build-complete-featurespaces)
  (learn-dependencies))