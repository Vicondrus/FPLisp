(SETQ atlas '((France . Paris) 
	      (Romania . Bucharest)
	      (Germany . Bonn)))

;;List of capitals
(DEFUN selcap (l)
  (MAPCAR #'CDR l))

