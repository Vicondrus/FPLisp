(defun rev0 (ls)
  (if (NOT (NULL ls))
      (ap (rev0 (REST ls))
          (LIST (FIRST ls)))))

(defun ap (ls1 ls2)
  (if (NULL ls1) ls2 
      (CONS (CAR ls1) (ap (CDR ls1) ls2))))
