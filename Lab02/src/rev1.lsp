(defun rev1 (ls)
  (rev1ac ls NIL))

(defun rev1ac (ls res)
  (cond ((ENDP ls) res)
        (T (rev1ac (REST ls) (CONS (FIRST ls) res)))))
