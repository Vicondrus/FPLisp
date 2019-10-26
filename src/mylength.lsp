(defun mylength (l)
    (cond ((NULL l) 0)
          (T (+ 1 (mylength (rest l))))))
