(DEFUN our-remove-if (pred l)
  (MAPCAN #'(LAMBDA (x) (IF (NOT (FUNCALL pred x)) (LIST x))) l))