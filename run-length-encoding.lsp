(defun groupAux (ls x)
    (if (not (null ls))
    (if (equal (car ls) (car x)) (groupAux (cdr ls) (append x (list (car ls)))) (append (list x) (group ls))) (list x)))

(defun group (ls)
    (groupAux (cdr ls) (list (car ls))))

(defun rle (ls)
    (mapcar #'(lambda (x) (list (length x) (car x))) (group ls)))

(print (rle '(a a a b b )))
