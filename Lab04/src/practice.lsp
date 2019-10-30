(defun fact (n)
    (if (<= n 1) 1 (* n (fact (- n 1)))))

;factorial with acumulator 
(defun fact2 (n a)
    (if (<= n 1) a (fact2 (- n 1) (* n a))))

(defun rev (l)
    (if (not (null l)) (append (rev (cdr l)) (list (car l))) l))

(defun my-length(l)
    (if (null l) 0 (+ 1 (my-length (cdr l)))))

(defun nb-atoms(l)
    (if (null l) 0
    (+ (if (listp (car l)) (nb-atoms(car l)) 1) (nb-atoms (cdr l)))))

