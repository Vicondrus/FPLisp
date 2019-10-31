(defun func 
    (l1 l2 l) 
    (   if 
        (not 
            (null l1))
        (func 
            (cdr l1) 
            (cdr l2) 
            (append l 
                (list 
                    (car l1)) 
                (list 
                    (car l2)))) l))

(defun wrapper 
    (l1 l2)
    (func l1 l2 nil))

(defun alternate 
    (l1 l2) 
    (mapcan #'
        (lambda 
            (x1 x2) 
            (append 
                (list x1) 
                (list x2))) l1 l2)
)

(defun sum 
    (l a) 
    (if 
        (null l) a
        (if 
            (= 
                (rem 
                    (car l) 3) 0) 
            (sum 
                (cdr l) 
                (+ a 
                    (car l))) 
            (sum 
                (cdr l) a))))

(defun wrapSum 
    (l) 
    (sum l 0))

(defun elimDupl 
    (li) 
    (if 
        (null li) nil 
        (if 
            (equal 
                (car li) 
                (cadr li)) 
            (elimDupl 
                (cdr li)) 
            (append 
                (list (car li)) 
                (elimDupl 
                    (cdr li)))))
)