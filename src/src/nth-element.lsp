(DEFUN nth_element 
    (n l)
    (if 
        (= n 1) 
        (car l)
        (if 
            (NULL l) NIL 
            (nth_element 
                (- n 1) 
                (cdr l))
)
)
)