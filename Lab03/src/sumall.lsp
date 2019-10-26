(DEFUN sumall 
    (l) 
    (sumallAux 0 l)
)

(DEFUN sumallAux 
    (a l)
    (if 
        (NULL l) a
        (if 
            (LISTP 
                (CAR l)) 
            (sumallAux 
                (+ a 
                    (sumall 
                        (car l)) 
)                (cdr l))
            (sumallAux 
                (+ a 
                    (if 
                        (numberp 
                            (car l)) 
                        (car l) 0) 
)                (cdr l))
))
)