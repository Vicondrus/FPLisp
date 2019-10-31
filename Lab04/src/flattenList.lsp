(DEFUN flatten 
    (ls) 
    (if 
        (null ls) 
        () 
        (if 
            (listp 
                (car ls)) 
            (append 
                (flatten 
                    (car ls)) 
                (flatten 
                    (cdr ls))) 
            (append  
                (list 
                    (car ls)) 
                (flatten 
                    (cdr ls))))
)
)

(DEFUN flatten1 
    (li)
    (flattenAux li '())

)

(DEFUN flattenAux 
    (li f) 
    (if 
        (NULL li) f

        (if 
            (LISTP 
                (car li)) 

            (flattenAux 
                (cdr li)  
                (append f 
                    (flatten 
                        (car li))))

            (flattenAux 
                (cdr li)
                (append f (list (car li))))

)
))