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