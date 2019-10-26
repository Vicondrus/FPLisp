(DEFUN removeNth 
    (n ls) 
    (if 
        (null ls) ls 
        (if 
            (= n 1)  
            (cdr ls)
            (append 
                (list 
                    (car ls)) 
                (removeNth  
                    (- n 1) 
                    (cdr ls)))
)
)
)

(DEFUN insertNth 
    (n el ls) 
    (if 
        (null ls) ls 
        (if 
            (= n 1) 
            (append 
                (list el) ls) 
            (append 
                (list 
                    (car ls)) 
                (insertNth 
                    (- n 1) el 
                    (cdr ls)))
)
)
)