(DEFUN rev 
    (li) 
    (if 
        (NULL li) NIL 
        (append 
            (reverse 
                (cdr li)) 
            (list 
                (car li)))
)
)

(DEFUN palindromep 
    (li) 
    (equal 
        (reverse li) li)
)

(DEFUN listFromNumber 
    (x) 
    (if 
        (<= x 0) nil 
        (append 
            (listFromNumber 
                (floor x 10)) 
            (list 
                (mod x 10)))
)
)

(DEFUN firstK 
    (li k) 
    (if 
        (= k 0) nil 
        (append 
            (list 
                (car li)) 
            (firstK  
                (cdr li) 
                (- k 1))))
)

(DEFUN rotateLeft 
    (li k) 
    (if 
        (< k 0) 
        (if 
            (< k 
                (- 
                    (length li))) 
            (rotateLeft li 
                (- 
                    (mod 
                        (- k) 
                        (length li))))
            (append 
                (reverse 
                    (firstK 
                        (reverse li) 
                        (- k))) 
                (firstK li 
                    (- 
                        (length li) 
                        (- k))))) 
        (if 
            (> k 
                (length li)) 
            (rotateLeft li 
                (mod k 
                    (length li))) 
            (append 
                (reverse 
                    (firstK 
                        (reverse li) 
                        (- 
                            (length li) k))) 
                (firstK li k)))))