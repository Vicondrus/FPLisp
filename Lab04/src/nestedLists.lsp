(DEFUN compress 
    (li) 
    (if 
        (null li) li 
        (if 
            (equal 
                (first li) 
                (second li)) 
            (compress 
                (cdr li)) 
            (append 
                (list 
                    (car li)) 
                (compress 
                    (cdr li)))
))
)

(DEFUN group 
    (li) 
    (if 
        (null 
            (cdr li)) li 
        (groupAux 
            (cdr li) 
            (car li) 
            (list 
                (car li))
)
))

(defun groupAux 
    (li prev acc) 
    (if 
        (null li)
        (list acc) 
        (if 
            (equal 
                (car li) prev) 
            (groupAux 
                (cdr li) prev 
                (cons prev acc))
            (cons acc 
                (groupAux 
                    (cdr li) 
                    (car li) 
                    (list 
                        (car li))))
)
)
)

(DEFUN runLength 
    (li) 
    (mapcar
        (lambda 
            (group) 
            (list 
                (length group) 
                (first group)))
        (group li)
)
)