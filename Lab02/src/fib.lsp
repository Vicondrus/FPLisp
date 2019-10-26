
(DEFUN fib 
    (n) 
    (if 
        (numberp n) 
        (cond 
            (
                (= n 0) 0) 
            (
                (= n 1) 1) 
            (T 
                (+ 
                    (fib 
                        (- n 1))  
                    (fib 
                        (- n 2)))
)
)
)
)

(DEFUN fiba 
    (n)
    (fibaux n 0 1)
)

(DEFUN fibaux
    (n a b)
    (if 
        (= n 0) a 
        (fibaux 
            (- n 1) b 
            (+ a b))
)
)