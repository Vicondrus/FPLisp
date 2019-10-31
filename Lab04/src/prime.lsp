(defun primeAux (nb x) 
    (if (= nb x) T 
    (and (primeAux nb (+ 1 x)) (not (= (mod nb x) 0)))
    )
)

(defun prime (nb) 
    (if (<= nb 1) NIL 
    (primeAux nb 2)
    )
)