;;;;;;;;;;;;;;;;;;;;
;;; Reversing a list 
;;;;;;;;;;;;;;;;;;;;

;; No accumulator version 
;; In order to see the execution time: 
;; (trace rev0) 

;; (trace ap)
;; (rev0 '(1 2 3 4))
(defun rev0 (ls)
  (if (NOT (NULL ls))
      (ap (rev0 (REST ls))
          (LIST (FIRST ls)))))

;;user defined append for trace  purposes 
(defun ap (ls1 ls2)
  (if (NULL ls1) ls2 
      (CONS (CAR ls1) (ap (CDR ls1) ls2))))


;; Accumulator version
(defun rev1 (ls)
  (rev1ac ls NIL))

(defun rev1ac (ls res)
  (cond ((ENDP ls) res)
        (T (rev1ac (REST ls) (CONS (FIRST ls) res)))))

;;For tracing: 
(setq tl '( 1  2  3  4  5  6  7  8  9 10 
           11 12 13 14 15 16 17 18 19 20 
	   21 22 23 24 25 26 27 28 29 30 
	   31 32 33 34 35 36 37 38 39 40 
	   41 42 43 44 45 46 47 48 49 50 
	   51 52 53 54 55 56 57 58 59 60 
	   61 62 63 64 65 66 67 68 69 70 
	   71 72 73 74 75 76 77 78 79 80 
	   81 82 83 84 85 86 87 88 89 90 
	   91 92 93 94 95 96 97 98 99 100))


;;Generate a list of n elements; what does the function return when the IF test fails?
(defun nelems (n)
 (if (not (zerop n)) (cons n (nelems (- n 1)))))


;;Generate a list of n elements
(defun f (n) (fa n ()))

(defun fa (n a)
 (if (zerop n) a (fa (- n 1) (cons n a))))
