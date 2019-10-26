;; Lexical 
(defun foo (x)
  (bar) (print x) (+ 1 2))

(defun bar ()
  (print x))
