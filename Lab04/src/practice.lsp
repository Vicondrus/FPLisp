(defun fact (n)
    (if (<= n 1) 1 (* n (fact (- n 1)))))

;factorial with acumulator 
(defun fact2 (n a)
    (if (<= n 1) a (fact2 (- n 1) (* n a))))

(defun rev (l)
    (if (not (null l)) (append (rev (cdr l)) (list (car l))) l))

(defun my-length(l)
    (if (null l) 0 (+ 1 (my-length (cdr l)))))

(defun nb-atoms(l)
    (if (null l) 0
    (+ (if (listp (car l)) (nb-atoms(car l)) 1) (nb-atoms (cdr l)))))

(defun isAsc(ls)
    (if (= (length ls) 1) 1 (if (> (car ls) (cadr ls)) 0 (isAsc (cdr ls)) )))

(defun maxListFunc(ls m)
    (if (null ls) m (if (> (car ls) m) (maxListFunc (cdr ls) (car ls)) (maxListFunc (cdr ls) m))))

(defun maxList(ls)
    (maxListFunc ls 0))

(defun palindrome(ls)
    (cond ((null ls) T)
          (T (if (= (car ls) (car (reverse ls))) (palindrome (reverse (cdr (reverse (cdr ls))))) nil))))

(defun palindromeVic (ls) 
    (equal (reverse ls) ls)
)

(defun concat (&rest ls)
    (mapcan #'(lambda (x) x) ls))

(defun maxSublist(ls m) 
    (if (null ls) m
    (if (listp (car ls)) 
    (if (> (length (car ls)) m) (maxSublist (cdr ls) (length (car ls))) (maxSublist (cdr ls) m)) (maxSublist (cdr ls) m))))

(defun maxLenSub(ls)
    (maxSublist ls 0))

(defun arithSum(ls)
    (/ (eval (cons '+ ls)) (length ls)))

(defun arithSum2(ls)
    (/ (apply #'+ ls) (length ls)))

(defun eqList(ls1 ls2)
    (if (and (null ls1) (null ls2)) T
    (cond ((= (length ls1) (length ls2))
        (if (equal (car ls1) (car ls2)) (eqList (cdr ls1) (cdr ls2)) nil)))))

(defun groupAux (ls x)
    (if (not (null ls))
    (if (equal (car ls) (car x)) (groupAux (cdr ls) (append x (list (car ls)))) (append (list x) (group ls))) (list x)))

;??
(defun group (ls)
    (groupAux (cdr ls) (list (car ls))))

(defun rle (ls)
    (mapcar #'(lambda (x) (list (length x) (car x))) (group ls)))

(defun insertVc (ls pos x) 
    (append (butlast ls (- (length ls) pos)) (list x) (nthcdr ls pos))
)

(defun remove (ls x)
    (removeAux ls x nil))

(defun removeAux (ls x l)
    (if (null ls) l (if (= x 1) (removeAux (cdr ls) (- x 1) l) (removeAux (cdr ls) (- x 1) (append l (list (car ls))) ) )))

(defun insert(ls pos x)
    (insertAux ls pos x nil))

(defun insertAux(ls pos x l)
    (if (null ls) (if (= pos 1) (append l (list x)) l) 
    (if (= pos 1) (insertAux ls (- pos 1) x (append l (list x))) (insertAux (cdr ls) (- pos 1) x (append l (list (car ls)))))))

(defun flatten (ls)
    (mapcan #'(lambda (x) (if (listp x) (flatten x) (list x))) ls))

(defun elimDupl (ls l)
    (if (null ls) l (if (member (car ls) (cdr ls)) (elimDupl (cdr ls) l) (elimDupl (cdr ls) (append l (list (car ls)))))))

(defun eliminateDuplicates (ls)
    (elimDupl ls nil))

(defun elimConsDupl (ls l)
    (if (null ls) l (if (equal (car ls) (cadr ls)) (elimConsDupl (cdr ls) l) (elimConsDupl (cdr ls) (append l (list (car ls)))))))

(defun elimConsecDuplicates(ls)
    (elimConsDupl ls nil))

(defun groupAux (ls l)
    (if (null ls) l 
    (if (equal (car ls) (cadr ls)) (groupAux (cdr ls) (append l (list (car ls)))) (append (list (append l (list (car ls)))) (group (cdr ls))))))

(defun group (ls)
    (groupAux ls nil))

(defun rle (ls)
    (mapcan #'(lambda (x) (list (list (car x) (length x)))) (group ls)))

(defun dropNthEl(ls n l i)
    (if (null ls) l
    (if (equal (rem i n) 0) (dropNthEl (cdr ls) n l (+ i 1)) (dropNthEl (cdr ls) n (append l (list (car ls))) (+ i 1)))))

(defun dropNthElement(ls n)
    (dropNthEl ls n nil 1))
;??????????????????????????????????
(defun rotateK(ls k l)
    (if (null ls) l
    (if (>= k 0) (append (nthcdr k ls) (butlast ls (- (length ls) k))) (append (nthcdr (- (length ls) (- k)) ls) (butlast ls  (- k))))))

(defun rotateKtoLeft(ls k)
    (rotateK ls k nil))

(defun prime (n d)
    (if (<= n d) T
    (if (equal (rem n d) 0) nil (prime n (+ d 1)))))

(defun is-prime(n)
    (prime n 2))

(defun minTuples(ls m)
    (if (null ls) m (minTuples (cdr ls) (min m (caar ls) (cdar ls)))))

(defun findSmallest (ls)
    (minTuples ls 9999))

(defun makeList (n l)
    (if (= n 0) l (append (makeList (floor n 10) l) (list (rem n 10)))))

(defun listFromNb (n)
    (makeList n nil))
