;;;; Patterm matching program 
(DEFUN match (p d assignments)
  (COND ((AND	(NULL p) (NULL d))		        ;Success: both p and d empty  
	        (COND ((NULL assignments) T)            ;Return TRUE (case identincal) or
		       (T assignments)))                ;assignments (case identincal non-empty list)
	 ((OR	(NULL p) (NULL d)) NIL)			;Failure: one null, one not null (case different)
	 ((OR	(EQUAL (FIRST p) '?)			;Case singleton wildcard 
		(EQUAL (FIRST p) (FIRST d)))		;Case identincal (same heads)
	    (match (REST p) (REST d) assignments))
	 ((EQUAL (FIRST p) '+)				;Case multiple wildcard 
	    (OR (match (REST p) (REST d) assignments)  
		(match p        (REST d) assignments)))
	 ((ATOM (FIRST p)) NIL)				;Unmatched atom 
	 ((EQUAL (pattern-indicator (FIRST p)) '>)	;Case instantiate variable 
	    (match (REST p) (REST d)
		    (shove-gr	(pattern-variable (FIRST p))
				(FIRST d)
				assignments)))
	 ((EQUAL (pattern-indicator (FIRST p)) '<)	;Case already instantiated variable
	    (match (CONS (pull-value (pattern-variable (FIRST p)) 
	                              assignments)
			 (REST p))
		    d
		    assignments))
	 ((EQUAL (pattern-indicator (FIRST p)) '+)	;Case instantiate list 
	    (LET ((new-assignments (shove-pl (pattern-variable (FIRST p))
				             (FIRST d)
				             assignments)))
		  (OR	(match (REST p) (REST d) new-assignments)
			(match p (REST d) new-assignments))))
	 ((AND	(EQUAL (pattern-indicator (FIRST p)) 'RESTRICT) ;Case restricted symbol 			
		(EQUAL (restriction-indicator (FIRST p)) '?)
		(test  (restriction-predicates (FIRST p)) (FIRST d)))
	    (match (REST p) (REST d) assignments))))

;;; Selector macros
(DEFMACRO pattern-indicator (l) `(FIRST ,l))    ; >,<,+ or RESTRICT
(DEFMACRO pattern-variable (l)  `(SECOND ,l))   ; >,< or +
(DEFMACRO match-value (key a-list) `(SECOND (ASSOC ,key ,a-list)))
(DEFMACRO pull-value (variable a-list) `(SECOND (ASSOC ,variable ,a-list)))


;;; Mutator 
(DEFMACRO shove-gr (variable item a-list)
  `(APPEND ,a-list (LIST (LIST ,variable ,item))))
(DEFUN shove-pl (variable item a-list)
  (COND ((NULL a-list) (LIST (LIST variable (LIST item))))
	 ((EQUAL variable (FIRST (FIRST a-list)))
		(CONS	(LIST variable (APPEND (SECOND (FIRST a-list))
						   (LIST item)))
			(REST a-list)))
	 (T (CONS (FIRST a-list)
		   (shove-pl variable item (REST a-list))))))

(DEFMACRO restriction-indicator (pattern-item)
  `(SECOND ,pattern-item))
  
(DEFMACRO restriction-predicates (pattern-item)
  `(REST (REST ,pattern-item)))

;;; Check if the elements in datum satisfy the given predicates      
(DEFUN test (predicates argument)
  (COND ((NULL predicates) T)                      ;All tests T?
	 ((FUNCALL (FIRST predicates) argument)    ;This test T?
		(test (REST predicates) argument))
	 (T NIL)))                                 ;This test NIL?

