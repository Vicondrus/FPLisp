;;; A psychiatrist program, which imitates the Doctor program, aka ELIZA
;;; To run, call: 
;;;   (doctor)
;;;
(DEFUN doctor ()
  (PRINT '(Speak up!)) (TERPRI)
  (DO  ( (sentence (READ) (READ)) 
         (a-list NIL NIL) 
         (mother))
       (NIL)	
       (COND ((SETQ a-list (match '(I am worried (+ l)) sentence NIL))
              (PRINT `(How long have you been worried ,@(match-value 'l a-list))))
			
             ((match '(+ mother +) sentence NIL)
              (SETQ mother T)
              (PRINT '(Tell me more about your family)))

             ((match '(+ computers +) sentence NIL)
              (PRINT '(Do machines frighten you)))
	   
             ((OR (match '(No) sentence NIL)
                  (match '(Yes) sentence NIL))
              (PRINT '(Please do not be so short with me)))
	   
             ((match '(+ (RESTRICT ? bad-word-p) +) sentence NIL)
              (PRINT '(Please do not use words like that)))
	   
             (mother (SETQ mother NIL)
              (PRINT '(Earlier you spoke of your mother)))
		  
             (T (PRINT '(I am sorry our time is up))
                (RETURN 'Goodbye)))
        (TERPRI)))

(DEFUN bad-word-p (word) (MEMBER word '(shucks darn)))
