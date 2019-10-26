(DEFUN f (l &optional end)
    (if (NULL end)  (append l '(period))
                    (append l end)))
