(DEFUN rev 
    (lsp) 
    (if 
        (null lsp) nil 
        (append 
            (rev 
                (cdr lsp)) 
            (list 
                (car lsp)))
)
)