;;; Wrapper to load and start Eliza / expression

;;; Psychiatrist ELIZA
(DEFUN go-Eliza ()
 (load "patternMatching.lsp")
 (load "eliza.lsp")
 (doctor))
