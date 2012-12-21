(define-derived-mode gud-gdb-colors-mode gud-mode
  (setq font-lock-defaults
		'((;; function names
		   ("\s *\\([a-zA-Z_]\\(\\sw\\|_\\|:\\)*\\) \(" 1 font-lock-function-name-face)
		   ;; line numbers
		   (":\\([0-9]*\\)" 1 font-lock-type-face)) t))
  
  (setq mode-name "gud-gdb-colors-mode")
)
