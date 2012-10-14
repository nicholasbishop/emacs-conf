; winner!
(winner-mode)

;; window movement
(global-set-key (kbd "C-S-k") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-S-j") (lambda () (interactive) (other-window -1)))

;; protect/unprotect the window from modification
(global-set-key "\C-z\C-w"
		(lambda ()
		  (interactive)
		  (set-window-dedicated-p (selected-window)
								  (not (window-dedicated-p (selected-window))))
		  (if (window-dedicated-p (selected-window))
			  (message "Window protected")
			(message "Window unprotected"))))

;; writing mode
(global-set-key "\C-z]"
				(lambda ()
				  (interactive)
				  (visual-line-mode 1)
				  (let* ((win (selected-window))
						 (width (window-total-width win))
						 (margin (max (/ (- width 78) 2) 0)))
					(set-window-margins nil margin margin))))

(global-set-key "\C-z["
				(lambda ()
				  (interactive)
				  (visual-line-mode -1)
				  (set-window-margins nil nil nil)))

(global-set-key (kbd "<f7>")
				(lambda ()
				  (interactive)
				  (split-window-below)
				  (balance-windows)))
(global-set-key (kbd "<f8>")
				(lambda ()
				  (interactive)
				  (split-window-right)
				  (balance-windows)))
