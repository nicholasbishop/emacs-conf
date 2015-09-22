; winner!
(winner-mode)

;; window movement
(global-set-key (kbd "C-S-k") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-S-j") (lambda () (interactive) (other-window -1)))

;; frame movement
(global-set-key "\C-zo" 'other-frame)

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

;; fix for shell column width after splitting frame
;; see http://stackoverflow.com/questions/7987494/emacs-shell-mode-display-is-too-wide-after-splitting-window
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    ;; by default, ls uses tabs ("for efficiency", according to the
    ;; GNU docs (wtf?)), and expects them to be eight spaces
    (set (make-local-variable 'tab-width) 8)
    (let ((proc (get-buffer-process (current-buffer))))
      (when proc
	(set-process-window-size proc
				 (window-height)
				 (window-width))))))

(defun my-shell-mode-hook ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; convenient buffer rename
(global-set-key (kbd "C-z RET") 'rename-buffer)

;; Disable this stupid suspend key
(global-unset-key (kbd "C-x C-z"))
