;; enable column-number-mode
(setq-default column-number-mode t)

;; change scroll behavior of compilation
(setq compilation-scroll-output 'first-error)

;; recompile key
(global-set-key "\C-zc"
		'(lambda ()
		   (interactive)
		   (save-window-excursion
		     (select-window
		      (get-buffer-window "*compilation*"))
		     (recompile))))

;; key to switch to debugger window
(global-set-key "\C-zd"
		'(lambda ()
		   (interactive)
		   (mapcar '(lambda (buf)
			      ;; look for a *gud* buffer
			      (if (string-match
				   "\*gud-.*"
				   (buffer-name buf))
				  ;; select its window
				  (select-window
				   (get-buffer-window buf))))
			   (buffer-list))))

;; keys to start and stop the debugged process
(global-set-key "\C-zr" 'gud-run)
(global-set-key "\C-zk" 'gud-stop-subjob)

(push "\\*compil.*" same-window-regexps)

;; key to switch between header and src
(global-set-key "\C-zh" 'ff-find-other-file)

;; key to load manpage
(global-set-key "\C-zm" 'man)

;; c-mode hook
(defun c-mode-blender-hook ()
  (push '(substatement-open . (after)) c-hanging-braces-alist)
  (push '(brace-list-open . (after)) c-hanging-braces-alist)
  (push '(brace-list-close) c-hanging-braces-alist)
  (push '(block-close . (before)) c-hanging-braces-alist)
  (push '(class-open . (after)) c-hanging-braces-alist)
  (push '(class-close) c-hanging-braces-alist)
  (c-toggle-auto-newline 1)

  ;; don't indent braces that are on their own line (e.g. after
  ;; multi-line if)
  (push '(substatement-open . 0) c-offsets-alist)
  (push '(case-label . +) c-offsets-alist)

  ;; don't insert a newline after semi-colons
  (push '(lambda () 'stop) c-hanging-semi&comma-criteria))
(add-hook 'c-mode-common-hook 'c-mode-blender-hook)

;; Blender-specific
(global-set-key "\C-z3"
				(lambda ()
				  (interactive)
				  (insert "scripts/startup/bl_ui/")))

;; enable electric-pair-mode
(electric-pair-mode)

(defun compile-dir ()
  "Run compile in specified directory"
  (interactive)
  (let ((cur-dir default-directory))
	;; change to the user-specified compile directory
	(cd (read-directory-name "Build directory: "))
	;; start compile
	(call-interactively 'compile)
	;; protect window
	(set-window-dedicated-p (selected-window) t)
	;; restore current directory
	;(cd cur-dir)))
	))

(defun gud-gdb-dir ()
  "Run gud-gdb in specified directory"
  (interactive)
  (let ((cur-dir default-directory))
	;; change to the user-specified compile directory
	(cd (read-directory-name "Debug directory: "))
	;; start gud-gdb
	(call-interactively 'gud-gdb)
	(gud-gdb-colors-mode)
	;; protect window
	(set-window-dedicated-p (selected-window) t)
	;; restore current directory
	;(cd cur-dir)))
	))

(global-set-key "\C-z1" 'compile-dir)
(global-set-key "\C-z2" 'gud-gdb-dir)

(global-set-key "\C-z\C-z" 'comment-region)
(global-set-key "\C-zz"
				'(lambda ()
				   (interactive)
				   (comment-region (point) (mark) -1)))

;; section header comment in C
(defun c-section-comment ()
  (interactive)
  (let* ((text-len
		  (- (line-end-position) (line-beginning-position)))
		 (target-len (- 72 4))
		 (right-len (/ (- target-len text-len) 2))
		 (left-len (- target-len text-len right-len)))
	;; (print text-len)
	;; (print target-len)
	;; (print right-len)
	;; (print left-len)
	(beginning-of-line)
	(insert "/")
	(dotimes (i left-len)
	  (insert "*"))
	(insert " ")
	(end-of-line)
	(insert " ")
	(dotimes (i right-len)
	  (insert "*"))
	(insert "/")))

(global-set-key "\C-zs" 'c-section-comment)