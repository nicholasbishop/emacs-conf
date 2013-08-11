;; enable column-number-mode
(setq-default column-number-mode t)

;; change scroll behavior of compilation
(setq compilation-scroll-output 'first-error)

;; recompile key
(global-set-key "\C-zc"
		'(lambda ()
		   (interactive)
		   (let ((orig-window (selected-window)))
			 (select-window
		      (get-buffer-window "*compilation*"))
		     (recompile)
			 (select-window orig-window))))
;; kill compilation key
(global-set-key "\C-zk" 'kill-compilation)

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

(push "\\*compil.*" same-window-regexps)

;; set better include search paths
(setq-default ff-search-directories
      '("."
        "/usr/include"
        "/usr/local/include"
        "/usr/include/c++/4.8"
        "../*/"
        "../../src/main/cpp"
        "../../../include/*/"))

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
(global-set-key "\C-z4"
				(lambda ()
				  (interactive)
				  (insert "scripts/startup/bl_ui/")))

;; enable electric-pair-mode
(electric-pair-mode)

(defun do-in-other-dir (dir action)
  "Run action in dir without blowing away current buffer's directory"
  (let ((old-dir default-directory)
		(old-buf (current-buffer)))
	;; change to the specified directory
	(cd dir)
	;; run action
	(call-interactively action)
	;; switch back to the previous buffer and restore the current
	;; working directory
	(let ((cur-buf (current-buffer)))
	  (set-buffer old-buf)
	  (cd old-dir)
	  ;; switch back to the new buffer
	  (set-buffer cur-buf))))

(defun compile-dir ()
  "Run compile in specified directory"
  (interactive)
  (do-in-other-dir
   (read-directory-name "Build directory: ")
   'compile)
  ;; protect compile window
  (set-window-dedicated-p (selected-window) t))

(defun gud-gdb-dir ()
  "Run gud-gdb in specified directory"
  (interactive)
  (do-in-other-dir
   (read-directory-name "Debug directory: ")
   'gud-gdb)
  ;; set better syntax highlighting
  (gud-gdb-colors-mode)
  ;; protect window
  (set-window-dedicated-p (selected-window) t))

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

;; Comments start with a '*'
(setq c-block-comment-prefix "*")

(defun make-path-safe-for-c-define (file-name)
  "Upcase the path, use underscores for dots, hyphens, and slashes"
  (replace-regexp-in-string "[-\\.\\\\\/]" "_" (upcase file-name)))

(defun c-header-guard-string (file-name use-dir suffix)
  "Return a C header guard name"
  (if use-dir
      (concat (make-path-safe-for-c-define file-name) suffix)
    (concat (make-path-safe-for-c-define
             (file-name-nondirectory file-name)) suffix)))

;; insert header guards
(defun c-header-guards ()
  (interactive)
  (let ((guard (read-string
                "Guard: "
                ;; Default value
                (c-header-guard-string (buffer-file-name) nil ""))))
    ;; Insert test and define before point
    (insert (concat "#ifndef " guard "\n#define " guard "\n\n")))
  ;; Insert endif after point
  (let ((pos (point)))
    (insert "\n\n#endif\n")
    (goto-char pos)))

(global-set-key "\C-zi" 'c-header-guards)

;; Namespace insertion
(defun cxx-insert-namespace ()
  (interactive)
  (insert "namespace " (read-string "Namespace: ")))

(global-set-key "\C-zn" 'cxx-insert-namespace)

;; C++11 fixes from stackoverflow.com/questions/6497374
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(font-lock-add-keywords
 'c++-mode
 '(("\\<final\\>" . font-lock-keyword-face)
   ("\\<nullptr\\>" . font-lock-keyword-face)
   ("\\<override\\>" . font-lock-keyword-face)))
