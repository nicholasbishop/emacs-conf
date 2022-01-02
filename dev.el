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
      '("." "../*/"))

;; key to switch between header and src
(global-set-key "\C-zh" 'ff-find-other-file)

;; key to load manpage
(global-set-key "\C-zm" 'man)

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

(setq copyright-owner "Neverware")

;; insert header guards
(defun c-header-guards ()
  (interactive)
  (let* ((file-name (file-relative-name (buffer-file-name) (git-root-dir)))
         (guard (c-header-guard-string file-name t "_"))
         (year (format-time-string "%Y"))
         (copyright (concat "// Copyright " year " " copyright-owner)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat copyright "\n\n"
                      "#ifndef " guard "\n"
                      "#define " guard "\n\n"))
      (goto-char (point-max))
      (insert (concat "\n#endif  // " guard "\n"))
      (goto-char ()))))

(global-set-key "\C-zi" 'c-header-guards)

;; Namespace insertion
(defun cxx-insert-namespace ()
  (interactive)
  (insert "namespace " (read-string "Namespace: ")))

(global-set-key "\C-zn" 'cxx-insert-namespace)

(defun string-strip (str)
  (replace-regexp-in-string
   "[ \t]*$" ""
   (replace-regexp-in-string "^[ \t]*" "" str)))

(defun c-make-switch-cases (text)
  "text should be body of an enum, outputs corresponding switch case lines"
  (let ((result ""))
    (dolist (line (split-string text "\n"))
      (when (not (string= line ""))
        (setq result
              (concat result
                      "case "
                      ;; Strip whitespace from ends and trailing comma
                      (string-strip (replace-regexp-in-string "," "" line))
                      ":\n"))))
    result))

(defun c-make-and-insert-switch-cases ()
  (interactive)
  (let ((input (car kill-ring)))
       (insert-before-markers (c-make-switch-cases input))
  ))

(global-set-key "\C-ze" 'c-make-and-insert-switch-cases)


(require 'web-mode)
(setq web-mode-enable-current-element-highlight nil)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-enable-auto-indentation nil)
            (setq web-mode-enable-auto-quoting nil)))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Javascript: default to 2-space indent
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
			(setq indent-tabs-mode nil)))

;; Same for Typescript
(add-hook 'typescript-mode-hook
          (lambda ()
            (setq typescript-indent-level 2)))

;; don't use tabs
(setq-default indent-tabs-mode nil)

;; header files (*.h) as C++ by default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; default to Google C++ style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; QT's qmake configuration files
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Rust
;;
;; Based on https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  ;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )


(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))
