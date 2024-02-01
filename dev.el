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

(setq copyright-owner "The ChromiumOS Authors\n// Use of this source code is governed by a BSD-style license that can be\n// found in the LICENSE file.")

;; insert header guards
(defun c-header-guards ()
  (interactive)
  (let* ((file-name (file-relative-name (file-truename (buffer-file-name)) (git-root-dir)))
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

;; Fixup for 8b754ca318cb88d9e5f437ccd919a722d345f13f,
;; "compilation-goto-locus does not handle right display-buffer" This
;; commit causes next-error (C-x `) to do the wrong thing, as it no
;; longer uses the current window as a target, which is what I want it
;; to do.
;;
;; This is a full copy of compilation-goto-locus from before that patch
;; was applied.
;;
;; Possibly there's some alternative that involves the poorly-documented
;; display-buffer-alist, or with advice, but honestly who has the time.
(defun compilation-goto-locus (msg mk end-mk)
  "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non-nil, mark is set there
and overlay is highlighted between MK and END-MK."
  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((from-compilation-buffer (eq (window-buffer)
                                      (marker-buffer msg)))
         ;; Use an existing window if it is in a visible frame.
         (pre-existing (get-buffer-window (marker-buffer msg) 0))
         (w (if (and from-compilation-buffer pre-existing)
                ;; Calling display-buffer here may end up (partly) hiding
                ;; the error location if the two buffers are in two
                ;; different frames.  So don't do it if it's not necessary.
                pre-existing
	      (display-buffer (marker-buffer msg) '(nil (allow-no-window . t)))))
	 (highlight-regexp (with-current-buffer (marker-buffer msg)
			     ;; also do this while we change buffer
			     (goto-char (marker-position msg))
			     (and w (progn (compilation-set-window w msg)
                                           (compilation-set-overlay-arrow w)))
			     compilation-highlight-regexp)))
    ;; Ideally, the window-size should be passed to `display-buffer'
    ;; so it's only used when creating a new window.
    (when (and (not pre-existing) w)
      (compilation-set-window-height w))

    (if from-compilation-buffer
        ;; If the compilation buffer window was selected,
        ;; keep the compilation buffer in this window;
        ;; display the source in another window.
        (let ((pop-up-windows t))
          (pop-to-buffer (marker-buffer mk) 'other-window))
      (switch-to-buffer (marker-buffer mk)))
    (unless (eq (goto-char mk) (point))
      ;; If narrowing gets in the way of going to the right place, widen.
      (widen)
      (if next-error-move-function
	  (funcall next-error-move-function msg mk)
	(goto-char mk)))
    (if end-mk
        (push-mark end-mk t)
      (if mark-active (setq mark-active nil)))
    ;; If hideshow got in the way of
    ;; seeing the right place, open permanently.
    (dolist (ov (overlays-at (point)))
      (when (eq 'hs (overlay-get ov 'invisible))
        (delete-overlay ov)
        (goto-char mk)))

    (when highlight-regexp
      (if (timerp next-error-highlight-timer)
	  (cancel-timer next-error-highlight-timer))
      (unless compilation-highlight-overlay
	(setq compilation-highlight-overlay
	      (make-overlay (point-min) (point-min)))
	(overlay-put compilation-highlight-overlay 'face 'next-error))
      (with-current-buffer (marker-buffer mk)
	(save-excursion
	  (if end-mk (goto-char end-mk) (end-of-line))
	  (let ((end (point)))
	    (if mk (goto-char mk) (beginning-of-line))
	    (if (and (stringp highlight-regexp)
		     (re-search-forward highlight-regexp end t))
		(progn
		  (goto-char (match-beginning 0))
		  (move-overlay compilation-highlight-overlay
				(match-beginning 0) (match-end 0)
				(current-buffer)))
	      (move-overlay compilation-highlight-overlay
			    (point) end (current-buffer)))
	    (if (or (eq next-error-highlight t)
		    (numberp next-error-highlight))
		;; We want highlighting: delete overlay on next input.
		(add-hook 'pre-command-hook
			  #'compilation-goto-locus-delete-o)
	      ;; We don't want highlighting: delete overlay now.
	      (delete-overlay compilation-highlight-overlay))
	    ;; We want highlighting for a limited time:
	    ;; set up a timer to delete it.
	    (when (numberp next-error-highlight)
	      (setq next-error-highlight-timer
		    (run-at-time next-error-highlight nil
				 'compilation-goto-locus-delete-o)))))))
    (when (and (eq next-error-highlight 'fringe-arrow))
      ;; We want a fringe arrow (instead of highlighting).
      (setq next-error-overlay-arrow-position
	    (copy-marker (line-beginning-position))))))
