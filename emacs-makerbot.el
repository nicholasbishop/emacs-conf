(add-to-list 'load-path "/home/nicholasbishop/emacs-conf/")
(load "top")

;; don't use tabs
(setq-default indent-tabs-mode nil)

;; header files (*.h) as C++ by default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun remove-obj-dir (filename)
  (replace-regexp-in-string
   "obj/" "" filename))

(setq compilation-parse-errors-filename-function 'remove-obj-dir)

;; default to Google C++ style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(defun makerbot-header-init ()
  (interactive)
  (let* ((file-name (file-relative-name (buffer-file-name) (git-root-dir)))
         (guard (c-header-guard-string file-name t "_"))
         (year (format-time-string "%Y"))
         (copyright (concat "// Copyright " year " MakerBot Industries")))
    (save-excursion
      (goto-char (point-min))
      (insert (concat copyright "\n\n"
                      "#ifndef " guard "\n"
                      "#define " guard "\n\n"))
      (goto-char (point-max))
      (insert (concat "\n#endif  // " guard "\n"))
      (goto-char ()))))

;; specialize the header initialization for MakerBot style
(global-set-key "\C-zi" 'makerbot-header-init)

;; start a shell
(if (eq system-type 'windows-nt)
    (cd "C:/Toolchain-Release")
  (cd "/d/makerbot"))
(shell)
