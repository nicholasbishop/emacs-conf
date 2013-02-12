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

;; start a shell
(cd "/d/makerbot/Toolchain-Release")
(shell)
