(global-unset-key "\C-z")

;; color theme
(load "theme")

;; window customizations/keys
(load "window")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(pop-up-windows nil)
 '(pop-up-frames nil)
 '(c-basic-offset 4)
 '(tab-width 4)
 '(inhibit-startup-screen t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; disable toolbar, menubar, and scrollbars
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(global-set-key "\C-x\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key [f5] 'rgrep)

;; Enable some disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Combine C-a and M-m into C-a, change C-S-m to kill *entire* line
(global-set-key "\C-a"
				(lambda ()
				  (interactive)
				  (let ((curpoint (point)))
					(back-to-indentation)
					(if (= curpoint (point))
						(move-beginning-of-line nil)))))

(global-set-key (kbd "C-S-m")
				(lambda ()
				  (interactive)
				  (move-beginning-of-line nil)
				  (kill-whole-line)))

(push '("." . "~/.emacs_backups") backup-directory-alist)

(global-set-key "\C-zl" 'toggle-truncate-lines)

;; start server if not already running
(unless (boundp 'server-process)
  (server-start))

;; mode line
(load "mode-line")

;; ;; color theme (emacs 23)
;; (add-to-list 'load-path (concat dd "color-theme-6.6.0"))
;; (load-library "color-theme")
;; (load (concat dd "color-theme-tango"))
;; (color-theme-tango)
;; ;; color of shell output
(setq ansi-color-names-vector '["black" "red" "green" "yellow" "steel blue" "magenta" "cyan" "white"])

;; Windows development: change default shell to git-bash
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:\\Program Files (x86)\\Git\\bin\\bash")
  (setq explicit-sh-args '("-login" "-i")))

;; development
(load "dev")
(load "git")
(load "gud-gdb-colors-mode")
;; don't ask about killing compilation
(load "compilation-always-kill")
(compilation-always-kill-mode 1)
;; TeX
(load "tex")
;; json mode
(load "json-mode")
(setq auto-mode-alist
   (cons '("\\.json" . json-mode) auto-mode-alist))
;; cmake mode
(load "cmake-mode")
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;; Small font (only on X11)
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 11")))

;; Avoid splitting windows when a command wants to show something
(defun no-split-window ()
  (interactive)
  nil)
(setq split-window-preferred-function 'no-split-window)

;; Load SConscript/SConstruct files in python-mode
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

;; Load .qrc in xml-mode
(add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))

(defun shell-dir ()
  "Run shell in specified directory"
  (interactive)
  ;; change to the user-specified directory
  (cd (read-directory-name "Shell directory: "))
  ;; start shell
  (call-interactively 'shell)
  ;; protect window
  (set-window-dedicated-p (selected-window) t))
(global-set-key "\C-z3" 'shell-dir)

;; add hook to image mode that reloads the image when <return> is
;; pressed
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(add-hook 'image-mode-hook
		  (lambda () (local-set-key
					  (kbd "<return>")
					  'revert-buffer-no-confirm)))

;; glsl
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;; ebuild
(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))

;; enable subword mode in all buffers
(global-subword-mode t)

;; remap M-space to act like "C-c C-d" in CC-Mode)
(require 'cc-mode)
(global-set-key (kbd "M-SPC") 'c-hungry-delete-forward)

;; map C-z C-r to reload the buffer (useful for read-only buffers
;; where regular insertion doesn't work)
(global-set-key "\C-z\C-r" 'revert-buffer)

;; Better dynamic-expansion when working with camel-case code
(setq dabbrev-case-fold-search nil)

;; Enable additional package sources
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
