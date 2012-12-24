;; Adapted from:
;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b "
        'help-echo (buffer-file-name)))

    ;; line and column
    ;; '%02' to set to 2 chars at least; prevents flickering
	(propertize " (%03l:%02c)" 'face 'font-lock-type-face)

    ;; relative position, size of file
	"  ("
    ;;(propertize "%p") ;; % above top
    ;;":"
    (propertize "%I") ;; size
    ") "

    ;; the current major mode for the buffer.
    " "
	'(:eval mode-line-modes)


    " (" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ", "  (propertize "Modified"
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ", "  (propertize "RO"
                             'help-echo "Buffer is read-only"))))

	;; is this buffer protected?
	'(:eval (when (window-dedicated-p)
              (concat ", "  (propertize "Protected"
                             'help-echo "Buffer is read-only"))))
    ") "

    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    ;;"%-" ;; fill with '-'
    ))
