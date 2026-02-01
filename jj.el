;; Jujutsu

(defun jj-line-revision ()
  "Get revision of current line in current buffer or nil if not committed"
  (string-trim
   (shell-command-to-string 
    (concat
     "jjb blame-line "
	 (buffer-file-name)
     " "
     (number-to-string (line-number-at-pos))))))

(defun jj-print-line-log ()
  "Print log of jj-line-revision and copy commit hash to kill ring"

  (interactive)
  (let ((rev (jj-line-revision)))
	(progn
	  ;; output log
	  (message (shell-command-to-string (concat "jj show -s " rev)))
	  ;; copy commit hash to kill ring
	  (kill-new rev))))

(global-unset-key "\C-j")
(global-set-key "\C-jl" 'jj-print-line-log)
(global-set-key "\C-j\C-l" 'jj-print-line-log)
