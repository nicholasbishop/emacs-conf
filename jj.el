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

(defun jj-diff ()

  (interactive)

  (let ((bufname "*jj-diff*")
        (diff (shell-command-to-string (concat "jj diff --git " (buffer-file-name)))))
    (progn
      ;; Delete the buffer if it already exists.
      (condition-case nil
          (kill-buffer bufname)
        (error nil))

      ;; Create the buffer and fill it.
      (set-buffer (get-buffer-create bufname))
      (insert diff)
      (diff-mode)
      (read-only-mode)

      ;; Show the buffer.
      (display-buffer bufname))))

(defun jj-root ()
  "Search upwards from cwd for jj root."
  (substring (shell-command-to-string "jj root") 0 -1))

(defun jj-buffer-path ()
  "Add buffer's file path relative to git root to the kill ring"
  (interactive)
  (let ((path (file-relative-name
               ;; buffer-filename returns an absolute path but not
               ;; necessarily a canonical path in that symlinks are
               ;; not resolved, whereas git-root-dir does implicitly
               ;; resolve symlinks because that's the behavior of `git
               ;; rev-parse --show-toplevel`. To make
               ;; file-relative-name work as expected, call
               ;; file-truename on the buffer name to canonicalize it.
               (file-truename (buffer-file-name))
               (jj-root))))
	(message path)
	(kill-new path)))
      
(global-unset-key "\C-j")

(global-set-key "\C-jl" 'jj-print-line-log)
(global-set-key "\C-j\C-l" 'jj-print-line-log)

(global-set-key "\C-jd" 'jj-diff)
(global-set-key "\C-j\C-d" 'jj-diff)

(global-set-key "\C-jf" 'jj-buffer-path)
(global-set-key "\C-j\C-f" 'jj-buffer-path)
