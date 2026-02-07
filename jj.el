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
  "Get a diff of the current buffer and show it in a new buffer."
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
      (pop-to-buffer bufname))))

(defun jj-root ()
  "Search upwards from cwd for jj root."
  (substring (shell-command-to-string "jj root") 0 -1))

(defun jj-buffer-path ()
  "Get buffer's file path relative to the jj repo root."
  (file-relative-name
   ;; buffer-filename returns an absolute path but not
   ;; necessarily a canonical path in that symlinks are
   ;; not resolved, whereas git-root-dir does implicitly
   ;; resolve symlinks because that's the behavior of `git
   ;; rev-parse --show-toplevel`. To make
   ;; file-relative-name work as expected, call
   ;; file-truename on the buffer name to canonicalize it.
   (file-truename (buffer-file-name))
   (jj-root)))

(defun jj-copy-buffer-path ()
  "Add buffer's file path relative to the jj root to the kill ring."
  (interactive)
  (let ((path (jj-buffer-path)))
	(message path)
	(kill-new path)))

(defun jj-open ()
  (interactive)
  (let* ((filename (read-from-minibuffer "Filename: " ""))
		 (pattern (shell-quote-argument filename)))
	
	(let* ((dir (jj-root))
		   (at-most-two-matches-raw
            ;; Run fd to get at most two matching paths. The idea is
            ;; that if there is exactly one match we should open that
            ;; file directly, but if there are two or more we should
            ;; use dired to allow the user to pick.
            (shell-command-to-string
             (concat "fd --max-results=2 " pattern " " dir)))
           (at-most-two-matches (split-string at-most-two-matches-raw "\n" t))
           (num-match (length at-most-two-matches)))
      (cond
       ;; no matches, display an error
       ((= num-match 0) (message "no matches"))
       ;; only one match, load it
       ((= num-match 1) (find-file (car at-most-two-matches)))
       ;; multiple matches, pass off to find-dired
       (t
		(fd-dired dir pattern))))))

(defun jj-github ()
  "Open a file in the repo on github."
  (interactive)
  (browse-url (shell-command-to-string 
               (concat
                "jjb file-url "
	            (jj-buffer-path)
                " "
                (number-to-string (line-number-at-pos))))))
      
(global-unset-key "\C-j")

(global-set-key "\C-jl" 'jj-print-line-log)
(global-set-key "\C-j\C-l" 'jj-print-line-log)

(global-set-key "\C-jd" 'jj-diff)
(global-set-key "\C-j\C-d" 'jj-diff)

(global-set-key "\C-jf" 'jj-copy-buffer-path)
(global-set-key "\C-j\C-f" 'jj-copy-buffer-path)

(global-set-key "\C-jo" 'jj-open)
(global-set-key "\C-j\C-o" 'jj-open)

(global-set-key "\C-jg" 'jj-github)
(global-set-key "\C-j\C-g" 'jj-github)
