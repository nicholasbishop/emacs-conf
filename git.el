;; git

(defun git-add-current-buffer ()
  "call 'git add [current-buffer]'"
  
  (interactive)
  (let* ((buffile (buffer-file-name))
		 (output (shell-command-to-string
				  (concat "git add " (buffer-file-name)))))
	(message (if (not (string= output ""))
				 output
			   (concat "Added " buffile)))))
   

(defun git-show-status ()
  "call 'git status'"
  
  (interactive)
  (message
   (shell-command-to-string
	(concat "git status -sb"))))

(defun git-commit ()
  "call 'git commit'"
  
  (interactive)
  (async-shell-command "git commit"))

(defun git-commit-amend ()
  "call 'git commit'"
  
  (interactive)
  (async-shell-command "git commit --amend"))

(defun git-line-revision ()
  "Get revision of current line in current buffer or nil if not committed"

  (let ((rev (substring (shell-command-to-string 
						 (concat "git blame "
								 (buffer-file-name)
								 " -L" (number-to-string
										(line-number-at-pos))
								 ",+1 -l")) 0 40)))
	(if (string= rev "0000000000000000000000000000000000000000")
		nil
	  rev)))

(defun git-print-line-log ()
  "print log of git-line-revision and copy commit hash to kill ring"

  (interactive)
  (let ((commit-hash (git-line-revision)))
	(if commit-hash
		(if (string= "^" (substring commit-hash 0 1))
			(message "Current line is from prehistory")
		  (progn
			;; output log
			(message (shell-command-to-string
					  (concat "git log -1 " commit-hash)))
			;; copy commit hash to kill ring (and truncate for brevity)
			(kill-new (substring commit-hash 0 7))))
	  (message "Current line has not been committed"))))

(defun git-rebase-interactive ()
  "Call 'git rebase -i [arg] using current line's commit's parent as default arg"
  
  (interactive)
  (let* ((commit-hash (git-line-revision))
		 (arg (read-string "Root: " (if commit-hash
										(concat
										 (substring commit-hash 0 7) "~")
									  nil))))
	(async-shell-command (concat "git rebase -i " arg))))

(defun git-rebase-continue ()
  (interactive)
  (async-shell-command "git rebase --continue"))

(defun git-buffer-filename ()
  "Add buffer's file path relative to git root to the kill ring"
  (interactive)
  (let ((path (file-relative-name (buffer-file-name) (git-root-dir))))
	(message path)
	(kill-new path)))
(global-set-key "\C-zf" 'git-buffer-filename)
(global-set-key "\C-z\C-f" 'git-buffer-filename)

;; key bindings, using C-z C-v as prefix
(global-set-key "\C-zvs" 'git-show-status)
(global-set-key "\C-zvcc" 'git-commit)
(global-set-key "\C-zvca" 'git-commit-amend)
(global-set-key "\C-zva" 'git-add-current-buffer)
(global-set-key "\C-zvl" 'git-print-line-log)
(global-set-key "\C-zvrr" 'git-rebase-interactive)
(global-set-key "\C-zvrc" 'git-rebase-continue)

(defun git-root-dir ()
  "Search upwards from cwd for git root"
  (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1))

(require 'grep)

(setq grep-find-ignored-directories (cons "cmake-*" grep-find-ignored-directories))
(setq grep-find-ignored-directories (cons "obj" grep-find-ignored-directories))

;; This stinks, but no idea how to do this otherwise; it's a direct
;; copy-paste of rgrep, but uses git-root-dir rather than
;; read-directory-name for getting the base directory
(defun grep-git-root (regexp &optional files dir confirm)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (git-root-dir))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-directory-p dir) (file-readable-p dir))
      (setq dir default-directory))
    (if (null files)
	(if (not (string= regexp (if (consp grep-find-command)
				     (car grep-find-command)
				   grep-find-command)))
	    (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (require 'find-dired)		; for `find-name-arg'
      (let ((command (grep-expand-template
		      grep-find-template
		      regexp
		      (concat (shell-quote-argument "(")
			      " " find-name-arg " "
			      (mapconcat #'shell-quote-argument
					 (split-string files)
					 (concat " -o " find-name-arg " "))
			      " "
			      (shell-quote-argument ")"))
		      dir
		      (concat
		       (and grep-find-ignored-directories
			    (concat "-type d "
				    (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -path "
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument
						 (concat "*/" ignore)))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (concat "*/"
							      (cdr ignore)))))))
				     grep-find-ignored-directories
				     " -o -path ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o "))
		       (and grep-find-ignored-files
			    (concat (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -name "
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument ignore))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (cdr ignore))))))
				     grep-find-ignored-files
				     " -o -name ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o "))))))
	(when command
	  (if confirm
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-find-history))
	    (add-to-history 'grep-find-history command))
	  (let ((default-directory dir))
	    (compilation-start command 'grep-mode))
	  ;; Set default-directory if we started rgrep in the *grep* buffer.
	  (if (eq next-error-last-buffer (current-buffer))
	      (setq default-directory dir)))))))

(global-set-key "\C-c\C-j" 'grep-git-root)
(global-set-key "\C-cj" 'grep-git-root)

(defvar open-search-git-history "") ;; TODO

;; TODO: at least clean up the ignore directory stuff (cmake-* and obj)
(defun open-search-git-root ()
  (interactive)
  (let* ((filename (read-from-minibuffer "Filename: " ""))
		 (pattern (shell-quote-argument filename)))
	
	;; should be some conditional?
	
	(setq open-search-git-filename filename)

	(let* ((dir (git-root-dir))
		   (find-str (concat "find " dir " -name 'obj' -prune , -iname '" pattern "'"))
		   (num-match (shell-command-to-string (concat find-str " | wc -l"))))
	  (if (string= num-match "1\n")
		  ;; only one match, load it
		  (find-file (substring (shell-command-to-string find-str) 0 -1))
		;; multiple matches, pass off to find-dired
		(find-dired dir
					(concat " -iname 'cmake-*' -prune , -name 'obj' -prune , -iname "
                            pattern))))))

(global-set-key "\C-c\C-f" 'open-search-git-root)
(global-set-key "\C-cf" 'open-search-git-root)
