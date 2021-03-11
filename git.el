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
			;; copy commit hash to kill ring
			(kill-new commit-hash)))
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
  (let ((path (file-relative-name
               ;; buffer-filename returns an absolute path but not
               ;; necessarily a canonical path in that symlinks are
               ;; not resolved, whereas git-root-dir does implicitly
               ;; resolve symlinks because that's the behavior of `git
               ;; rev-parse --show-toplevel`. To make
               ;; file-relative-name work as expected, call
               ;; file-truename on the buffer name to canonicalize it.
               (file-truename (buffer-file-name))
               (git-root-dir))))
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

;; This is annoying copy paste, but no idea how to do this otherwise;
;; it's a direct copy-paste of ripgrep-regexp, but uses git-root-dir
;; rather than read-directory-name for getting the base directory.
(defun grep-git-root (regexp directory &optional args)
  "Run a ripgrep search with `REGEXP' at the root of the git repo.
`ARGS' provides Ripgrep command line arguments."
  (interactive
   (list (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))
         (git-root-dir)))
  (let ((default-directory directory))
    (compilation-start
     (mapconcat 'identity
                (append (list ripgrep-executable)
                        ripgrep-arguments
                        args
                        ripgrep--base-arguments
                        (when ripgrep-highlight-search '("--color=always"))
                        (when (and case-fold-search
                                   (isearch-no-upper-case-p regexp t))
                          '("--ignore-case"))
                        '("--")
                        (list (shell-quote-argument regexp) ".")) " ")
     'ripgrep-search-mode)))

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
