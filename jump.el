;; Jump around!

(defun bishjump ()
  "Make a guess about where to go, then load that file and goto a line therein."
  (interactive)

  ;; Search for a path:line:col regex backwards from current point.
  (save-excursion
    ;; TODO: consider allowing the line and column to be missing.
    (re-search-backward " \\([^:<]+\\):\\([0-9]+\\):\\([0-9]+\\)")
    (let ((path (match-string 1))
          (line (string-to-number (match-string 2)))
          (column (- (string-to-number (match-string 3)) 1)))

      ;; TODO: don't load the file if it doesn't exist.
      (find-file path)
      (goto-line line)
      (move-to-column column)

    )))

(global-set-key "\C-c\\" 'bishjump)
