;; Jump around!

(defconst src-loc-re " \\([^:< ]+\\):\\([0-9]+\\):\\([0-9]+\\)")

(cl-defstruct src-loc path line column)

(defun get-src-loc-from-match (s)
  ;; Get the source location from the regex. The arg is just used for testing,
  ;; where elisp requires the searched string to be passed back into
  ;; `match-string`. Garbage language.
  (make-src-loc
   :path (match-string 1 s)
   :line (string-to-number (match-string 2 s))
   :column (- (string-to-number (match-string 3 s)) 1)))

(defun get-src-loc-from-str (s)
  ;; Just used for testing.
  (string-match src-loc-re s)
  (get-src-loc-from-match s))

;; TODO: figure out how tests are supposed to be written.
(eval-when-compile (require 'cl-lib))
(cl-assert (equal (get-src-loc-from-str
                "frame #35: 0x000055555bb933f4 file_test`Carbon::SemIR::ConstantId Carbon::Check::TryEvalTypedInst<Carbon::SemIR::LookupImplWitness>(eval_context=0x00007fffffff4a00, inst_id=inst6400013F, inst=(kind_ = 92, type_id_ = typeE, arg0_ = 1677721790, arg1_ = 1677721600)) at toolchain/check/eval.cpp:3206:11")
               (make-src-loc
                :path "toolchain/check/eval.cpp"
                :line 3206
                :column 10)))

(defun bishjump ()
  "Make a guess about where to go, then load that file and goto a line therein."
  (interactive)

  ;; Search for a path:line:col regex backwards from current point.
  (save-excursion
    ;; TODO: consider allowing the line and column to be missing.
    (re-search-backward src-loc-re)
    (let ((sl (get-src-loc-from-match nil)))

      ;; TODO: don't load the file if it doesn't exist.
      (find-file (src-loc-path sl))
      (goto-line (src-loc-line sl))
      (move-to-column (src-loc-column sl))

    )))

(global-set-key "\C-c\\" 'bishjump)
