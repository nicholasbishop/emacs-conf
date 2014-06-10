(setq mac-command-modifier 'meta)
(setq explicit-bash-args '("-l"))

;; Requires cocoaAspell to be installed
;;
;; Contains magic to fix errors about not finding word lists
;;
;; Also had to set dict-dir to
;; /Library/Application Support/cocoAspell/aspell6-en-6.0-0 in /usr/local/etc/aspell.conf
;;
;; Grr magic.
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
(setq ispell-dictionary-alist
      '((nil
	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
	 ("-B" "-d" "english" "--dict-dir"
	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
	 nil iso-8859-1)))
