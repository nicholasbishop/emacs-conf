;; generate pdf output
;; creates also a fake dvi file so that tex-dvi-view-command doesn't choke
(setq latex-run-command
	  "(TEXFILE=*; pdflatex $TEXFILE && cp ${TEXFILE%.tex}.pdf ${TEXFILE%.tex}.dvi)")

;; xdvi is crap, use evince instead
(setq tex-dvi-view-command "(DVIFILE=*; evince ${DVIFILE%.dvi}.pdf)")
