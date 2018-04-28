SHELL := /bin/bash
EMACS := emacs

all: go-guru.elc go-mode.elc go-rename.elc

clean:
	rm --force --verbose -- *.elc

%.elc: %.el
	$(EMACS) --quick --batch --load=bytecomp \
	  --eval='(setq byte-compile-error-on-warn t)' --directory=. \
	  --funcall=batch-byte-compile $<

.PHONY: all clean
