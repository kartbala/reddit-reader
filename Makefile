EMACS ?= emacs
LOAD = -L emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch $(LOAD) \
	  -l ert -l tests/elisp/test-reddit-reader.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch $(LOAD) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile emacs/reddit-reader.el

clean:
	rm -f emacs/*.elc
