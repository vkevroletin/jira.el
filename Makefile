EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

docs:
	${CASK} exec ${EMACS} -Q --script bin/docs.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile jira.el

clean-elc:
	rm -f jira.elc

.PHONY: all test docs unit
