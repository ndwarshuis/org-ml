EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} compile
	${MAKE} clean-elc

docs:
	${CASK} exec ${EMACS} -Q -batch \
       -l dev/om-dev.el \
       -l dev/om-dev-doc.el \
       -f create-docs-file

unit:
	${CASK} exec ${EMACS} -Q -batch \
       -l dev/om-dev.el \
       -l dev/om-dev-test.el \
       -f ert-run-tests-batch-and-exit

compile:
	${CASK} build

clean-elc:
	${CASK} clean-elc

.PHONY:	all test docs unit
