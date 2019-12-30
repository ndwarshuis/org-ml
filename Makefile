EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} compile
	${MAKE} clean-elc

docs:
	${CASK} exec ${EMACS} -Q -batch \
       -l test/om-dev.el \
       -l test/om-dev-doc.el \
       -f create-docs-file

unit:
	${CASK} exec ${EMACS} -Q -batch \
       -l test/om-dev.el \
       -l test/om-dev-test.el \
       -f ert-run-tests-batch-and-exit

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile om.el

clean-elc:
	rm -f om.elc

.PHONY:	all test docs unit
