EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} compile
	${MAKE} clean-elc

docs:
	${CASK} exec ${EMACS} -Q -batch \
       om.el -l \
	   test/examples-to-docs.el -l \
       test/examples.el -f create-docs-file

unit:
	${CASK} exec ${EMACS} -Q -batch \
       -l om.el \
       -l test/examples-to-tests.el \
       -l test/examples.el \
       -l test/test-helper.el \
       -l test/om-test.el \
       -f ert-run-tests-batch-and-exit

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile om.el

clean-elc:
	rm -f om.elc

.PHONY:	all test docs unit
