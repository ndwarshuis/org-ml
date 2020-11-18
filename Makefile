EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} compile

docs:
	${CASK} exec ${EMACS} -Q -L . -L dev -batch \
		-l dev/org-ml-docs.el \
		-f create-docs-file

internal:
	${CASK} exec buttercup -L . -L dev \
		-l dev/org-ml-test-internal.el

external:
	${CASK} exec buttercup -L . -L dev \
		-l dev/org-ml-test-external.el

unit: 
	${CASK} exec buttercup -L . -L dev \
		-l dev/org-ml-test-external.el \
		-l dev/org-ml-test-internal.el

compile:
	${CASK} build
	${MAKE} unit
	${MAKE} clean-elc

clean-elc:
	${CASK} clean-elc

.PHONY:	all test docs unit
