EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} compile

docs:
	${CASK} exec ${EMACS} -Q -L . -L dev -batch \
		-l dev/org-ml-docs.el \
		-f create-docs-files

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

benchmark:
	${CASK} build
	${CASK} exec ${EMACS} -Q -L . -L bench -batch \
		-l bench/org-ml-benchmarks.el \
		-f org-ml-bench-run
	${MAKE} clean-elc

clean-elc:
	${CASK} clean-elc

.PHONY:	all test docs unit
