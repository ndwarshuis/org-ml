EMACS ?= emacs -Q --batch --load init.el -L .
RM ?= rm -f

all: test

test:
	${MAKE} unit
	${MAKE} compile

docs:
	${EMACS} -L dev \
		-l dev/org-ml-docs.el \
		-f create-docs-files

internal:
	${EMACS} -L dev \
		-l dev/org-ml-test-internal.el \
        -f buttercup-run-discover

external:
	${EMACS} -L dev \
		-l dev/org-ml-test-external.el \
        -f buttercup-run-discover

unit: 
	${EMACS} -L dev \
		-l dev/org-ml-test-external.el \
		-l dev/org-ml-test-internal.el \
        -f buttercup-run-discover

build:
	${EMACS} -f compile-target

compile:
	${MAKE} build
	${MAKE} unit
	${MAKE} clean-elc

benchmark:
	${EMACS} build
	${EMACS} -L bench \
		-l bench/org-ml-benchmarks.el \
		-f org-ml-bench-run
	${MAKE} clean-elc

clean-elc:
	${RM} *.elc

.PHONY:	all test docs unit
