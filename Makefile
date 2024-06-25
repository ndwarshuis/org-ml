EMACS ?= emacs -Q --batch --load init.el -L .
RM ?= rm -f

all: test

# run all tests with both interpreted and compiled org-ml functions
test:
	${MAKE} unit
	${MAKE} compile

# build docs for org-ml
docs:
	${EMACS} -L dev \
		-l dev/org-ml-docs.el \
		-f create-docs-files

# run internal (stateless) tests with interpreted org-ml functions
internal:
	${EMACS} -L dev \
		-l dev/org-ml-test-internal.el \
        -f buttercup-run-discover

# run external (stateful) tests with interpreted org-ml functions
external:
	${EMACS} -L dev \
		-l dev/org-ml-test-external.el \
        -f buttercup-run-discover

# run tests with interpreted org-ml functions
unit: 
	${EMACS} -L dev \
		-l dev/org-ml-test-external.el \
		-l dev/org-ml-test-internal.el \
        -f buttercup-run-discover


# run tests with compiled org-ml functions
compile:
	${MAKE} build
	${MAKE} unit
	${MAKE} clean-elc

# run and print benchmark results using compile org-ml functions
benchmark:
	${EMACS} build
	${EMACS} -L bench \
		-l bench/org-ml-benchmarks.el \
		-f org-ml-bench-run
	${MAKE} clean-elc

# remove compiled lisp files
clean-elc:
	${RM} *.elc

# byte-compile all org-ml lisp files
build:
	${EMACS} -f compile-target

# install all development packages for the current version
install:
	${EMACS} --eval '(print "Install finished")'

# write lockfile for current emacs version given each repo dependency
freeze:
	${EMACS} -f straight-freeze-versions

thaw:
	${EMACS} -f straight-thaw-versions

.PHONY:	all test docs unit
