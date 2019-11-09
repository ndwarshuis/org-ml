#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

if [ -z "$MAKEINFO" ] ; then
    MAKEINFO="makeinfo"
fi

$EMACS -batch -l om.el -l om-elem.el -l test/examples-to-docs.el -l test/examples.el -f create-docs-file
# $EMACS -batch -l om.el -l dev/examples-to-info.el -l dev/examples.el -f create-info-file
# $MAKEINFO --fill-column=70 dash.texi
