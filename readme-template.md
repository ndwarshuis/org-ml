# org-ml.el ![Github Workflow Status](https://img.shields.io/github/workflow/status/ndwarshuis/om.el/CI)

A functional API for org-mode inspired by
[@magnars](https://github.com/magnars)'s
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el) libraries.

# Installation

This package is not yet in MELPA. To install, clone this repository somewhere
into your load path:

```
git clone https://github.com/ndwarshuis/org-ml.el
```

Then require in your emacs config:

```
(require 'org-ml.el)
```

# Motivation

Org-mode comes with a powerful, built-in parse-tree generator specified in
`org-element.el`. The generated parse-tree is simply a heavily-nested list which
can be easily manipulated using (mostly pure) functional code. This contrasts
the majority of functions normally used to interface with org-mode files, which
are imperative in nature (`org-insert-headine`, `outline-next-heading`, etc) as
they depend on the mutable state of Emacs buffers. In general, functional code
is
([arguably](https://en.wikipedia.org/wiki/Functional_programming#Comparison_to_imperative_programming))
more robust, readable, and testable, especially in use-cases such as this where
a stateless abstract data structure is being transformed and queried.

The `org-element.el` provides a minimal API for handling this parse-tree in a
functional manner, but does not provide higher-level functions necessary for
intuitive, large-scale use. The `org-ml.el` package is designed to provide this
API. Furthermore, it is highly compatible with the `dash.el` package, which is a
generalized functional library for emacs-lisp.

# Org-Element Overview

Parsing a buffer with the function `org-element-parse-buffer` will yield a parse
tree composed of nodes. Nodes have types and properties associated with them.
See [the org-element API
documentation](https://orgmode.org/worg/dev/org-element-api.html#attributes) for
a list of all node types and their properties (also see the [terminology
conventions](#terminology) and [property omissions](#properties) used in this
package).

Each node is represented by a list where the first member is the type and the
second member is a plist describing the node's properties:

``` emacs-lisp
(type (:prop1 value1 :prop2 value2 ...))
```

Node types may be either leaves or branches, where branches may have zero or
more child nodes and leaves may not have child nodes at all. Leaves will always
have lists of the form shown above. Branches, on the other hand, have their
children appended to the end:

``` emacs-lisp
(type (:prop1 value1 :prop2 value2) child1 child2 ...)
```

In addition to leaves and branches, node types can belong to one of
two classes:
- Objects: roughly correspond to raw, possibly-formatted text
- Elements: more complex structures which may be built from objects

Within the branch node types, there are restrictions of which class is allowed
to be a child depending on the type. There are three of these restrictions:
- Branch element with child elements (aka 'greater elements'): these are element
  types that are generally nestable inside one another (eg headlines,
  plain-lists, items)
- Branch elements with child objects (aka 'object containers'): these are
  element types that hold textual information (eg paragraph)
- Branch objects with child objects (aka 'recursive objects'): these are object
  types used primarily for text formating (bold, italic, underline, etc)

Note: it is never allowed for an element type to be a child of a branch object
type.
      
# Conventions

## Terminology

This package takes several deviations from the original terminology found in
`org-element.el`.
- 'node' is used here to describe a vertex in the parse tree, where 'element'
  and 'object' are two classes used to describe said vertex (`org-element.el`
  seems to use 'element' to generally mean 'node' and uses 'object' to further
  specify)
- 'child' and 'children' are used here instead of 'content' and 'contents'
- 'branch' is used here instead of 'container'. Furthermore, 'leaf' is used to
  describe the converse of 'branch' (there does not seem to be an equivalent
  term in `org-element.el`)
- `org-element.el` uses 'attribute(s)' and 'property(ies)' interchangably to
  describe nodes; here only 'property(ies)' is used

## Properties

All properies specified by `org-element.el` are readable by this API (eg one can
query them with functions like `om-get-property`).

The properties `:begin`, `:end`, `:contents-begin`, `:contents-end`, `:parent`,
and `post-affiliated` are not settable by this API as they are not necessary for
manipulating the textual representation of the parse tree. In addition to these,
some properties unique to certain types are not settable for the same reason.
Each type's build function describes the properties that are settable.

## Threading

Each function that operates on an element/object will take the element/object as
its right-most argument. This allows convenient function chaining using
`dash.el`'s right-threading operators (`->>` and `-some->>`). The examples below
almost exclusively demonstrate this pattern. Additionally, the right-argument
convention also allows convenient partial application using `-partial` from
`dash.el`.

## Higher-order functions

Higher-order functions (functions that take other functions as arguments) have
two forms. The first takes a (usually unary) function and applies it:

``` emacs-lisp
(om-map-property :value (lambda (s) (concat "foo" s)) node)
(om-map-property :value (-partial concat "foo") node)
```

This can equivalently be written using an anaphoric form where the original
function name is appended with `*`. The symbol `it` carries the value of the
unary argument (unless otherwise specified):

``` emacs-lisp
(om-map-property* :value (concat "foo" it) node)
```

## Side effect functions

All functions that read and write from buffers are named like
`om-OPERATION-THING-at` where `OPERATION` is some operation to be performed on
`THING` in the current buffer. All these functions take `point` as one of their
arguments to denote where in the buffer to perform `OPERATION`.

All of these functions have current-point convenience analogues that are named
as `om-OPERATION-this-THING` where `OPERATION` and `THING` carry the same
meaning, but `OPERATION` is done at the current point and `point` is not an
argument to the function.

For the sake of brevity, only the former form of these functions are given in
the examples below.

# Function Summary

[[ function-list ]]

# Function Examples

[[ function-docs ]]

<!-- [[ version ]] -->

# Changelog

## 2.0.0 

- Renamed from `om.el` to `org-ml` (org-metalanguage)
- Renamed functions to be more consistent
  - `org-ml-get-headlines` and friends to `org-ml-parse-headlines`
  - `org-ml-do-headlines` and friends to `org-ml-update-headlines`
- Add POSIX ERE-like regexp syntax to `org-ml-match` and friends
- Add affiliated keyword support
- Numerous bug fixes

# Acknowledgements

- Nicolas Goaziou: author of `org-element.el`
- [@magnars](https://github.com/magnars):
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el)
