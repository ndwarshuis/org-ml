# om.el

A functional API for org-mode inspired by
[@magnars](https://github.com/magnars)'s
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el) libraries.

# Installation

This package is not yet in MELPA. To install, clone this repository
somewhere into your load path:

```
git clone https://github.com/ndwarshuis/om.el
```

Then require in your emacs config:

```
(require 'om.el)
```

# Motivation

Org-mode comes with a powerful, built-in parse-tree generator
specified in `org-element.el`. The generated parse-tree is simply a
heavily-nested list which can be easily manipulated using (mostly
pure) functional code. This contrasts the majority of functions
normally used to interface with org-mode files, which are imperative
in nature (`org-insert-headine`, `outline-next-heading`, etc) as they
depend on the mutable state of Emacs buffers. In general, functional
code is
([arguably](https://en.wikipedia.org/wiki/Functional_programming#Comparison_to_imperative_programming))
more robust, readable, and testable, especially in use-cases such as
this where a stateless abstract data structure is being transformed
and queried.

The `org-element.el` provides a minimal API for handling this
parse-tree in a functional manner, but lacks many of the higher-level
functions necessary for intuitive, large-scale use. The `om.el`
package is designed to provide this API. Furthermore, it is highly
compatible with the `dash.el` package, which is a generalized
functional library for emacs-lisp.

# Org-Element Overview

Parsing a buffer with the function `org-element-parse-buffer` will
yield a parse-tree composed of leaves and branches.

The leaves can be one of two broad categories:
- Objects: roughly correspond to raw, possibly-formatted text
  (plain-text, timestamp, code, etc)
- Elements: more complex structures, some of which may have objects in
  their properties (clock, planning, src-blocks, etc)

Each leaf is represented by a list where the first member is the type
and the second member is a plist describing the element or object's
properties:

``` emacs-lisp
(type (:prop1 value1 :prop2 value2 ...))
```

The branches with the leaves come in three
broad types:
- Recursive objects: objects that contain other objects
- Object Containers: elements that contain other objects
- Greater elements: elements that contain other elements

Branches of the tree have almost the same list structure as leaves; in
addition to the type and plist, each child is appended at the end:
  
``` emacs-lisp
(type (:prop1 value1 :prop2 value2) child1 child2 ...)
```

In summary, this implies the following hierarchy of types that
can validly fit into each other in the buffer parse-tree:
- Greater Elements
  - Greater Elements
  - Elements
  - Object Containers
    - Objects
    - Recursive Objects
      - Recursive Objects
      - Objects
      
More information and a full list of elements, objects and their
compatible properties can be found in [the org-element API
documentation](https://orgmode.org/worg/dev/org-element-api.html).

# Conventions

### Threading

Each function that operates on an element/object will take the
element/object as its right-most argument. This allows convenient
function chaining using `dash.el`'s right-threading operator (`->>`).
The examples below almost exclusively demonstrate this pattern.
Additionally, the right-argument convention also allows convenient
partial application using `-partial` from `dash.el`.

### Higher-order functions

Higher-order functions (functions that take other functions as
arguments) have two forms. The first takes a (usually unary) function
and applies it:

``` emacs-lisp
(om-elem-map-property :value (lambda (s) (concat "foo" s)) elem)
(om-elem-map-property :value (-partial concat "foo") elem)
```

This can equivalently be written using an anaphoric form where the
original function name is appended with `*`. The symbol `it`
carries the value of the unary argument (unless otherwise specified):

``` emacs-lisp
(om-elem-map-property* :value (concat "foo" it) elem)
```

### Side effect functions

The majority of side-effectual functions are named like
`om-elem-OPERATION-THING-at` where `OPERATION` is some operation to be
performed on `THING` in the current buffer. All these functions take
`point` as one of their arguments to denote where in the buffer to
perform `OPERATION`.

All of these functions have current-point convenience analogues that
are named as `om-elem-OPERATION-this-THING` where `OPERATION` and
`THING` carry the same meaning, but `OPERATION` is done at the current
point and `point` is not an argument to the function.

For the sake of brevity, only the former form of these functions are
given in the examples below.

# Function Summary

[[ function-list ]]

# Function Examples

[[ function-docs ]]

<!-- [[ version ]] -->

# Acknowledgements

- [@magnars](https://github.com/magnars):
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el)
- Devin Townsend: Puppetry
