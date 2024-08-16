# org-ml ![CI](https://github.com/ndwarshuis/org-ml/actions/workflows/test.yml/badge.svg) ![MELPA VERSION](https://melpa.org/packages/org-ml-badge.svg)

A functional API for org-mode inspired by
[@magnars](https://github.com/magnars)'s
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el) libraries.

# Installation

Install from MELPA:

```
M-x package-install RET org-ml RET
```

Alternatively, clone this repository to somewhere in your load path:

```
git clone https://github.com/ndwarshuis/org-ml ~/somewhere/in/load/path
```

Then require in your emacs config:

```
(require 'org-ml)
```

## Dependencies

- emacs (28.2, 29.3)
- org-mode (9.7.9)
- dash (2.17.0)
- s (1.13.0)

Explicit versions noted above have been tested. Other versions may work but are
not currently supported.

Notably, *only* org 9.7.x and above will work (9.6 and below will absolutely
break).

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
intuitive, large-scale use. The `org-ml` package is designed to provide this
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

- 'node' is a vertex in the parse tree, where 'element' and 'object' are two
  classes used to describe said vertex
- 'child' and 'children' are used here instead of 'content' and 'contents'
- 'branch' is a node that has or can have other nodes in it (`org-element`
  mostly uses 'container' to describe these)
- 'leaf' is a node without other nodes in it (opposite of branch)

## Properties

All properties specified by `org-element.el` are readable by this API (eg one
can query them with functions like `org-ml-get-property`).

The properties `:begin`, `:end`, `:contents-begin`, `:contents-end`, `:parent`,
and `post-affiliated` are not settable by this API as they are not necessary for
manipulating the textual representation of the parse tree. In addition to these,
some properties unique to certain types are not settable for the same reason.
Each type's build function (`org-ml-build-X`) describes the properties that are
settable.

See `org-ml-remove-parent` and `org-ml-remove-parents` for specific information
and functions regarding the `:parent` property, why it can be annoying, and when
you would want to remove it.

## Threading

Each function that operates on an element/object will take the element/object as
its right-most argument. This allows convenient function chaining using
`dash.el`'s right-threading operators (`->>` and `-some->>`). The examples in
the [API reference](docs/api-reference.md) almost exclusively demonstrate this
pattern. Additionally, the right-argument convention also allows convenient
partial application using `-partial` from `dash.el`.

## Higher-order functions

Higher-order functions (functions that take other functions as arguments) have
two forms. The first takes a (usually unary) function and applies it:

``` emacs-lisp
(org-ml-map-property :value (lambda (s) (concat "foo" s)) node)
(org-ml-map-property :value (-partial concat "foo") node)
```

This can equivalently be written using an anaphoric form where the original
function name is appended with `*`. The symbol `it` carries the value of the
unary argument (unless otherwise specified):

``` emacs-lisp
(org-ml-map-property* :value (concat "foo" it) node)
```

## Side effect functions

All functions that read and write from buffers are named like
`org-ml-OPERATION-THING-at` where `OPERATION` is some operation to be performed on
`THING` in the current buffer. All these functions take `point` as one of their
arguments to denote where in the buffer to perform `OPERATION`.

All of these functions have current-point convenience analogues that are named
as `org-ml-OPERATION-this-THING` where `OPERATION` and `THING` carry the same
meaning, but `OPERATION` is done at the current point and `point` is not an
argument to the function.

For the sake of brevity, only the former form of these functions are given in
the [API reference](docs/api-reference.md).

# Usage

For comprehensive documentation of all available functions see the [API
reference](docs/api-reference.md).

## Habits

Since org 9.7, habits are stored in `:repeater-deadline-unit` and
`:repeater-deadline-value` of `timestamp` nodes. "Deadline" refers to the last
bit in the repeater of a timestamp (ie the "3d" in "[2019-01-01 Tue 12:00
+1d/3d]").

See `org-ml-timestamp-get/set/map-deadline` to access and manipulate these.

# Performance

Benchmarking this library is still in the early stages.

Intuitively, the most costly operations are going to be those that go
back-and-forth between raw buffer text (here called "buffer space") and its node
representations (here called "node space") since those involve complicated
string formating, regular expressions, buffer searching, etc (examples:
`org-ml-parse-this-THING`, `org-ml-update-this-THING` and friends). Once the
data is in node space, execution should be very fast since nodes are just lists.
Thus if you have performance-intensive code that requires many small edits to
org-mode files, it might be better to use org-mode's built-in functions. On the
other hand, if most of the complicated processing can be done in node space
while limiting the number of conversions to/from buffer space, `org-ml` will be
much faster.

To be more scientific, the current tests in the suite (see
[here](bench/org-ml-benchmarks.el)) seem to support the following conclusions
when comparing `org-ml` to equivalent code written using built-in org-mode
functions (in line with the intuitions above):
* reading data (a one way conversion from buffer to node space) is up to an
  order of magnitude slower, specifically when the data to be obtained isn't
  very large (eg, reading the TODO state from a headline)
* text manipulations can be update to 10x slower *or faster* depending on what
  they are:
  * large edits like headline level changing are slower in `org-ml`
  * updating headline todo and tags are faster in `org-ml`
  * complex operations that involve lots of different functions tend to be
    faster in `org-ml` (since there are more list operations vs buffer edits)
  * changing the contents of headlines can be as fast or faster in `org-ml`,
    especially when using memoization and `org-ml-update-supercontents` (see
    below).

To run the benchmark suite:

``` sh
make benchmark
```

## Deferred Properties

### Overview

Starting with org 9.7, `org-element`'s abstract syntax tree uses lazy evaluation
for several text-heavy operations. Thus the tree that `org-ml` consumes may have
unevaluated (aka "deferred") properties in it. For the most part, this will not
affect user experience, but understanding this will help in optimizing
performance, as preventing lazy properties from being unnecessarily resolved
will lead to significant performance gains.

As of version 9.7.9, the properties which are deferred are:
* most properties in headlines (all except for :pre-blank and the properties in
  `org-element--standard-properties`)
* the :value property for code and verbatim nodes
   
Since most of the deferred properties are in headlines, and because headlines
are so prevalent, the remainder of this discussion will focus on headlines.

Accessing any deferred property in a headline will trigger that property to be
resolved, which is slow (as of 9.7.9 this often results in multiple properties
being resolved at once due to the interconnected nature of how a headilne is
parsed). In `org-ml` this means using `org-ml-get-property` or similar, as well
as `org-ml-to-string` which necessarily needs to read all properties to create a
string. Setting a property will resolve all properties, since (as noted above)
many deferred headline properties depend on others.

### Optimizations in org-ml

With regard to buffer editing (ie `org-ml-update-X` functions) this also means
that any operation that does *not* edit the headline itself can be much faster
under this new lazy paradigm. Examples of this include updating CLOSED or
SCHEDULED timestamps, editing the logbook, adding properties like Effort, or
adding other contents between these and the next headline. Unlike previous
versions of `org-ml` and `org` manipulating these would have involved parsing
the headline, parsing the stuff inside the headline, editing the stuff inside
the headline, then writing out a new headline. In 9.7, we can bypass most of the
headline parsing in this situation.

The functions to do this are `org-ml-update-supercontents` and
`org-ml-update-supersection`. Both are only meant to edit the section underneath
the headlines in the buffer, and will not touch the headline itself. This takes
advantage of the new lazy evaluation system. These functions create an
abstraction over the contents of the headline that can be manipulated in a sane
way (see their docstrings for details).

There is one important caveat; if one changes the whitespace immediately after
the headline, this likely will change the :pre-blank property of the headline
which will require the headline to be rewritten (and resolved) which negates
this performance benefit. However, these functions are smart enough to figure
out when :pre-blank is changed.

### Other Considerations

Because lazy evaluation defers parsing the buffer, this assumes that the buffer
will not be edited in between the time the org-element syntax tree is created
and accessing any deferred properties. By extension it assumes the buffer is not
entirely destroyed (which is probably when dealing with temp buffers).

If one expects that the buffer will not retain state prior to accessing deferred
properties, use `org-element-properties-resolve` (which will resolve deferred
properties in place) or either `org-element-copy` or `org-ml-copy` which will
resolve deferred properties and copy the entire node (see more below).

## Node Copying

To maintain functional purity, all public-facing functions in `org-ml` that
modify nodes should return a copy. This way, modifications to the returned node
will not "go backward" to the original input node.

However, making copies can be slow. It also might be unnecessary within a
pipeline (usually with the threading macros `->` and `->>` from `dash.el`)
since the intermediate values are not bound to any variable, which leaves no
opportunity for accidental side-effect leakage.

To solve this use-case, `org-ml` has the following specialized threading macros:

- `org-ml->`
- `org-ml->>`
- `org-ml-->`
- `org-ml-some->`
- `org-ml-some->>`
- `org-ml-some-->`

These correspond to the sans-`org-ml` macros from `dash.el`

The `org-ml` versions will set `org-ml-use-impure` to t, which will turn off all
copying within the pipeline. (see `org-ml-copy` which is a thin wrapper around
`org-element-copy` with this switch built-in).

Note that the performance benefits of this are significant but modest (5-10%
depending on the complexity of the operation), and this comes with a significant
cost of reduced safety since it breaks the functional paradigm. Weight this
accordingly.

## Memoization

### Build Functions

Node building (functions like `org-ml-build-*`) is a pure operation (ie the
result only depends on the inputs). Furthermore, it is used in many places,
including internally to `org-ml` itself.

Therefore, memoizing these functions can produce significant performance gains.

To turn this on globally, set `org-ml-memoize-builders` to `t`. This will
memoize all leaf node builders by default (as it is assumed that any branch
nodes will be sufficiently complicated that most will be unique and therefore
miss the cache). For more fine-grained control over which nodes are memoized,
see `org-ml-memoize-builder-types`.

#### Shorthand Builders

There is an analogous optimization for 'shorthand' builders (functions like
`org-ml-build-*`) which use simplified inputs. These are controled by
`org-ml-memoize-shorthand-builders` and
`org-ml-memoize-shorthand-builder-types`. These will by default memoize all
shorthand builders except those for item and headline, for similar reasons to
above.

### Match Patterns

For all pattern-matching functions (eg `org-ml-match` and `org-ml-match-X`), the
`PATTERN` parameter is processed into a lambda function which computationally
carries out the pattern matching. If there are many calls using the same or a
few unique patterns, this lambda-generation overhead may be memoized by setting
`org-ml-memoize-match-patterns`. See this varible's documentation for details.

## Other potential optimizations

These are some ideas that may be implemented later depending on how much they
matter.

### Tree-based Diff Algorithm

It makes sense to only update the parts of a buffer that actually change.
However, this is complicated to do in practice.

Current versions of `org-ml` can use the Myers Diff Algorithm (the thing that
powers the `diff` program) to only edit the buffer contents that change (see
`org~ml-update`). This can have some speedup since buffer editing is somewhat
expensive. The obvious tradeoff is the algorithm itself needs to be performed
prior to the edit, and its complexity is quadratic.

The problem with this algorithm is that it only works on strings, thus the
org-element tree needs to be interpreted for this to be used. Not only is this
inherently expensive, it also negates any of the defferred property enhancements
that come with 9.7.

The (potential) solution is to implement a tree-based version of the Myers Diff
algorithm that works directly on the org-element tree. The result would be a
list of nodes to be inserted/deleted at a given position.

This would potentially have a huge benefit for deeply-nested edits, which often
happen in property drawers, logbooks, clocking entries, lists, etc.

### Lazy Evaluation for Supercontents Functions

The functions `org-ml-get/set/map-supercontents` (and related) all operate on a
complicated abstraction over a headline's section nodes. While this makes many
operations easy and convenient, it has the drawback of converting the entire
section even if only a small part needs to be changed. Making some parts of this
data structure lazy could make this faster.

This most obviously matters for cases where one wants to edit the planning or
property nodes of a headline which also has a massive logbook or a lot of
clocks. Currently the entire logbook, clocks, etc, will be processed, despite
a tiny unrelated node actually being updated.

# Development

For most stable results, make sure you have a working conda or mamba
installation. Conda is not strictly needed, but reproducible testing results are
not guaranteed.

Begin by creating a testing environment using the provided env* files (with the
desired version):

```
mamba env create -f env-XX.Y.yml
conda activate org-ml-XX.Y
```

Install all dependencies:

```
make install
```

Run all tests:

```
make unit
```

Run all tests with compilation:

```
make compile
```

To update a dependency, navidate to the `.emacs/XX.Y/straight/repos/<dep>`
directory (after installation) and run `git reset --hard <ref>` (after fetch if
needed) to pull the desired git state. Then run:

```
make freeze
```

Which will update `.emacs/XX.Y/straight/versions/default.el`

If any of the above make commands fail with: `undefined symbol:
malloc_set_state` or similar, try the following:

```
export LD_PRELOAD=/usr/lib/libc_malloc_debug.so
```

# Version History

See [changelog](CHANGELOG.md).

# Acknowledgments

- Ihor Radchenko: author or `org-element-ast.el`
- Nicolas Goaziou: author of `org-element.el`
- [@magnars](https://github.com/magnars):
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el)
