# Changelog

## 6.0.0

This is a major update for org 9.7, which has been heavily optimized with a new
syntax tree API. This new version of org-ml takes advantage of this new API
(which is much faster) but also breaks several things.

summary of breaking changes

- `org-ml-planning-*` functions have been removed (they are no longer necessary)
- the supercontents data structure (for
  `org-ml-headline-get/set/map-supercontents) has been updated and rewritten.
  See docstring for `org-ml-headline-get-supercontents` for details. TLDR is
  that it now handles planning and node properties. This was done partly to
  better handle whitespace (which was not done correctly previously) and also to
  take advantage of performance improvements in the headline node type (see
  below for `org-ml-update-supercontents`)
- all previous depreciated functions have been removed
  - and `org-ml-timestamp-get/set-range` have been renamed to
    `org-ml-timestamp-get/set-length`
- `org-ml-parse-habits` has been removed, this is now elegantly handled by
  org-element itself. See `org-ml-timestamp-get/set/map-deadline` instead.
- `org-ml-headline-get/set/map-node-properties` now use a list of string pairs
  like `(KEY VAL)` instead of raw node-property nodes.
- `org-ml-timestamp-set-length` now takes a unit argument
- `org-ml-clone-node` has been removed
- `org-ml-unixtime-to-time-long/short` have been combined into
  `org-ml-unixtime-to-timelist` which takes a flag to determine if the hours and
  minutes should be included

summary of added features

- added higher-level timestamp-diary functions for start and end time
  manipulation (new in org 9.7)
- `org-ml-timestamp-get/set/map-deadline` which manipulates what is commonly
  called "habits" (new in org 9.7)
- `org-ml-update-supercontents` and `org-ml-update-supersection` which are two
  heavily-optimized functions which take advantage of the new lazy evalulation
  in org 9.7; use these to update headline contents without touching the
  headline itself
- memoization for builder functions
- ability to switch between pure and impure evaluation (the latter is faster but
  less safe); see `org-ml-use-impure`

bug fixes and refactorizations

- added missing tests for purity
- fixed many whitespace handling errors
- use conda to pin exact emacs version for local development
- use straight to pin exact versions of all dependencies
- remove lispy dependency

## 5.8.8

- fix list-like syntax in secondary string parsing

## 5.8.7

- use strings for `org-ml-build-property-drawer!` arguments

## 5.8.6

- bugfixes

## 5.8.5

- make docstring clearer

## 5.8.4

- don't parse bold text as a headline

## 5.8.3

- fix typo in README.md

## 5.8.2

- fix blank table cell bug

## 5.8.1

- fix typo

## 5.8.0

- make myers diff algorithm use linear space
- fix a bunch of compiler warnings for emacs 28+

## 5.7.3

- fix leaky abtraction bug

## 5.7.2

- fix incompatibility with org v9.5 (note: 9.5 not fully tested yet)

## 5.7.1

- add `org-ml-remove-parent(s)`

## 5.7.0

- add functions/checks for `org-data` nodes

## 5.6.2

- add explicit test path for emacs 27.2/org-mode 9.4

## 5.6.1

- make `org-ml-from-string` work correctly with all types and inputs

## 5.6.0

- add get/set/map functions for timestamp repeaters and warnings
- add optional switch for habit parsing

## 5.5.4

- fix `org-ml-headline-set-node-property` nil property bug

## 5.5.3

- fix missing zero-length ending timestamps

## 5.5.2

- fix compile warnings for `org-ml-macs.el`

## 5.5.1

- fix potential merge sort stack overflow

## 5.5.0

- reorganized headline/subtree batch functions (and depreciated old names)

## 5.4.3

- fixed nested headline parsing for `org-ml-get-headlines` et al

## 5.4.2

- make indent/outdent/promote/demote functions more accurate/faster
- further optimizations and additional benchmarks

## 5.4.1

- improve performance of string insertion and headline batch updating

## 5.4.0

- add pattern memoization to match function family
- numerous performance enhancements including:
  - remove majority of closures in anaphoric forms (eg `make-byte-code`)
  - implement faster macros for plists
  - improve timestamp processing
  - remove equality checking from `org-ml~update`
  - streamline headline batch functions (eg `org-ml-do-headlines` et al)

## 5.3.0

- add `org-ml-get-properties`
- add `org-ml-item-get/set/map-paragraph`
- make `org-ml-get-all-properties` public
- numerous bug fixes and performance enhancments

## 5.2.0

- add benchmark framework
- add intermediate functions to control Myers diff algorithm application
- fix potential infinite loop using native `equal` function for node comparison

## 5.1.0

- fixed array overflow error in myers diff code
- add affiliated keywords to polymorphic property interface and builder
  functions

## 5.0.2

- fixed active timestamp bug for closed planning nodes
- use Myers diff algorithm for update functions

## 5.0.1

- rearrange reference files
- use buttercup for testing

## 5.0.0

- add robust headline logbook and contents function
- rename indent/unindent functions to better reflect native org function naming
  conventions
- add `org-ml-from-string`
- improve subtree parsing performance
- fix whitespace errors for
  `org-ml-headline-set-planning/node-properties/supercontents` functions

## 4.0.1

- fix `org-ml-parse-this-table-row` and `org-ml-parse-table-row-at`
  beyond first row of table

## 4.0.0

- add `org-ml-get-parents`
- add `org-ml-headline-get-logbook-loose` and `org-ml-headline-get-contents`
- removed old `org-ml-headline-X-logbook` functions and replaced them with
  `org-ml-headline-X-logbook-drawer` which can be made aware of other drawers
  other than "LOGBOOK" or nothing
- add `org-ml-clone-n`

## 3.0.2

- Update dependencies
- Fix bugs

## 3.0.1

- Fix bugs
  - Don't use `nreverse` unless needed
  - Don't crash when `org-ml-headline-get-node-property` should return nil

## 3.0.0

- Update for org-mode 9.3

## 2.0.1

- Fixed byte compile
- Clean up docstrings

## 2.0.0 

- Renamed from `om.el` to `org-ml` (org-metalanguage)
- Renamed functions to be more consistent
  - `org-ml-get-headlines` and friends to `org-ml-parse-headlines`
  - `org-ml-do-headlines` and friends to `org-ml-update-headlines`
- Add POSIX ERE-like regexp syntax to `org-ml-match` and friends
- Add affiliated keyword support
- Numerous bug fixes
