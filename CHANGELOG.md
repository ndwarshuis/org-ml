# Changelog

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
