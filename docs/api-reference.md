# API Reference

## String Conversion


Convert nodes to strings.

* [org-ml-to-string](#org-ml-to-string-node) `(node)`
* [org-ml-to-trimmed-string](#org-ml-to-trimmed-string-node) `(node)`
* [org-ml-from-string](#org-ml-from-string-type-string) `(type string)`

## Buffer Parsing


Parse buffers to trees.

* [org-ml-parse-object-at](#org-ml-parse-object-at-point) `(point)`
* [org-ml-parse-element-at](#org-ml-parse-element-at-point) `(point)`
* [org-ml-parse-table-row-at](#org-ml-parse-table-row-at-point) `(point)`
* [org-ml-parse-headline-at](#org-ml-parse-headline-at-point) `(point)`
* [org-ml-parse-subtree-at](#org-ml-parse-subtree-at-point) `(point)`
* [org-ml-parse-item-at](#org-ml-parse-item-at-point) `(point)`
* [org-ml-parse-section-at](#org-ml-parse-section-at-point) `(point)`
* [org-ml-parse-this-toplevel-section](#org-ml-parse-this-toplevel-section-nil) `nil`
* [org-ml-this-buffer-has-headlines](#org-ml-this-buffer-has-headlines-nil) `nil`
* [org-ml-parse-headlines](#org-ml-parse-headlines-which) `(which)`
* [org-ml-parse-subtrees](#org-ml-parse-subtrees-which) `(which)`

## Building


Build new nodes.


### Leaf Object Nodes

* [org-ml-build-code](#org-ml-build-code-value-key-post-blank) `(value &key post-blank)`
* [org-ml-build-entity](#org-ml-build-entity-name-key-use-brackets-p-post-blank) `(name &key use-brackets-p post-blank)`
* [org-ml-build-export-snippet](#org-ml-build-export-snippet-back-end-value-key-post-blank) `(back-end value &key post-blank)`
* [org-ml-build-inline-babel-call](#org-ml-build-inline-babel-call-call-key-inside-header-arguments-end-header-post-blank) `(call &key inside-header arguments end-header post-blank)`
* [org-ml-build-inline-src-block](#org-ml-build-inline-src-block-language-key-parameters-value--post-blank) `(language &key parameters (value "") post-blank)`
* [org-ml-build-line-break](#org-ml-build-line-break-key-post-blank) `(&key post-blank)`
* [org-ml-build-latex-fragment](#org-ml-build-latex-fragment-value-key-post-blank) `(value &key post-blank)`
* [org-ml-build-macro](#org-ml-build-macro-key-key-args-post-blank) `(key &key args post-blank)`
* [org-ml-build-statistics-cookie](#org-ml-build-statistics-cookie-value-key-post-blank) `(value &key post-blank)`
* [org-ml-build-target](#org-ml-build-target-value-key-post-blank) `(value &key post-blank)`
* [org-ml-build-timestamp](#org-ml-build-timestamp-type-year-start-month-start-day-start-year-end-month-end-day-end-key-hour-start-minute-start-hour-end-minute-end-repeater-type-repeater-unit-repeater-value-warning-type-warning-unit-warning-value-post-blank) `(type year-start month-start day-start year-end month-end day-end &key hour-start minute-start hour-end minute-end repeater-type repeater-unit repeater-value warning-type warning-unit warning-value post-blank)`
* [org-ml-build-verbatim](#org-ml-build-verbatim-value-key-post-blank) `(value &key post-blank)`

### Branch Object Nodes

* [org-ml-build-bold](#org-ml-build-bold-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`
* [org-ml-build-footnote-reference](#org-ml-build-footnote-reference-key-label-post-blank-rest-object-nodes) `(&key label post-blank &rest object-nodes)`
* [org-ml-build-italic](#org-ml-build-italic-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`
* [org-ml-build-link](#org-ml-build-link-path-key-format-type-fuzzy-post-blank-rest-object-nodes) `(path &key format (type "fuzzy") post-blank &rest object-nodes)`
* [org-ml-build-radio-target](#org-ml-build-radio-target-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`
* [org-ml-build-strike-through](#org-ml-build-strike-through-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`
* [org-ml-build-superscript](#org-ml-build-superscript-key-use-brackets-p-post-blank-rest-object-nodes) `(&key use-brackets-p post-blank &rest object-nodes)`
* [org-ml-build-subscript](#org-ml-build-subscript-key-use-brackets-p-post-blank-rest-object-nodes) `(&key use-brackets-p post-blank &rest object-nodes)`
* [org-ml-build-table-cell](#org-ml-build-table-cell-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`
* [org-ml-build-underline](#org-ml-build-underline-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`

### Leaf Element Nodes

* [org-ml-build-babel-call](#org-ml-build-babel-call-call-key-inside-header-arguments-end-header-name-plot-header-results-caption-post-blank) `(call &key inside-header arguments end-header name plot header results caption post-blank)`
* [org-ml-build-clock](#org-ml-build-clock-value-key-post-blank) `(value &key post-blank)`
* [org-ml-build-comment](#org-ml-build-comment-value-key-post-blank) `(value &key post-blank)`
* [org-ml-build-comment-block](#org-ml-build-comment-block-key-value--name-plot-header-results-caption-post-blank) `(&key (value "") name plot header results caption post-blank)`
* [org-ml-build-diary-sexp](#org-ml-build-diary-sexp-key-value-name-plot-header-results-caption-post-blank) `(&key value name plot header results caption post-blank)`
* [org-ml-build-example-block](#org-ml-build-example-block-key-preserve-indent-switches-value--name-plot-header-results-caption-post-blank) `(&key preserve-indent switches (value "") name plot header results caption post-blank)`
* [org-ml-build-export-block](#org-ml-build-export-block-type-value-key-name-plot-header-results-caption-post-blank) `(type value &key name plot header results caption post-blank)`
* [org-ml-build-fixed-width](#org-ml-build-fixed-width-value-key-name-plot-header-results-caption-post-blank) `(value &key name plot header results caption post-blank)`
* [org-ml-build-horizontal-rule](#org-ml-build-horizontal-rule-key-name-plot-header-results-caption-post-blank) `(&key name plot header results caption post-blank)`
* [org-ml-build-keyword](#org-ml-build-keyword-key-value-key-name-plot-header-results-caption-post-blank) `(key value &key name plot header results caption post-blank)`
* [org-ml-build-latex-environment](#org-ml-build-latex-environment-value-key-name-plot-header-results-caption-post-blank) `(value &key name plot header results caption post-blank)`
* [org-ml-build-node-property](#org-ml-build-node-property-key-value-key-post-blank) `(key value &key post-blank)`
* [org-ml-build-planning](#org-ml-build-planning-key-closed-deadline-scheduled-post-blank) `(&key closed deadline scheduled post-blank)`
* [org-ml-build-src-block](#org-ml-build-src-block-key-value--language-parameters-preserve-indent-switches-name-plot-header-results-caption-post-blank) `(&key (value "") language parameters preserve-indent switches name plot header results caption post-blank)`

### Branch Element Nodes with Child Object Nodes

* [org-ml-build-paragraph](#org-ml-build-paragraph-key-name-plot-header-results-caption-post-blank-rest-object-nodes) `(&key name plot header results caption post-blank &rest object-nodes)`
* [org-ml-build-table-row](#org-ml-build-table-row-key-post-blank-rest-object-nodes) `(&key post-blank &rest object-nodes)`
* [org-ml-build-verse-block](#org-ml-build-verse-block-key-name-plot-header-results-caption-post-blank-rest-object-nodes) `(&key name plot header results caption post-blank &rest object-nodes)`

### Branch Element Nodes with Child Element Nodes

* [org-ml-build-org-data](#org-ml-build-org-data-rest-headline-or-sections-nodes) `(&rest headline-or-sections-nodes)`
* [org-ml-build-center-block](#org-ml-build-center-block-key-name-plot-header-results-caption-post-blank-rest-element-nodes) `(&key name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-drawer](#org-ml-build-drawer-drawer-name-key-name-plot-header-results-caption-post-blank-rest-element-nodes) `(drawer-name &key name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-dynamic-block](#org-ml-build-dynamic-block-block-name-key-arguments-name-plot-header-results-caption-post-blank-rest-element-nodes) `(block-name &key arguments name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-footnote-definition](#org-ml-build-footnote-definition-label-key-pre-blank-0-name-plot-header-results-caption-post-blank-rest-element-nodes) `(label &key (pre-blank 0) name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-headline](#org-ml-build-headline-key-archivedp-commentedp-footnote-section-p-level-1-pre-blank-0-priority-tags-title-todo-keyword-post-blank-rest-element-nodes) `(&key archivedp commentedp footnote-section-p (level 1) (pre-blank 0) priority tags title todo-keyword post-blank &rest element-nodes)`
* [org-ml-build-item](#org-ml-build-item-key-bullet---pre-blank-0-checkbox-counter-tag-post-blank-rest-element-nodes) `(&key (bullet '-) (pre-blank 0) checkbox counter tag post-blank &rest element-nodes)`
* [org-ml-build-plain-list](#org-ml-build-plain-list-key-name-plot-header-results-caption-post-blank-rest-element-nodes) `(&key name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-property-drawer](#org-ml-build-property-drawer-key-post-blank-rest-element-nodes) `(&key post-blank &rest element-nodes)`
* [org-ml-build-quote-block](#org-ml-build-quote-block-key-name-plot-header-results-caption-post-blank-rest-element-nodes) `(&key name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-section](#org-ml-build-section-key-post-blank-rest-element-nodes) `(&key post-blank &rest element-nodes)`
* [org-ml-build-special-block](#org-ml-build-special-block-type-key-name-plot-header-results-caption-post-blank-rest-element-nodes) `(type &key name plot header results caption post-blank &rest element-nodes)`
* [org-ml-build-table](#org-ml-build-table-key-tblfm-name-plot-header-results-caption-post-blank-rest-element-nodes) `(&key tblfm name plot header results caption post-blank &rest element-nodes)`

### Miscellaneous Builders

* [org-ml-clone-node](#org-ml-clone-node-node) `(node)`
* [org-ml-clone-node-n](#org-ml-clone-node-n-n-node) `(n node)`
* [org-ml-build-secondary-string!](#org-ml-build-secondary-string-string) `(string)`
* [org-ml-build-table-row-hline](#org-ml-build-table-row-hline-key-post-blank) `(&key post-blank)`
* [org-ml-build-timestamp-diary](#org-ml-build-timestamp-diary-form-key-post-blank) `(form &key post-blank)`

### Shorthand Builders


Build nodes with more convenient/shorter syntax.

* [org-ml-build-timestamp!](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank) `(start &key end active repeater warning post-blank)`
* [org-ml-build-clock!](#org-ml-build-clock-start-key-end-post-blank) `(start &key end post-blank)`
* [org-ml-build-planning!](#org-ml-build-planning-key-closed-deadline-scheduled-post-blank) `(&key closed deadline scheduled post-blank)`
* [org-ml-build-property-drawer!](#org-ml-build-property-drawer-key-post-blank-rest-keyvals) `(&key post-blank &rest keyvals)`
* [org-ml-build-headline!](#org-ml-build-headline-key-level-1-title-text-todo-keyword-tags-pre-blank-priority-commentedp-archivedp-post-blank-planning-statistics-cookie-section-children-rest-subheadlines) `(&key (level 1) title-text todo-keyword tags pre-blank priority commentedp archivedp post-blank planning statistics-cookie section-children &rest subheadlines)`
* [org-ml-build-item!](#org-ml-build-item-key-post-blank-bullet-checkbox-tag-paragraph-counter-rest-children) `(&key post-blank bullet checkbox tag paragraph counter &rest children)`
* [org-ml-build-paragraph!](#org-ml-build-paragraph-string-key-post-blank) `(string &key post-blank)`
* [org-ml-build-table-cell!](#org-ml-build-table-cell-string) `(string)`
* [org-ml-build-table-row!](#org-ml-build-table-row-row-list) `(row-list)`
* [org-ml-build-table!](#org-ml-build-table-key-tblfm-post-blank-rest-row-lists) `(&key tblfm post-blank &rest row-lists)`

### Logbook Item Builders


Build item nodes for inclusion in headline logbooks

* [org-ml-build-log-note](#org-ml-build-log-note-unixtime-note) `(unixtime note)`
* [org-ml-build-log-done](#org-ml-build-log-done-unixtime-optional-note) `(unixtime &optional note)`
* [org-ml-build-log-refile](#org-ml-build-log-refile-unixtime-optional-note) `(unixtime &optional note)`
* [org-ml-build-log-state](#org-ml-build-log-state-unixtime-new-state-old-state-optional-note) `(unixtime new-state old-state &optional note)`
* [org-ml-build-log-deldeadline](#org-ml-build-log-deldeadline-unixtime-old-timestamp-optional-note) `(unixtime old-timestamp &optional note)`
* [org-ml-build-log-delschedule](#org-ml-build-log-delschedule-unixtime-old-timestamp-optional-note) `(unixtime old-timestamp &optional note)`
* [org-ml-build-log-redeadline](#org-ml-build-log-redeadline-unixtime-old-timestamp-optional-note) `(unixtime old-timestamp &optional note)`
* [org-ml-build-log-reschedule](#org-ml-build-log-reschedule-unixtime-old-timestamp-optional-note) `(unixtime old-timestamp &optional note)`
* [org-ml-build-log-type](#org-ml-build-log-type-type-key-old-new-unixtime-username-full-username-note) `(type &key old new unixtime username full-username note)`

## Type Predicates


Test node types.

* [org-ml-get-type](#org-ml-get-type-node) `(node)`
* [org-ml-is-type](#org-ml-is-type-type-node) `(type node)`
* [org-ml-is-any-type](#org-ml-is-any-type-types-node) `(types node)`
* [org-ml-is-element](#org-ml-is-element-node) `(node)`
* [org-ml-is-branch-node](#org-ml-is-branch-node-node) `(node)`
* [org-ml-node-may-have-child-objects](#org-ml-node-may-have-child-objects-node) `(node)`
* [org-ml-node-may-have-child-elements](#org-ml-node-may-have-child-elements-node) `(node)`

## Property Manipulation


Set, get, and map properties of nodes.


### Generic

* [org-ml-contains-point-p](#org-ml-contains-point-p-point-node) `(point node)`
* [org-ml-set-property](#org-ml-set-property-prop-value-node) `(prop value node)`
* [org-ml-get-property](#org-ml-get-property-prop-node) `(prop node)`
* [org-ml-map-property](#org-ml-map-property-prop-fun-node) `(prop fun node)`
* [org-ml-toggle-property](#org-ml-toggle-property-prop-node) `(prop node)`
* [org-ml-shift-property](#org-ml-shift-property-prop-n-node) `(prop n node)`
* [org-ml-insert-into-property](#org-ml-insert-into-property-prop-index-string-node) `(prop index string node)`
* [org-ml-remove-from-property](#org-ml-remove-from-property-prop-string-node) `(prop string node)`
* [org-ml-plist-put-property](#org-ml-plist-put-property-prop-key-value-node) `(prop key value node)`
* [org-ml-plist-remove-property](#org-ml-plist-remove-property-prop-key-node) `(prop key node)`
* [org-ml-get-properties](#org-ml-get-properties-props-node) `(props node)`
* [org-ml-get-all-properties](#org-ml-get-all-properties-node) `(node)`
* [org-ml-set-properties](#org-ml-set-properties-plist-node) `(plist node)`
* [org-ml-map-properties](#org-ml-map-properties-plist-node) `(plist node)`
* [org-ml-get-parents](#org-ml-get-parents-node) `(node)`
* [org-ml-remove-parent](#org-ml-remove-parent-node) `(node)`

### Clock

* [org-ml-clock-is-running](#org-ml-clock-is-running-clock) `(clock)`

### Entity

* [org-ml-entity-get-replacement](#org-ml-entity-get-replacement-key-entity) `(key entity)`

### Headline

* [org-ml-headline-set-title!](#org-ml-headline-set-title-title-text-stats-cookie-value-headline) `(title-text stats-cookie-value headline)`
* [org-ml-headline-is-done](#org-ml-headline-is-done-headline) `(headline)`
* [org-ml-headline-has-tag](#org-ml-headline-has-tag-tag-headline) `(tag headline)`
* [org-ml-headline-get-statistics-cookie](#org-ml-headline-get-statistics-cookie-headline) `(headline)`

### Item

* [org-ml-item-toggle-checkbox](#org-ml-item-toggle-checkbox-item) `(item)`

### Planning

* [org-ml-planning-set-timestamp!](#org-ml-planning-set-timestamp-prop-planning-list-planning) `(prop planning-list planning)`

### Statistics Cookie

* [org-ml-statistics-cookie-is-complete](#org-ml-statistics-cookie-is-complete-statistics-cookie) `(statistics-cookie)`

### Timestamp (Auxiliary)


Functions to work with timestamp data

* [org-ml-time-is-long](#org-ml-time-is-long-time) `(time)`
* [org-ml-time-to-unixtime](#org-ml-time-to-unixtime-time) `(time)`
* [org-ml-unixtime-to-time-long](#org-ml-unixtime-to-time-long-unixtime) `(unixtime)`
* [org-ml-unixtime-to-time-short](#org-ml-unixtime-to-time-short-unixtime) `(unixtime)`

### Timestamp (Standard)

* [org-ml-timestamp-get-start-time](#org-ml-timestamp-get-start-time-timestamp) `(timestamp)`
* [org-ml-timestamp-get-end-time](#org-ml-timestamp-get-end-time-timestamp) `(timestamp)`
* [org-ml-timestamp-get-range](#org-ml-timestamp-get-range-timestamp) `(timestamp)`
* [org-ml-timestamp-is-active](#org-ml-timestamp-is-active-timestamp) `(timestamp)`
* [org-ml-timestamp-is-ranged](#org-ml-timestamp-is-ranged-timestamp) `(timestamp)`
* [org-ml-timestamp-range-contains-p](#org-ml-timestamp-range-contains-p-unixtime-timestamp) `(unixtime timestamp)`
* [org-ml-timestamp-set-collapsed](#org-ml-timestamp-set-collapsed-flag-timestamp) `(flag timestamp)`
* [org-ml-timestamp-get-warning](#org-ml-timestamp-get-warning-timestamp) `(timestamp)`
* [org-ml-timestamp-set-warning](#org-ml-timestamp-set-warning-warning-timestamp) `(warning timestamp)`
* [org-ml-timestamp-map-warning](#org-ml-timestamp-map-warning-fun-timestamp) `(fun timestamp)`
* [org-ml-timestamp-get-repeater](#org-ml-timestamp-get-repeater-timestamp) `(timestamp)`
* [org-ml-timestamp-set-repeater](#org-ml-timestamp-set-repeater-repeater-timestamp) `(repeater timestamp)`
* [org-ml-timestamp-map-repeater](#org-ml-timestamp-map-repeater-fun-timestamp) `(fun timestamp)`
* [org-ml-timestamp-set-start-time](#org-ml-timestamp-set-start-time-time-timestamp) `(time timestamp)`
* [org-ml-timestamp-set-end-time](#org-ml-timestamp-set-end-time-time-timestamp) `(time timestamp)`
* [org-ml-timestamp-set-single-time](#org-ml-timestamp-set-single-time-time-timestamp) `(time timestamp)`
* [org-ml-timestamp-set-double-time](#org-ml-timestamp-set-double-time-time1-time2-timestamp) `(time1 time2 timestamp)`
* [org-ml-timestamp-set-range](#org-ml-timestamp-set-range-range-timestamp) `(range timestamp)`
* [org-ml-timestamp-set-active](#org-ml-timestamp-set-active-flag-timestamp) `(flag timestamp)`
* [org-ml-timestamp-shift](#org-ml-timestamp-shift-n-unit-timestamp) `(n unit timestamp)`
* [org-ml-timestamp-shift-start](#org-ml-timestamp-shift-start-n-unit-timestamp) `(n unit timestamp)`
* [org-ml-timestamp-shift-end](#org-ml-timestamp-shift-end-n-unit-timestamp) `(n unit timestamp)`
* [org-ml-timestamp-toggle-active](#org-ml-timestamp-toggle-active-timestamp) `(timestamp)`
* [org-ml-timestamp-truncate](#org-ml-timestamp-truncate-timestamp) `(timestamp)`
* [org-ml-timestamp-truncate-start](#org-ml-timestamp-truncate-start-timestamp) `(timestamp)`
* [org-ml-timestamp-truncate-end](#org-ml-timestamp-truncate-end-timestamp) `(timestamp)`

### Timestamp (diary)

* [org-ml-timestamp-diary-set-value](#org-ml-timestamp-diary-set-value-form-timestamp-diary) `(form timestamp-diary)`

### Affiliated Keywords

* [org-ml-get-affiliated-keyword](#org-ml-get-affiliated-keyword-key-node) `(key node)`
* [org-ml-set-affiliated-keyword](#org-ml-set-affiliated-keyword-key-value-node) `(key value node)`
* [org-ml-map-affiliated-keyword](#org-ml-map-affiliated-keyword-key-fun-node) `(key fun node)`
* [org-ml-set-caption!](#org-ml-set-caption-caption-node) `(caption node)`

## Branch/Child Manipulation


Set, get, and map the children of branch nodes.


### Polymorphic

* [org-ml-children-contain-point](#org-ml-children-contain-point-point-branch-node) `(point branch-node)`
* [org-ml-get-children](#org-ml-get-children-branch-node) `(branch-node)`
* [org-ml-set-children](#org-ml-set-children-children-branch-node) `(children branch-node)`
* [org-ml-map-children](#org-ml-map-children-fun-branch-node) `(fun branch-node)`
* [org-ml-is-childless](#org-ml-is-childless-branch-node) `(branch-node)`

### Object Nodes

* [org-ml-unwrap](#org-ml-unwrap-object-node) `(object-node)`
* [org-ml-unwrap-types-deep](#org-ml-unwrap-types-deep-types-object-node) `(types object-node)`
* [org-ml-unwrap-deep](#org-ml-unwrap-deep-object-node) `(object-node)`

### Secondary Strings

* [org-ml-flatten](#org-ml-flatten-secondary-string) `(secondary-string)`
* [org-ml-flatten-types-deep](#org-ml-flatten-types-deep-types-secondary-string) `(types secondary-string)`
* [org-ml-flatten-deep](#org-ml-flatten-deep-secondary-string) `(secondary-string)`

### Item

* [org-ml-item-get-paragraph](#org-ml-item-get-paragraph-item) `(item)`
* [org-ml-item-set-paragraph](#org-ml-item-set-paragraph-secondary-string-item) `(secondary-string item)`
* [org-ml-item-map-paragraph](#org-ml-item-map-paragraph-fun-item) `(fun item)`

### Headline

* [org-ml-headline-get-section](#org-ml-headline-get-section-headline) `(headline)`
* [org-ml-headline-set-section](#org-ml-headline-set-section-children-headline) `(children headline)`
* [org-ml-headline-map-section](#org-ml-headline-map-section-fun-headline) `(fun headline)`
* [org-ml-headline-get-subheadlines](#org-ml-headline-get-subheadlines-headline) `(headline)`
* [org-ml-headline-set-subheadlines](#org-ml-headline-set-subheadlines-subheadlines-headline) `(subheadlines headline)`
* [org-ml-headline-map-subheadlines](#org-ml-headline-map-subheadlines-fun-headline) `(fun headline)`

### Headline (metadata)

* [org-ml-headline-get-planning](#org-ml-headline-get-planning-headline) `(headline)`
* [org-ml-headline-set-planning](#org-ml-headline-set-planning-planning-headline) `(planning headline)`
* [org-ml-headline-map-planning](#org-ml-headline-map-planning-fun-headline) `(fun headline)`
* [org-ml-headline-get-node-properties](#org-ml-headline-get-node-properties-headline) `(headline)`
* [org-ml-headline-set-node-properties](#org-ml-headline-set-node-properties-node-properties-headline) `(node-properties headline)`
* [org-ml-headline-map-node-properties](#org-ml-headline-map-node-properties-fun-headline) `(fun headline)`
* [org-ml-headline-get-node-property](#org-ml-headline-get-node-property-key-headline) `(key headline)`
* [org-ml-headline-set-node-property](#org-ml-headline-set-node-property-key-value-headline) `(key value headline)`
* [org-ml-headline-map-node-property](#org-ml-headline-map-node-property-key-fun-headline) `(key fun headline)`

### Headline (logbook and contents)

* [org-ml-headline-get-supercontents](#org-ml-headline-get-supercontents-config-headline) `(config headline)`
* [org-ml-headline-set-supercontents](#org-ml-headline-set-supercontents-config-supercontents-headline) `(config supercontents headline)`
* [org-ml-headline-map-supercontents](#org-ml-headline-map-supercontents-config-fun-headline) `(config fun headline)`
* [org-ml-headline-get-logbook-items](#org-ml-headline-get-logbook-items-config-headline) `(config headline)`
* [org-ml-headline-set-logbook-items](#org-ml-headline-set-logbook-items-config-items-headline) `(config items headline)`
* [org-ml-headline-map-logbook-items](#org-ml-headline-map-logbook-items-config-fun-headline) `(config fun headline)`
* [org-ml-headline-get-logbook-clocks](#org-ml-headline-get-logbook-clocks-config-headline) `(config headline)`
* [org-ml-headline-set-logbook-clocks](#org-ml-headline-set-logbook-clocks-config-clocks-headline) `(config clocks headline)`
* [org-ml-headline-map-logbook-clocks](#org-ml-headline-map-logbook-clocks-config-fun-headline) `(config fun headline)`
* [org-ml-headline-get-contents](#org-ml-headline-get-contents-config-headline) `(config headline)`
* [org-ml-headline-set-contents](#org-ml-headline-set-contents-config-contents-headline) `(config contents headline)`
* [org-ml-headline-map-contents](#org-ml-headline-map-contents-config-fun-headline) `(config fun headline)`
* [org-ml-headline-logbook-append-item](#org-ml-headline-logbook-append-item-config-item-headline) `(config item headline)`
* [org-ml-headline-logbook-append-open-clock](#org-ml-headline-logbook-append-open-clock-config-unixtime-headline) `(config unixtime headline)`
* [org-ml-headline-logbook-close-open-clock](#org-ml-headline-logbook-close-open-clock-config-unixtime-note-headline) `(config unixtime note headline)`
* [org-ml-headline-logbook-convert-config](#org-ml-headline-logbook-convert-config-config1-config2-headline) `(config1 config2 headline)`

### Headline (misc)

* [org-ml-headline-get-path](#org-ml-headline-get-path-headline) `(headline)`
* [org-ml-headline-update-item-statistics](#org-ml-headline-update-item-statistics-headline) `(headline)`
* [org-ml-headline-update-todo-statistics](#org-ml-headline-update-todo-statistics-headline) `(headline)`
* [org-ml-headline-demote-subheadline](#org-ml-headline-demote-subheadline-index-headline) `(index headline)`
* [org-ml-headline-demote-subtree](#org-ml-headline-demote-subtree-index-headline) `(index headline)`
* [org-ml-headline-promote-subheadline](#org-ml-headline-promote-subheadline-index-child-index-headline) `(index child-index headline)`
* [org-ml-headline-promote-all-subheadlines](#org-ml-headline-promote-all-subheadlines-index-headline) `(index headline)`

### Plain List

* [org-ml-plain-list-set-type](#org-ml-plain-list-set-type-type-plain-list) `(type plain-list)`
* [org-ml-plain-list-indent-item](#org-ml-plain-list-indent-item-index-plain-list) `(index plain-list)`
* [org-ml-plain-list-indent-item-tree](#org-ml-plain-list-indent-item-tree-index-plain-list) `(index plain-list)`
* [org-ml-plain-list-outdent-item](#org-ml-plain-list-outdent-item-index-child-index-plain-list) `(index child-index plain-list)`
* [org-ml-plain-list-outdent-all-items](#org-ml-plain-list-outdent-all-items-index-plain-list) `(index plain-list)`

### Table

* [org-ml-table-get-cell](#org-ml-table-get-cell-row-index-column-index-table) `(row-index column-index table)`
* [org-ml-table-delete-column](#org-ml-table-delete-column-column-index-table) `(column-index table)`
* [org-ml-table-delete-row](#org-ml-table-delete-row-row-index-table) `(row-index table)`
* [org-ml-table-insert-column!](#org-ml-table-insert-column-column-index-column-text-table) `(column-index column-text table)`
* [org-ml-table-insert-row!](#org-ml-table-insert-row-row-index-row-text-table) `(row-index row-text table)`
* [org-ml-table-replace-cell!](#org-ml-table-replace-cell-row-index-column-index-cell-text-table) `(row-index column-index cell-text table)`
* [org-ml-table-replace-column!](#org-ml-table-replace-column-column-index-column-text-table) `(column-index column-text table)`
* [org-ml-table-replace-row!](#org-ml-table-replace-row-row-index-row-text-table) `(row-index row-text table)`

## Node Matching


Use pattern-matching to selectively perform operations on nodes in trees.

* [org-ml-match](#org-ml-match-pattern-node) `(pattern node)`
* [org-ml-match-delete](#org-ml-match-delete-pattern-node) `(pattern node)`
* [org-ml-match-extract](#org-ml-match-extract-pattern-node) `(pattern node)`
* [org-ml-match-map](#org-ml-match-map-pattern-fun-node) `(pattern fun node)`
* [org-ml-match-mapcat](#org-ml-match-mapcat-pattern-fun-node) `(pattern fun node)`
* [org-ml-match-replace](#org-ml-match-replace-pattern-node-node) `(pattern node* node)`
* [org-ml-match-insert-before](#org-ml-match-insert-before-pattern-node-node) `(pattern node* node)`
* [org-ml-match-insert-after](#org-ml-match-insert-after-pattern-node-node) `(pattern node* node)`
* [org-ml-match-insert-within](#org-ml-match-insert-within-pattern-index-node-node) `(pattern index node* node)`
* [org-ml-match-splice](#org-ml-match-splice-pattern-nodes-node) `(pattern nodes* node)`
* [org-ml-match-splice-before](#org-ml-match-splice-before-pattern-nodes-node) `(pattern nodes* node)`
* [org-ml-match-splice-after](#org-ml-match-splice-after-pattern-nodes-node) `(pattern nodes* node)`
* [org-ml-match-splice-within](#org-ml-match-splice-within-pattern-index-nodes-node) `(pattern index nodes* node)`
* [org-ml-match-do](#org-ml-match-do-pattern-fun-node) `(pattern fun node)`

## Buffer Side Effects


Map node manipulations into buffers.


### Insert

* [org-ml-insert](#org-ml-insert-point-node) `(point node)`
* [org-ml-insert-tail](#org-ml-insert-tail-point-node) `(point node)`

### Update

* [org-ml-update](#org-ml-update-fun-node) `(fun node)`
* [org-ml-update-object-at](#org-ml-update-object-at-point-fun) `(point fun)`
* [org-ml-update-element-at](#org-ml-update-element-at-point-fun) `(point fun)`
* [org-ml-update-table-row-at](#org-ml-update-table-row-at-point-fun) `(point fun)`
* [org-ml-update-item-at](#org-ml-update-item-at-point-fun) `(point fun)`
* [org-ml-update-headline-at](#org-ml-update-headline-at-point-fun) `(point fun)`
* [org-ml-update-subtree-at](#org-ml-update-subtree-at-point-fun) `(point fun)`
* [org-ml-update-section-at](#org-ml-update-section-at-point-fun) `(point fun)`
* [org-ml-update-headlines](#org-ml-update-headlines-which-fun) `(which fun)`
* [org-ml-update-subtrees](#org-ml-update-subtrees-which-fun) `(which fun)`

### Misc

* [org-ml-fold](#org-ml-fold-node) `(node)`
* [org-ml-unfold](#org-ml-unfold-node) `(node)`
## String Conversion


Convert nodes to strings.

#### org-ml-to-string `(node)`

Return **`node`** as an interpreted string without text properties.

```el
(org-ml-to-string '(bold (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
			 "text"))
 ;; => "*text*"

(org-ml-to-string '(bold (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
			 "text"))
 ;; => "*text*   "

(org-ml-to-string nil)
 ;; => ""

```

#### org-ml-to-trimmed-string `(node)`

Like [`org-ml-to-string`](#org-ml-to-string-node) but strip whitespace when returning **`node`**.

```el
(org-ml-to-trimmed-string '(bold (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
				 "text"))
 ;; => "*text*"

(org-ml-to-trimmed-string '(bold (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
				 "text"))
 ;; => "*text*"

(org-ml-to-trimmed-string nil)
 ;; => ""

```

#### org-ml-from-string `(type string)`

Convert **`string`** to a node.
**`type`** is the node type intended by **`string`**; if **`string`** cannot be
parsed into **`type`** this function will return nil.

```el
(org-ml-from-string 'bold "*text*")
 ;; => '(bold (:begin 1 :end 7 :contents-begin 2 :contents-end 6 :post-blank 0 :parent nil) "text")

(org-ml-from-string 'italic "*text*")
 ;; => nil

```


## Buffer Parsing


Parse buffers to trees.

#### org-ml-parse-object-at `(point)`

Return object node under **`point`** or nil if not on an object.

```el
;; Given the following contents:
; *text*

(->> (org-ml-parse-object-at 1)
  (car))
 ;; => 'bold

;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-object-at 1)
  (car))
 ;; => 'timestamp

;; Given the following contents:
; - notme

;; Return nil when parsing an element
(org-ml-parse-object-at
 1)
 ;; => nil

```

#### org-ml-parse-element-at `(point)`

Return element node under **`point`** or nil if not on an element.

This function will return every element available in `org-ml-elements`
with the exception of `section`, `item`, and `table-row`. To
specifically parse these, use the functions [`org-ml-parse-section-at`](#org-ml-parse-section-at-point),
[`org-ml-parse-item-at`](#org-ml-parse-item-at-point), and [`org-ml-parse-table-row-at`](#org-ml-parse-table-row-at-point).

```el
;; Given the following contents:
; #+call: ktulu()

(->> (org-ml-parse-element-at 1)
  (car))
 ;; => 'babel-call

;; Given the following contents:
; - plain-list

;; Give the plain-list, not the item for this function
(->> (org-ml-parse-element-at 1)
  (car))
 ;; => 'plain-list

;; Given the following contents:
; | R | A |
; | G | E |

;; Return a table, not the table-row for this function
(->> (org-ml-parse-element-at 1)
  (car))
 ;; => 'table

```

#### org-ml-parse-table-row-at `(point)`

Return table-row node under **`point`** or nil if not on a table-row.

```el
;; Given the following contents:
; | bow | stroke |
; |-----+--------|
; | wob | ekorts |

;; Return the row itself
(->> (org-ml-parse-table-row-at 1)
  (car))
 ;; => 'table-row

(->> (org-ml-parse-table-row-at 20)
  (car))
 ;; => 'table-row

(->> (org-ml-parse-table-row-at 40)
  (car))
 ;; => 'table-row

;; Also return the row when not at beginning of line
(->> (org-ml-parse-table-row-at 5)
  (car))
 ;; => 'table-row

;; Given the following contents:
; - bow and arrow choke

;; Return nil if not a table-row
(->> (org-ml-parse-table-row-at 1)
  (car))
 ;; => nil

```

#### org-ml-parse-headline-at `(point)`

Return headline node under **`point`** or nil if not on a headline.
**`point`** does not need to be on the headline itself. Only the headline
and its section will be returned. To include subheadlines, use
[`org-ml-parse-subtree-at`](#org-ml-parse-subtree-at-point).

```el
;; Given the following contents:
; * headline

;; Return the headline itself
(->> (org-ml-parse-headline-at 1)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; section crap

;; Return headline and section
(->> (org-ml-parse-headline-at 1)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Return headline when point is in the section
(->> (org-ml-parse-headline-at 12)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Given the following contents:
; * headline
; section crap
; ** not parsed

;; Don't parse any subheadlines
(->> (org-ml-parse-headline-at 1)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Given the following contents:
; nothing nowhere

;; Return nil if not under a headline
(->> (org-ml-parse-headline-at 1)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-parse-subtree-at `(point)`

Return headline node under **`point`** or nil if not on a headline.
**`point`** does not need to be on the headline itself. Unlike
[`org-ml-parse-headline-at`](#org-ml-parse-headline-at-point), the returned node will include
child headlines.

```el
;; Given the following contents:
; * headline

;; Return the headline itself
(->> (org-ml-parse-subtree-at 1)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; section crap

;; Return headline and section
(->> (org-ml-parse-subtree-at 1)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Return headline when point is in the section
(->> (org-ml-parse-subtree-at 12)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Given the following contents:
; * headline
; section crap
; ** parsed

;; Return all the subheadlines
(->> (org-ml-parse-subtree-at 1)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      section crap
 ;      ** parsed"

;; Given the following contents:
; nothing nowhere

;; Return nil if not under a headline
(->> (org-ml-parse-subtree-at 1)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-parse-item-at `(point)`

Return item node under **`point`** or nil if not on an item.
This will return the item node even if **`point`** is not at the beginning
of the line.

```el
;; Given the following contents:
; - item

;; Return the item itself
(->> (org-ml-parse-item-at 1)
  (org-ml-to-trimmed-string))
 ;; => "- item"

;; Also return the item when not at beginning of line
(->> (org-ml-parse-item-at 5)
  (org-ml-to-trimmed-string))
 ;; => "- item"

;; Given the following contents:
; - item
;   - item 2

;; Return item and its subitems
(->> (org-ml-parse-item-at 1)
  (org-ml-to-trimmed-string))
 ;; => "- item
 ;        - item 2"

;; Given the following contents:
; * not item

;; Return nil if not an item
(->> (org-ml-parse-item-at 1)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-parse-section-at `(point)`

Return section node under **`point`** or nil if not on a section.
If **`point`** is on or within a headline, return the section under that
headline. If **`point`** is before the first headline (if any), return
the section at the top of the org buffer.

```el
;; Given the following contents:
; over headline
; * headline
; under headline

;; Return the section above the headline
(->> (org-ml-parse-section-at 1)
  (org-ml-to-trimmed-string))
 ;; => "over headline"

;; Return the section under headline
(->> (org-ml-parse-section-at 25)
  (org-ml-to-trimmed-string))
 ;; => "under headline"

;; Given the following contents:
; * headline
; ** subheadline

;; Return nil if no section under headline
(->> (org-ml-parse-section-at 1)
  (org-ml-to-trimmed-string))
 ;; => ""

;; Given the following contents:
; 

;; Return nil if no section at all
(->> (org-ml-parse-section-at 1)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-parse-this-toplevel-section `nil`

Return section node corresponding to the top of the current buffer.
If there is no such section, return nil.

```el
;; Given the following contents:
; over headline
; * headline
; under headline

(->> (org-ml-parse-this-toplevel-section)
  (org-ml-to-trimmed-string))
 ;; => "over headline"

;; Given the following contents:
; * headline
; under headline

(->> (org-ml-parse-this-toplevel-section)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-this-buffer-has-headlines `nil`

Return t if the current buffer has headlines, else return nil.

```el
;; Given the following contents:
; not headline
; * headline

(org-ml-this-buffer-has-headlines)
 ;; => t

;; Given the following contents:
; not headline

(org-ml-this-buffer-has-headlines)
 ;; => nil

```

#### org-ml-parse-headlines `(which)`

Return list of headline nodes from current buffer.

**`which`** describes the location of headlines to be parsed and is one
of the following:
- `n`: parse up to index `n` headlines (which 0 is the first); if negative
    start counting from the last headline (which -1 refers to the last)
- `(m n)`: like `n` but parse after index `m` headlines; `m` and `n` may both
    be similarly negative
- [`a` `b`]: parse all headlines whose first point falls between points
    `a` and `b` in the buffer; if `a` and `b` are nil, use `point-min` and
    `point-max` respectively.
- 'all': parse all headlines (equivalent to [nil nil])

Each headline is obtained with [`org-ml-parse-headline-at`](#org-ml-parse-headline-at-point).

```el
;; Given the following contents:
; not headline
; * one
; * two
; * three

(->> (org-ml-parse-headlines 'all)
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      * two
 ;      * three
 ;      "

;; Given the following contents:
; not headline

(->> (org-ml-parse-headlines 'all)
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => ""

;; Given the following contents:
; not headline
; * one
; ** two
; *** three

(->> (org-ml-parse-headlines 'all)
  (-map #'org-ml-to-trimmed-string))
 ;; => '("* one
 ;     ** two
 ;     *** three" "** two
 ;     *** three" "*** three")

;; Given the following contents:
; not headline
; * one
; * two
; * three

(->> (org-ml-parse-headlines 0)
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      "

(->> (org-ml-parse-headlines '(0 1))
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      * two
 ;      "

(->> (org-ml-parse-headlines [10 25])
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      * two
 ;      "

```

#### org-ml-parse-subtrees `(which)`

Return list of subtree nodes from current buffer.

**`which`** has analogous meaning to that in [`org-ml-parse-headlines`](#org-ml-parse-headlines-which)
except applied to subtrees not individual headlines.

```el
;; Given the following contents:
; not headline
; * one
; ** _one
; * two
; ** _two
; * three
; ** _three

(->> (org-ml-parse-subtrees 'all)
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      ** _one
 ;      * two
 ;      ** _two
 ;      * three
 ;      ** _three
 ;      "

;; Given the following contents:
; not headline

(->> (org-ml-parse-subtrees 'all)
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => ""

;; Given the following contents:
; not headline
; * one
; ** _one
; * two
; ** _two
; * three
; ** _three

(->> (org-ml-parse-subtrees 0)
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      ** _one
 ;      "

(->> (org-ml-parse-subtrees '(0 1))
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      ** _one
 ;      * two
 ;      ** _two
 ;      "

(->> (org-ml-parse-subtrees [10 30])
  (-map #'org-ml-to-string)
  (s-join ""))
 ;; => "* one
 ;      ** _one
 ;      * two
 ;      ** _two
 ;      "

```


## Building


Build new nodes.


### Leaf Object Nodes

#### org-ml-build-code `(value &key post-blank)`

Build a code object node.

The following properties are settable:
- **`value`**: (required) a string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-code "text")
  (org-ml-to-string))
 ;; => "~text~"

```

#### org-ml-build-entity `(name &key use-brackets-p post-blank)`

Build an entity object node.

The following properties are settable:
- **`name`**: (required) a string that makes `org-entity-get` return non-nil
- **`use-brackets-p`**:  nil or t
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-entity "gamma")
  (org-ml-to-string))
 ;; => "\\gamma"

```

#### org-ml-build-export-snippet `(back-end value &key post-blank)`

Build an export-snippet object node.

The following properties are settable:
- **`back-end`**: (required) a oneline string
- **`value`**: (required) a string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-export-snippet "back" "value")
  (org-ml-to-string))
 ;; => "@@back:value@@"

```

#### org-ml-build-inline-babel-call `(call &key inside-header arguments end-header post-blank)`

Build an inline-babel-call object node.

The following properties are settable:
- **`call`**: (required) a oneline string
- **`inside-header`**:  a plist
- **`arguments`**:  a list of oneline strings
- **`end-header`**:  a plist
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-inline-babel-call "name")
  (org-ml-to-string))
 ;; => "call_name()"

(->> (org-ml-build-inline-babel-call "name" :arguments '("n=4"))
  (org-ml-to-string))
 ;; => "call_name(n=4)"

(->> (org-ml-build-inline-babel-call "name" :inside-header '(:key val))
  (org-ml-to-string))
 ;; => "call_name[:key val]()"

(->> (org-ml-build-inline-babel-call "name" :end-header '(:key val))
  (org-ml-to-string))
 ;; => "call_name()[:key val]"

```

#### org-ml-build-inline-src-block `(language &key parameters (value "") post-blank)`

Build an inline-src-block object node.

The following properties are settable:
- **`language`**: (required) a oneline string
- **`parameters`**:  a plist
- **`value`**: (default `""`) a string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-inline-src-block "lang")
  (org-ml-to-string))
 ;; => "src_lang{}"

(->> (org-ml-build-inline-src-block "lang" :value "value")
  (org-ml-to-string))
 ;; => "src_lang{value}"

(->> (org-ml-build-inline-src-block "lang" :value "value" :parameters '(:key val))
  (org-ml-to-string))
 ;; => "src_lang[:key val]{value}"

```

#### org-ml-build-line-break `(&key post-blank)`

Build a line-break object node.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-line-break)
  (org-ml-to-string))
 ;; => "\\\\
 ;      "

```

#### org-ml-build-latex-fragment `(value &key post-blank)`

Build a latex-fragment object node.

The following properties are settable:
- **`value`**: (required) a string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-latex-fragment "$2+2=5$")
  (org-ml-to-string))
 ;; => "$2+2=5$"

```

#### org-ml-build-macro `(key &key args post-blank)`

Build a macro object node.

The following properties are settable:
- **`key`**: (required) a oneline string
- **`args`**:  a list of oneline strings
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-macro "economics")
  (org-ml-to-string))
 ;; => "{{{economics}}}"

(->> (org-ml-build-macro "economics" :args '("s=d"))
  (org-ml-to-string))
 ;; => "{{{economics(s=d)}}}"

```

#### org-ml-build-statistics-cookie `(value &key post-blank)`

Build a statistics-cookie object node.

The following properties are settable:
- **`value`**: (required) a list of non-neg integers like `(perc)` or `(num den)` which make [`num`/`den`] and [`perc`%] respectively
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-statistics-cookie '(nil))
  (org-ml-to-string))
 ;; => "[%]"

(->> (org-ml-build-statistics-cookie '(nil nil))
  (org-ml-to-string))
 ;; => "[/]"

(->> (org-ml-build-statistics-cookie '(50))
  (org-ml-to-string))
 ;; => "[50%]"

(->> (org-ml-build-statistics-cookie '(1 3))
  (org-ml-to-string))
 ;; => "[1/3]"

```

#### org-ml-build-target `(value &key post-blank)`

Build a target object node.

The following properties are settable:
- **`value`**: (required) a oneline string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-target "text")
  (org-ml-to-string))
 ;; => "<<text>>"

```

#### org-ml-build-timestamp `(type year-start month-start day-start year-end month-end day-end &key hour-start minute-start hour-end minute-end repeater-type repeater-unit repeater-value warning-type warning-unit warning-value post-blank)`

Build a timestamp object node.

The following properties are settable:
- **`type`**: (required) a symbol from `inactive`, `active`, `inactive-range`, or `active-range`
- **`year-start`**: (required) a positive integer
- **`month-start`**: (required) a positive integer
- **`day-start`**: (required) a positive integer
- **`year-end`**: (required) a positive integer
- **`month-end`**: (required) a positive integer
- **`day-end`**: (required) a positive integer
- **`hour-start`**:  a non-negative integer or nil
- **`minute-start`**:  a non-negative integer or nil
- **`hour-end`**:  a non-negative integer or nil
- **`minute-end`**:  a non-negative integer or nil
- **`repeater-type`**:  nil or a symbol from `catch-up`, `restart`, or `cumulate`
- **`repeater-unit`**:  nil or a symbol from `year` `month` `week` `day`, or `hour`
- **`repeater-value`**:  a positive integer or nil
- **`warning-type`**:  nil or a symbol from `all` or `first`
- **`warning-unit`**:  nil or a symbol from `year` `month` `week` `day`, or `hour`
- **`warning-value`**:  a positive integer or nil
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-timestamp 'inactive 2019 1 15 2019 1 15)
  (org-ml-to-string))
 ;; => "[2019-01-15 Tue]"

(->> (org-ml-build-timestamp 'active-range 2019 1 15 2019 1 16)
  (org-ml-to-string))
 ;; => "<2019-01-15 Tue>--<2019-01-16 Wed>"

(->> (org-ml-build-timestamp 'inactive 2019 1 15 2019 1 15 :warning-type 'all :warning-unit 'day :warning-value 1)
  (org-ml-to-string))
 ;; => "[2019-01-15 Tue -1d]"

```

#### org-ml-build-verbatim `(value &key post-blank)`

Build a verbatim object node.

The following properties are settable:
- **`value`**: (required) a string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-verbatim "text")
  (org-ml-to-string))
 ;; => "=text="

```


### Branch Object Nodes

#### org-ml-build-bold `(&key post-blank &rest object-nodes)`

Build a bold object node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-bold "text")
  (org-ml-to-string))
 ;; => "*text*"

```

#### org-ml-build-footnote-reference `(&key label post-blank &rest object-nodes)`

Build a footnote-reference object node with **`object-nodes`** as children.

The following properties are settable:
- **`label`**:  a oneline string or nil
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-footnote-reference)
  (org-ml-to-string))
 ;; => "[fn:]"

(->> (org-ml-build-footnote-reference :label "label")
  (org-ml-to-string))
 ;; => "[fn:label]"

(->> (org-ml-build-footnote-reference :label "label" "content")
  (org-ml-to-string))
 ;; => "[fn:label:content]"

```

#### org-ml-build-italic `(&key post-blank &rest object-nodes)`

Build an italic object node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-italic "text")
  (org-ml-to-string))
 ;; => "/text/"

```

#### org-ml-build-link `(path &key format (type "fuzzy") post-blank &rest object-nodes)`

Build a link object node with **`object-nodes`** as children.

The following properties are settable:
- **`path`**: (required) a oneline string
- **`format`**:  the symbol `plain`, `bracket` or `angle`
- **`type`**: (default `"fuzzy"`) a oneline string from `org-link-types` or `"coderef"`, `"custorg-ml-id"`, `"file"`, `"id"`, `"radio"`, or `"fuzzy"`
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-link "target")
  (org-ml-to-string))
 ;; => "[[target]]"

(->> (org-ml-build-link "target" :type "file")
  (org-ml-to-string))
 ;; => "[[file:target]]"

(->> (org-ml-build-link "target" "desc")
  (org-ml-to-string))
 ;; => "[[target][desc]]"

```

#### org-ml-build-radio-target `(&key post-blank &rest object-nodes)`

Build a radio-target object node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-radio-target "text")
  (org-ml-to-string))
 ;; => "<<<text>>>"

```

#### org-ml-build-strike-through `(&key post-blank &rest object-nodes)`

Build a strike-through object node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-strike-through "text")
  (org-ml-to-string))
 ;; => "+text+"

```

#### org-ml-build-superscript `(&key use-brackets-p post-blank &rest object-nodes)`

Build a superscript object node with **`object-nodes`** as children.

The following properties are settable:
- **`use-brackets-p`**:  nil or t
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-superscript "text")
  (org-ml-to-string))
 ;; => "^text"

```

#### org-ml-build-subscript `(&key use-brackets-p post-blank &rest object-nodes)`

Build a subscript object node with **`object-nodes`** as children.

The following properties are settable:
- **`use-brackets-p`**:  nil or t
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-subscript "text")
  (org-ml-to-string))
 ;; => "_text"

```

#### org-ml-build-table-cell `(&key post-blank &rest object-nodes)`

Build a table-cell object node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-table-cell "text")
  (org-ml-to-string))
 ;; => " text |"

```

#### org-ml-build-underline `(&key post-blank &rest object-nodes)`

Build an underline object node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-underline "text")
  (org-ml-to-string))
 ;; => "_text_"

```


### Leaf Element Nodes

#### org-ml-build-babel-call `(call &key inside-header arguments end-header name plot header results caption post-blank)`

Build a babel-call element node.

The following properties are settable:
- **`call`**: (required) a oneline string
- **`inside-header`**:  a plist
- **`arguments`**:  a list of oneline strings
- **`end-header`**:  a plist
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-babel-call "name")
  (org-ml-to-trimmed-string))
 ;; => "#+call: name()"

(->> (org-ml-build-babel-call "name" :arguments '("arg=x"))
  (org-ml-to-trimmed-string))
 ;; => "#+call: name(arg=x)"

(->> (org-ml-build-babel-call "name" :inside-header '(:key val))
  (org-ml-to-trimmed-string))
 ;; => "#+call: name[:key val]()"

(->> (org-ml-build-babel-call "name" :end-header '(:key val))
  (org-ml-to-trimmed-string))
 ;; => "#+call: name() :key val"

```

#### org-ml-build-clock `(value &key post-blank)`

Build a clock element node.

The following properties are settable:
- **`value`**: (required) a ranged or unranged inactive timestamp node with no warning or repeater
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-clock (org-ml-build-timestamp! '(2019 1 1 0 0)))
  (org-ml-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 00:00]"

(->> (org-ml-build-timestamp! '(2019 1 1 0 0)
			       :end '(2019 1 1 1 0))
  (org-ml-set-property :type 'inactive-range)
  (org-ml-build-clock)
  (org-ml-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-01 Tue 01:00] =>  1:00"

```

#### org-ml-build-comment `(value &key post-blank)`

Build a comment element node.

The following properties are settable:
- **`value`**: (required) a string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-comment "text")
  (org-ml-to-trimmed-string))
 ;; => "# text"

(->> (org-ml-build-comment "text\nless")
  (org-ml-to-trimmed-string))
 ;; => "# text
 ;      # less"

```

#### org-ml-build-comment-block `(&key (value "") name plot header results caption post-blank)`

Build a comment-block element node.

The following properties are settable:
- **`value`**: (default `""`) a string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-comment-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_comment
 ;      #+end_comment"

(->> (org-ml-build-comment-block :value "text")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_comment
 ;      text
 ;      #+end_comment"

```

#### org-ml-build-diary-sexp `(&key value name plot header results caption post-blank)`

Build a diary-sexp element node.

The following properties are settable:
- **`value`**:  a list form or nil
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-diary-sexp)
  (org-ml-to-trimmed-string))
 ;; => "%%()"

(->> (org-ml-build-diary-sexp :value '(text))
  (org-ml-to-trimmed-string))
 ;; => "%%(text)"

```

#### org-ml-build-example-block `(&key preserve-indent switches (value "") name plot header results caption post-blank)`

Build an example-block element node.

The following properties are settable:
- **`preserve-indent`**:  nil or t
- **`switches`**:  a list of oneline strings
- **`value`**: (default `""`) a string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-example-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_example
 ;      #+end_example"

(->> (org-ml-build-example-block :value "text")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_example
 ;        text
 ;      #+end_example"

(->> (org-ml-build-example-block :value "text" :switches '("switches"))
  (org-ml-to-trimmed-string))
 ;; => "#+begin_example switches
 ;        text
 ;      #+end_example"

```

#### org-ml-build-export-block `(type value &key name plot header results caption post-blank)`

Build an export-block element node.

The following properties are settable:
- **`type`**: (required) a oneline string
- **`value`**: (required) a string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-export-block "type" "value\n")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_export type
 ;      value
 ;      #+end_export"

```

#### org-ml-build-fixed-width `(value &key name plot header results caption post-blank)`

Build a fixed-width element node.

The following properties are settable:
- **`value`**: (required) a oneline string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-fixed-width "text")
  (org-ml-to-trimmed-string))
 ;; => ": text"

```

#### org-ml-build-horizontal-rule `(&key name plot header results caption post-blank)`

Build a horizontal-rule element node.

The following properties are settable:
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-horizontal-rule)
  (org-ml-to-trimmed-string))
 ;; => "-----"

```

#### org-ml-build-keyword `(key value &key name plot header results caption post-blank)`

Build a keyword element node.

The following properties are settable:
- **`key`**: (required) a oneline string
- **`value`**: (required) a oneline string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-keyword "FILETAGS" "tmsu")
  (org-ml-to-trimmed-string))
 ;; => "#+filetags: tmsu"

```

#### org-ml-build-latex-environment `(value &key name plot header results caption post-blank)`

Build a latex-environment element node.

The following properties are settable:
- **`value`**: (required) a list of strings like `(env body)` or `(env)`
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-latex-environment '("env" "text"))
  (org-ml-to-trimmed-string))
 ;; => "\\begin{env}
 ;      text
 ;      \\end{env}"

```

#### org-ml-build-node-property `(key value &key post-blank)`

Build a node-property element node.

The following properties are settable:
- **`key`**: (required) a oneline string
- **`value`**: (required) a oneline string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-node-property "key" "val")
  (org-ml-to-trimmed-string))
 ;; => ":key:      val"

```

#### org-ml-build-planning `(&key closed deadline scheduled post-blank)`

Build a planning element node.

The following properties are settable:
- **`closed`**:  a zero-range, inactive timestamp node
- **`deadline`**:  a zero-range, active timestamp node
- **`scheduled`**:  a zero-range, active timestamp node
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-planning :closed (org-ml-build-timestamp! '(2019 1 1)
							      :active nil))
  (org-ml-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

(->> (org-ml-build-planning :scheduled (org-ml-build-timestamp! '(2019 1 1)
								 :active t))
  (org-ml-to-trimmed-string))
 ;; => "SCHEDULED: <2019-01-01 Tue>"

(->> (org-ml-build-planning :deadline (org-ml-build-timestamp! '(2019 1 1)
								:active t))
  (org-ml-to-trimmed-string))
 ;; => "DEADLINE: <2019-01-01 Tue>"

```

#### org-ml-build-src-block `(&key (value "") language parameters preserve-indent switches name plot header results caption post-blank)`

Build a src-block element node.

The following properties are settable:
- **`value`**: (default `""`) a string
- **`language`**:  a string or nil
- **`parameters`**:  a plist
- **`preserve-indent`**:  nil or t
- **`switches`**:  a list of oneline strings
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-src-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_src
 ;      #+end_src"

(->> (org-ml-build-src-block :value "body")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_src
 ;        body
 ;      #+end_src"

(->> (org-ml-build-src-block :value "body" :language "emacs-lisp")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_src emacs-lisp
 ;        body
 ;      #+end_src"

(->> (org-ml-build-src-block :value "body" :switches '("-n 20" "-r"))
  (org-ml-to-trimmed-string))
 ;; => "#+begin_src -n 20 -r
 ;        body
 ;      #+end_src"

(->> (org-ml-build-src-block :value "body" :parameters '(:key val))
  (org-ml-to-trimmed-string))
 ;; => "#+begin_src :key val
 ;        body
 ;      #+end_src"

```


### Branch Element Nodes with Child Object Nodes

#### org-ml-build-paragraph `(&key name plot header results caption post-blank &rest object-nodes)`

Build a paragraph element node with **`object-nodes`** as children.

The following properties are settable:
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-paragraph "text")
  (org-ml-to-trimmed-string))
 ;; => "text"

```

#### org-ml-build-table-row `(&key post-blank &rest object-nodes)`

Build a table-row element node with **`object-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-table-cell "a")
  (org-ml-build-table-row)
  (org-ml-to-trimmed-string))
 ;; => "| a |"

```

#### org-ml-build-verse-block `(&key name plot header results caption post-blank &rest object-nodes)`

Build a verse-block element node with **`object-nodes`** as children.

The following properties are settable:
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-verse-block "text\n")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_verse
 ;      text
 ;      #+end_verse"

```


### Branch Element Nodes with Child Element Nodes

#### org-ml-build-org-data `(&rest headline-or-sections-nodes)`

Return a new org-data node.

```el
(->> (org-ml-build-headline :title '("dummy"))
  (org-ml-build-org-data)
  (org-ml-to-trimmed-string))
 ;; => "* dummy"

```

#### org-ml-build-center-block `(&key name plot header results caption post-blank &rest element-nodes)`

Build a center-block element node with **`element-nodes`** as children.

The following properties are settable:
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-center-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_center
 ;      #+end_center"

(->> (org-ml-build-paragraph "text")
  (org-ml-build-center-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_center
 ;      text
 ;      #+end_center"

```

#### org-ml-build-drawer `(drawer-name &key name plot header results caption post-blank &rest element-nodes)`

Build a drawer element node with **`element-nodes`** as children.

The following properties are settable:
- **`drawer-name`**: (required) a oneline string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-drawer "NAME")
  (org-ml-to-trimmed-string))
 ;; => ":NAME:
 ;      :END:"

(->> (org-ml-build-paragraph "text")
  (org-ml-build-drawer "NAME")
  (org-ml-to-trimmed-string))
 ;; => ":NAME:
 ;      text
 ;      :END:"

```

#### org-ml-build-dynamic-block `(block-name &key arguments name plot header results caption post-blank &rest element-nodes)`

Build a dynamic-block element node with **`element-nodes`** as children.

The following properties are settable:
- **`block-name`**: (required) a oneline string
- **`arguments`**:  a plist
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-dynamic-block "empty")
  (org-ml-to-trimmed-string))
 ;; => "#+begin: empty
 ;      #+end:"

(->> (org-ml-build-comment "I'm in here")
  (org-ml-build-dynamic-block "notempty")
  (org-ml-to-trimmed-string))
 ;; => "#+begin: notempty
 ;      # I'm in here
 ;      #+end:"

```

#### org-ml-build-footnote-definition `(label &key (pre-blank 0) name plot header results caption post-blank &rest element-nodes)`

Build a footnote-definition element node with **`element-nodes`** as children.

The following properties are settable:
- **`label`**: (required) a oneline string
- **`pre-blank`**:  a non-negative integer
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-paragraph "footnote contents")
  (org-ml-build-footnote-definition "label")
  (org-ml-to-trimmed-string))
 ;; => "[fn:label] footnote contents"

```

#### org-ml-build-headline `(&key archivedp commentedp footnote-section-p (level 1) (pre-blank 0) priority tags title todo-keyword post-blank &rest element-nodes)`

Build a headline element node with **`element-nodes`** as children.

The following properties are settable:
- **`archivedp`**:  nil or t
- **`commentedp`**:  nil or t
- **`footnote-section-p`**:  nil or t
- **`level`**:  a positive integer
- **`pre-blank`**:  a non-negative integer
- **`priority`**:  an integer between (inclusive) `org-highest-priority` and `org-lowest-priority`
- **`tags`**:  a string list
- **`title`**:  a secondary string
- **`todo-keyword`**:  a oneline string or nil
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-headline)
  (org-ml-to-trimmed-string))
 ;; => "*"

(->> (org-ml-build-headline :level 2 :title '("dummy")
			     :tags '("tmsu"))
  (org-ml-to-trimmed-string))
 ;; => "** dummy            :tmsu:"

(->> (org-ml-build-headline :todo-keyword "TODO" :archivedp t :commentedp t :priority 65)
  (org-ml-to-trimmed-string))
 ;; => "* TODO COMMENT [#A]  :ARCHIVE:"

```

#### org-ml-build-item `(&key (bullet '-) (pre-blank 0) checkbox counter tag post-blank &rest element-nodes)`

Build an item element node with **`element-nodes`** as children.

The following properties are settable:
- **`bullet`**: (default `-`) a positive integer (ordered) or the symbol `-` (unordered)
- **`pre-blank`**:  a non-negative integer
- **`checkbox`**:  nil or the symbols `on`, `off`, or `trans`
- **`counter`**:  a positive integer or nil
- **`tag`**:  a secondary string
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-paragraph "item contents")
  (org-ml-build-item)
  (org-ml-to-trimmed-string))
 ;; => "- item contents"

(->> (org-ml-build-paragraph "item contents")
  (org-ml-build-item :bullet 1)
  (org-ml-to-trimmed-string))
 ;; => "1. item contents"

(->> (org-ml-build-paragraph "item contents")
  (org-ml-build-item :checkbox 'on)
  (org-ml-to-trimmed-string))
 ;; => "- [X] item contents"

(->> (org-ml-build-paragraph "item contents")
  (org-ml-build-item :tag '("tmsu"))
  (org-ml-to-trimmed-string))
 ;; => "- tmsu :: item contents"

(->> (org-ml-build-paragraph "item contents")
  (org-ml-build-item :counter 10)
  (org-ml-to-trimmed-string))
 ;; => "- [@10] item contents"

```

#### org-ml-build-plain-list `(&key name plot header results caption post-blank &rest element-nodes)`

Build a plain-list element node with **`element-nodes`** as children.

The following properties are settable:
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-paragraph "item contents")
  (org-ml-build-item)
  (org-ml-build-plain-list)
  (org-ml-to-trimmed-string))
 ;; => "- item contents"

```

#### org-ml-build-property-drawer `(&key post-blank &rest element-nodes)`

Build a property-drawer element node with **`element-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-property-drawer)
  (org-ml-to-trimmed-string))
 ;; => ":PROPERTIES:
 ;      :END:"

(->> (org-ml-build-node-property "key" "val")
  (org-ml-build-property-drawer)
  (org-ml-to-trimmed-string))
 ;; => ":PROPERTIES:
 ;      :key:      val
 ;      :END:"

```

#### org-ml-build-quote-block `(&key name plot header results caption post-blank &rest element-nodes)`

Build a quote-block element node with **`element-nodes`** as children.

The following properties are settable:
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-quote-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_quote
 ;      #+end_quote"

(->> (org-ml-build-paragraph "quoted stuff")
  (org-ml-build-quote-block)
  (org-ml-to-trimmed-string))
 ;; => "#+begin_quote
 ;      quoted stuff
 ;      #+end_quote"

```

#### org-ml-build-section `(&key post-blank &rest element-nodes)`

Build a section element node with **`element-nodes`** as children.

The following properties are settable:

- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-paragraph "text")
  (org-ml-build-section)
  (org-ml-to-trimmed-string))
 ;; => "text"

```

#### org-ml-build-special-block `(type &key name plot header results caption post-blank &rest element-nodes)`

Build a special-block element node with **`element-nodes`** as children.

The following properties are settable:
- **`type`**: (required) a oneline string
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-special-block "monad")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_monad
 ;      #+end_monad"

(->> (org-ml-build-comment "Launch missiles")
  (org-ml-build-special-block "monad")
  (org-ml-to-trimmed-string))
 ;; => "#+begin_monad
 ;      # Launch missiles
 ;      #+end_monad"

```

#### org-ml-build-table `(&key tblfm name plot header results caption post-blank &rest element-nodes)`

Build a table element node with **`element-nodes`** as children.

The following properties are settable:
- **`tblfm`**:  a list of oneline strings
- **`name`**:  a string or nil
- **`plot`**:  a string or nil
- **`header`**:  a list of plists where all plist values are strings
- **`results`**:  a list like `(source)` or `(hash source)` where `hash` and `source` are strings.
- **`caption`**:  a list including `(long)` or `(short long)` where `short` and `long` are both strings representing the short and long captions
- **`post-blank`**: a non-negative integer

```el
(->> (org-ml-build-table-cell "cell")
  (org-ml-build-table-row)
  (org-ml-build-table)
  (org-ml-to-trimmed-string))
 ;; => "| cell |"

```


### Miscellaneous Builders

#### org-ml-clone-node `(node)`

Return copy of **`node`**, which may be a circular tree.

This is only necessary to copy nodes parsed using any of parsing
functions from this package (for example, `org-ml-parse-this-headline`)
because these retain parent references which makes the node a circular
list. None of the builder functions add parent references, so
`copy-tree` will be a faster alternative to this function.

```el
;; Given the following contents:
; dolly

(let* ((node1 (org-ml-parse-this-element))
       (node2 (org-ml-clone-node node1)))
  (equal node1 node2))
 ;; => t

(let* ((node1 (org-ml-parse-this-element))
       (node2 (org-ml-clone-node node1)))
  (eq node1 node2))
 ;; => nil

```

#### org-ml-clone-node-n `(n node)`

Like [`org-ml-clone-node`](#org-ml-clone-node-node) but make **`n`** copies of **`node`**.

```el
;; Given the following contents:
; dolly

(-let* ((node1 (org-ml-parse-this-element))
	((node2 node3)
	 (org-ml-clone-node-n 2 node1)))
  (or (equal node1 node2)
      (equal node1 node3)
      (equal node2 node3)))
 ;; => t

(-let* ((node1 (org-ml-parse-this-element))
	((node2 node3)
	 (org-ml-clone-node-n 2 node1)))
  (or (eq node1 node2)
      (eq node1 node3)
      (eq node2 node3)))
 ;; => nil

```

#### org-ml-build-secondary-string! `(string)`

Return a secondary string (list of object nodes) from **`string`**.
**`string`** is any string that contains a textual representation of
object nodes. If the string does not represent a list of object nodes,
throw an error.

```el
(->> (org-ml-build-secondary-string! "I'm plain")
  (-map #'org-ml-get-type))
 ;; => '(plain-text)

(->> (org-ml-build-secondary-string! "I'm *not* plain")
  (-map #'org-ml-get-type))
 ;; => '(plain-text bold plain-text)

(->> (org-ml-build-secondary-string! "* I'm not an object")
  (-map #'org-ml-get-type))
Error

```

#### org-ml-build-table-row-hline `(&key post-blank)`

Return a new rule-typed table-row node.
Optionally set **`post-blank`** (a positive integer).

```el
(->> (org-ml-build-table (org-ml-build-table-row (org-ml-build-table-cell "text"))
			 (org-ml-build-table-row-hline))
  (org-ml-to-trimmed-string))
 ;; => "| text |
 ;      |------|"

```

#### org-ml-build-timestamp-diary `(form &key post-blank)`

Return a new diary-sexp timestamp node from **`form`**.
Optionally set **`post-blank`** (a positive integer).

```el
(->> (org-ml-build-timestamp-diary '(diary-float t 4 2))
  (org-ml-to-string))
 ;; => "<%%(diary-float t 4 2)>"

```


### Shorthand Builders


Build nodes with more convenient/shorter syntax.

#### org-ml-build-timestamp! `(start &key end active repeater warning post-blank)`

Return a new timestamp node.

**`start`** specifies the start time and is a list of integers in one of
the following forms:
- `(year month day)`: short form
- `(year month day nil nil)`: short form
- `(year month day hour minute)`: long form

**`end`** (if supplied) will add the ending time, and follows the same
formatting rules as **`start`**.

**`active`** is a boolean where t signifies the type is `active`, else
`inactive` (the range suffix will be added if an end time is
supplied).

**`repeater`** and **`warning`** are lists corresponding to those required
for [`org-ml-timestamp-set-repeater`](#org-ml-timestamp-set-repeater-repeater-timestamp) and
[`org-ml-timestamp-set-warning`](#org-ml-timestamp-set-warning-warning-timestamp) respectively.

Building a diary sexp timestamp is not possible with this function.

```el
(->> (org-ml-build-timestamp! '(2019 1 1))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue]"

(->> (org-ml-build-timestamp! '(2019 1 1 12 0)
			       :active t :warning '(all 1 day)
			       :repeater '(cumulate 1 month))
  (org-ml-to-string))
 ;; => "<2019-01-01 Tue 12:00 +1m -1d>"

(->> (org-ml-build-timestamp! '(2019 1 1)
			       :end '(2019 1 2))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```

#### org-ml-build-clock! `(start &key end post-blank)`

Return a new clock node.

**`start`** and **`end`** follow the same rules as their respective arguments in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
(->> (org-ml-build-clock! '(2019 1 1))
  (org-ml-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue]"

(->> (org-ml-build-clock! '(2019 1 1 12 0))
  (org-ml-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 12:00]"

(->> (org-ml-build-clock! '(2019 1 1 12 0)
			   :end '(2019 1 1 13 0))
  (org-ml-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00] =>  1:00"

```

#### org-ml-build-planning! `(&key closed deadline scheduled post-blank)`

Return a new planning node.

**`closed`**, **`deadline`**, and **`scheduled`** are lists with the following structure
(brackets denote optional members):

`(year minute day [hour] [min]
 [&warning type value unit]
 [&repeater type value unit])`

In terms of arguments supplied to [`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank), the first
five members correspond to the list supplied as `time`, and the `type`,
`value`, and `unit` fields correspond to the lists supplied to `warning` and
`repeater` arguments. The order of warning and repeater does not
matter.

```el
(->> (org-ml-build-planning! :closed '(2019 1 1))
  (org-ml-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

(->> (org-ml-build-planning! :closed '(2019 1 1)
			      :scheduled '(2018 1 1))
  (org-ml-to-trimmed-string))
 ;; => "SCHEDULED: <2018-01-01 Mon> CLOSED: [2019-01-01 Tue]"

(->> (org-ml-build-planning! :closed '(2019 1 1 &warning all 1 day &repeater cumulate 1 month))
  (org-ml-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue +1m -1d]"

```

#### org-ml-build-property-drawer! `(&key post-blank &rest keyvals)`

Return a new property-drawer node.

Each member in **`keyvals`** is a list like `(key val)` where `key` and `val`
are both strings, where each list will generate a node-property
node in the property-drawer node like `":key: val"`.

```el
(->> (org-ml-build-property-drawer! '(key val))
  (org-ml-to-trimmed-string))
 ;; => ":PROPERTIES:
 ;      :key:      val
 ;      :END:"

```

#### org-ml-build-headline! `(&key (level 1) title-text todo-keyword tags pre-blank priority commentedp archivedp post-blank planning statistics-cookie section-children &rest subheadlines)`

Return a new headline node.

**`title-text`** is a oneline string for the title of the headline.

**`planning`** is a list like `(planning-type args ...)` where
`planning-type` is one of `:closed`, `:deadline`, or `:scheduled`, and
`args` are the args supplied to any of the planning types in
[`org-ml-build-planning!`](#org-ml-build-planning-key-closed-deadline-scheduled-post-blank). Up to all three planning types can be used
in the same list like `(:closed args :deadline args :scheduled args)`.

**`statistics-cookie`** is a list following the same format as
[`org-ml-build-statistics-cookie`](#org-ml-build-statistics-cookie-value-key-post-blank).

**`section-children`** is a list of elements that will go in the headline
section.

**`subheadlines`** contains zero or more headlines that will go under the
created headline. The level of all members in **`subheadlines`** will
automatically be adjusted to **`level`** + 1.

All arguments not mentioned here follow the same rules as
[`org-ml-build-headline`](#org-ml-build-headline-key-archivedp-commentedp-footnote-section-p-level-1-pre-blank-0-priority-tags-title-todo-keyword-post-blank-rest-element-nodes)

```el
(->> (org-ml-build-headline! :title-text "really impressive title")
  (org-ml-to-trimmed-string))
 ;; => "* really impressive title"

(->> (org-ml-build-headline! :title-text "really impressive title" :statistics-cookie '(0 9000))
  (org-ml-to-trimmed-string))
 ;; => "* really impressive title [0/9000]"

(->> (org-ml-build-headline! :title-text "really impressive title" :section-children (list (org-ml-build-property-drawer! '(key val))
											   (org-ml-build-paragraph! "section text"))
			      (org-ml-build-headline! :title-text "subhead"))
  (org-ml-to-trimmed-string))
 ;; => "* really impressive title
 ;      :PROPERTIES:
 ;      :key:      val
 ;      :END:
 ;      section text
 ;      ** subhead"

```

#### org-ml-build-item! `(&key post-blank bullet checkbox tag paragraph counter &rest children)`

Return a new item node.

**`tag`** is a string representing the tag (make with
[`org-ml-build-secondary-string!`](#org-ml-build-secondary-string-string)) .

**`paragraph`** is a string that will be the initial text in the item
(made with [`org-ml-build-paragraph!`](#org-ml-build-paragraph-string-key-post-blank)).

**`children`** contains the nodes that will go under this item after
**`paragraph`**.

All other arguments follow the same rules as [`org-ml-build-item`](#org-ml-build-item-key-bullet---pre-blank-0-checkbox-counter-tag-post-blank-rest-element-nodes).

```el
(->> (org-ml-build-item! :bullet 1 :tag "complicated *tag*" :paragraph "petulant /frenzy/" (org-ml-build-plain-list (org-ml-build-item! :bullet '- :paragraph "below")))
  (org-ml-to-trimmed-string))
 ;; => "1. complicated *tag* :: petulant /frenzy/
 ;           - below"

```

#### org-ml-build-paragraph! `(string &key post-blank)`

Return a new paragraph node from **`string`**.

**`string`** is the text to be parsed into a paragraph and must contain
valid textual representations of object nodes.

```el
(->> (org-ml-build-paragraph! "stuff /with/ *formatting*" :post-blank 2)
  (org-ml-to-string))
 ;; => "stuff /with/ *formatting*
 ;      
 ;      
 ;      "

(->> (org-ml-build-paragraph! "* stuff /with/ *formatting*")
  (org-ml-to-string))
Error

```

#### org-ml-build-table-cell! `(string)`

Return a new table-cell node.

**`string`** is the text to be contained in the table-cell node. It must
contain valid textual representations of objects that are allowed in
table-cell nodes.

```el
(->> (org-ml-build-table-cell! "rage")
  (org-ml-to-trimmed-string))
 ;; => "rage |"

(->> (org-ml-build-table-cell! "*rage*")
  (org-ml-to-trimmed-string))
 ;; => "*rage* |"

```

#### org-ml-build-table-row! `(row-list)`

Return a new table-row node.

**`row-list`** is a list of strings to be built into table-cell nodes via
[`org-ml-build-table-cell!`](#org-ml-build-table-cell-string) (see that function for restrictions).
Alternatively, **`row-list`** may the symbol `hline` instead of a string to
create a rule-typed table-row.

```el
(->> (org-ml-build-table-row! '("R" "A" "G" "E"))
  (org-ml-to-trimmed-string))
 ;; => "| R | A | G | E |"

(->> (org-ml-build-table-row! 'hline)
  (org-ml-to-trimmed-string))
 ;; => "|-"

```

#### org-ml-build-table! `(&key tblfm post-blank &rest row-lists)`

Return a new table node.

**`row-lists`** is a list of lists where each member list will be converted
to a table-row node via [`org-ml-build-table-row!`](#org-ml-build-table-row-row-list) (see that function for
restrictions).

All other arguments follow the same rules as [`org-ml-build-table`](#org-ml-build-table-key-tblfm-name-plot-header-results-caption-post-blank-rest-element-nodes).

```el
(->> (org-ml-build-table! '("R" "A")
			   '("G" "E"))
  (org-ml-to-trimmed-string))
 ;; => "| R | A |
 ;      | G | E |"

(->> (org-ml-build-table! '("L" "O")
			   'hline '("V" "E"))
  (org-ml-to-trimmed-string))
 ;; => "| L | O |
 ;      |---+---|
 ;      | V | E |"

```


### Logbook Item Builders


Build item nodes for inclusion in headline logbooks

#### org-ml-build-log-note `(unixtime note)`

Return an item node for a new note log entry.

This will format the log entry from the default value for the
'note` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

**`note`** is a string for the note text.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-note "noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- Note taken on [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-done `(unixtime &optional note)`

Return an item node for a done log entry.

This will format the log entry from the default value for the
'done` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-done)
  (org-ml-to-trimmed-string))
 ;; => "- CLOSING NOTE [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-done "noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- CLOSING NOTE [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-refile `(unixtime &optional note)`

Return an item node for a refile log entry.
This will format the log entry from the default value for the
'deldeadline` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-refile)
  (org-ml-to-trimmed-string))
 ;; => "- Refiled on [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-refile "noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- Refiled on [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-state `(unixtime new-state old-state &optional note)`

Return an item node for a state change log entry.

This will format the log entry from the default value for the
'state` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

**`new-state`** and **`old-state`** are strings for the new and old todo keywords
respectively.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-state "HOLD" "TODO")
  (org-ml-to-trimmed-string))
 ;; => "- State \"HOLD\"       from \"TODO\"       [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-state "HOLD" "TODO" "noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- State \"HOLD\"       from \"TODO\"       [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-deldeadline `(unixtime old-timestamp &optional note)`

Return an item node for a delete deadline log entry.

This will format the log entry from the default value for the
'deldeadline` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

**`old-timestamp`** is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-deldeadline (org-ml-build-timestamp! '(2019 1 2)))
  (org-ml-to-trimmed-string))
 ;; => "- Removed deadline, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-deldeadline (org-ml-build-timestamp! '(2019 1 2))
				"noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- Removed deadline, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-delschedule `(unixtime old-timestamp &optional note)`

Return an item node for a delete schedule log entry.

This will format the log entry from the default value for the
'delschedule` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

**`old-timestamp`** is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-delschedule (org-ml-build-timestamp! '(2019 1 2)))
  (org-ml-to-trimmed-string))
 ;; => "- Not scheduled, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-delschedule (org-ml-build-timestamp! '(2019 1 2))
				"noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- Not scheduled, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-redeadline `(unixtime old-timestamp &optional note)`

Return an item node for a new deadline log entry.

This will format the log entry from the default value for the
'redeadline` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

**`old-timestamp`** is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-redeadline (org-ml-build-timestamp! '(2019 1 2)))
  (org-ml-to-trimmed-string))
 ;; => "- New deadline from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-redeadline (org-ml-build-timestamp! '(2019 1 2))
			       "noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- New deadline from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-reschedule `(unixtime old-timestamp &optional note)`

Return an item node for a new schedule log entry.

This will format the log entry from the default value for the
'reschedule` cell in `org-log-note-headings`.

**`unixtime`** is an integer representing the time to be used for all
timestamp nodes.

**`old-timestamp`** is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string **`note`** is supplied, append a note to the log entry.

```el
(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-reschedule (org-ml-build-timestamp! '(2019 1 2)))
  (org-ml-to-trimmed-string))
 ;; => "- Rescheduled from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]"

(-> (- 1546300800 (car (current-time-zone)))
  (org-ml-build-log-reschedule (org-ml-build-timestamp! '(2019 1 2))
			       "noteworthy")
  (org-ml-to-trimmed-string))
 ;; => "- Rescheduled from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\
 ;        noteworthy"

```

#### org-ml-build-log-type `(type &key old new unixtime username full-username note)`

Return an item for an arbitrary log entry.

**`type`** is a symbol corresponding to the car of one of the cells in
`org-log-note-headings`. Unlike the other log entry build functions
in this package, this function will not use the default value of
`org-log-note-headings` which means it can be used for customly
formatted log entries.

The arguments correspond to the following formatting placeholders
(see `org-log-note-headings` for more information on these
placeholders):
- **`new`**: either a string or timestamp node that will replace the
    new state/timestamp placeholder (%s)
- **`old`**: like **`new`** but for the old state/timestamp placeholder (%S)
- **`unixtime`**: an integer corresponding to the time to be used for the
    timestamp placeholders (%t/%T/%d/%D)
- **`username`**: a string for the username (%u)
- **`full-username`**: a string for the full username (%U)

If any of these arguments are not supplied but their placeholders
are present in the heading determined by **`type`**, the placeholders will
not be substituted.

If string **`note`** is supplied, append a note to the log entry.

```el
(let ((org-log-note-headings '((test . "Changed %s from %S on %t by %u")))
      (ut (- 1546300800 (car (current-time-zone)))))
  (->> (org-ml-build-log-type 'test :unixtime ut :old "TODO" :new "DONE" :username "shadowbrokers" :note "We're coming for you")
    (org-ml-to-trimmed-string)))
 ;; => "- Changed \"DONE\" from \"TODO\" on [2019-01-01 Tue 00:00] by shadowbrokers \\\\
 ;        We're coming for you"

```


## Type Predicates


Test node types.

#### org-ml-get-type `(node)`

Return the type of **`node`**.

```el
;; Given the following contents:
; *I'm emboldened*

(->> (org-ml-parse-this-object)
  (org-ml-get-type))
 ;; => 'bold

;; Given the following contents:
; * I'm the headliner

(->> (org-ml-parse-this-element)
  (org-ml-get-type))
 ;; => 'headline

;; Given the following contents:
; [2112-12-21 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-get-type))
 ;; => 'timestamp

```

#### org-ml-is-type `(type node)`

Return t if the type of **`node`** is **`type`** (a symbol).

```el
;; Given the following contents:
; *ziltoid*

(->> (org-ml-parse-this-object)
  (org-ml-is-type 'bold))
 ;; => t

(->> (org-ml-parse-this-object)
  (org-ml-is-type 'italic))
 ;; => nil

```

#### org-ml-is-any-type `(types node)`

Return t if the type of **`node`** is in **`types`** (a list of symbols).

```el
;; Given the following contents:
; *ziltoid*

(->> (org-ml-parse-this-object)
  (org-ml-is-any-type '(bold)))
 ;; => t

(->> (org-ml-parse-this-object)
  (org-ml-is-any-type '(bold italic)))
 ;; => t

(->> (org-ml-parse-this-object)
  (org-ml-is-any-type '(italic)))
 ;; => nil

```

#### org-ml-is-element `(node)`

Return t if **`node`** is an element class.

```el
;; Given the following contents:
; *ziltoid*

;; Parsing this text as an element node gives a paragraph node
(->> (org-ml-parse-this-element)
  (org-ml-is-element))
 ;; => t

;; Parsing the same text as an object node gives a bold node
(->> (org-ml-parse-this-object)
  (org-ml-is-element))
 ;; => nil

```

#### org-ml-is-branch-node `(node)`

Return t if **`node`** is a branch node.

```el
;; Given the following contents:
; *ziltoid*

;; Parsing this as an element node gives a paragraph node (a branch node)
(->> (org-ml-parse-this-element)
  (org-ml-is-branch-node))
 ;; => t

;; Parsing this as an object node gives a bold node (also a branch node)
(->> (org-ml-parse-this-object)
  (org-ml-is-branch-node))
 ;; => t

;; Given the following contents:
; ~ziltoid~

;; Parsing this as an object node gives a code node (not a branch node)
(->> (org-ml-parse-this-object)
  (org-ml-is-branch-node))
 ;; => nil

;; Given the following contents:
; # ziltoid

;; Parsing this as an element node gives a comment node (also not a branch node)
(->> (org-ml-parse-this-element)
  (org-ml-is-branch-node))
 ;; => nil

;; Given the following contents:
; * I'm so great

;; Parsing this as an element node gives a headline node (a branch node)
(->> (org-ml-parse-this-element)
  (org-ml-is-branch-node))
 ;; => t

```

#### org-ml-node-may-have-child-objects `(node)`

Return t if **`node`** is a branch node that may have child objects.

```el
;; Given the following contents:
; *ziltoid*

;; Parsing this as an element node gives a paragraph node (can have child object
;; nodes)
(->> (org-ml-parse-this-element)
  (org-ml-node-may-have-child-objects))
 ;; => t

;; Parsing this as an object node gives a bold node (also can have child object
;; nodes)
(->> (org-ml-parse-this-object)
  (org-ml-node-may-have-child-objects))
 ;; => t

;; Given the following contents:
; ~ziltoid~

;; Parsing this as an object node gives a code node (not a branch node)
(->> (org-ml-parse-this-object)
  (org-ml-node-may-have-child-objects))
 ;; => nil

;; Given the following contents:
; # ziltoid

;; Parsing this as an element node gives a comment node (not a branch node)
(->> (org-ml-parse-this-element)
  (org-ml-node-may-have-child-objects))
 ;; => nil

;; Given the following contents:
; * I'm so great

;; Parsing this as an element node gives a headline node (can only have child
;; element nodes)
(->> (org-ml-parse-this-element)
  (org-ml-node-may-have-child-objects))
 ;; => nil

```

#### org-ml-node-may-have-child-elements `(node)`

Return t if **`node`** is a branch node that may have child elements.

Note this implies that **`node`** is also of class element since only
elements may have other elements as children.

```el
;; Given the following contents:
; * I'm so great

;; Parsing this as an element node gives a headline node (can have child element
;; nodes)
(->> (org-ml-parse-this-element)
  (org-ml-node-may-have-child-elements))
 ;; => t

;; Given the following contents:
; *ziltoid*

;; Parsing this as an element node gives a paragraph node (can only have child
;; object nodes)
(->> (org-ml-parse-this-element)
  (org-ml-node-may-have-child-elements))
 ;; => nil

;; Given the following contents:
; # ziltoid

;; Parsing this as an element node gives a comment node (not a branch node)
(->> (org-ml-parse-this-element)
  (org-ml-node-may-have-child-elements))
 ;; => nil

```


## Property Manipulation


Set, get, and map properties of nodes.


### Generic

#### org-ml-contains-point-p `(point node)`

Return t if **`point`** is within the boundaries of **`node`**.

```el
;; Given the following contents:
; *findme*

(->> (org-ml-parse-this-object)
  (org-ml-contains-point-p 2))
 ;; => t

(->> (org-ml-parse-this-object)
  (org-ml-contains-point-p 10))
 ;; => nil

```

#### org-ml-set-property `(prop value node)`

Return **`node`** with **`prop`** set to **`value`**.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; #+call: ktulu()

(->> (org-ml-parse-this-element)
  (org-ml-set-property :call "cthulhu")
  (org-ml-set-property :inside-header '(:cache no))
  (org-ml-set-property :arguments '("x=4"))
  (org-ml-set-property :end-header '(:exports results))
  (org-ml-to-trimmed-string))
 ;; => "#+call: cthulhu[:cache no](x=4) :exports results"

;; Given the following contents:
; call_kthulu()

(->> (org-ml-parse-this-object)
  (org-ml-set-property :call "cthulhu")
  (org-ml-set-property :inside-header '(:cache no))
  (org-ml-set-property :arguments '("x=4"))
  (org-ml-set-property :end-header '(:exports results))
  (org-ml-to-trimmed-string))
 ;; => "call_cthulhu[:cache no](x=4)[:exports results]"

;; Given the following contents:
; src_emacs{(print 'yeah-boi)}

(->> (org-ml-parse-this-object)
  (org-ml-set-property :language "python")
  (org-ml-set-property :parameters '(:cache no))
  (org-ml-set-property :value "print \"yeah boi\"")
  (org-ml-to-trimmed-string))
 ;; => "src_python[:cache no]{print \"yeah boi\"}"

;; Given the following contents:
; - thing

(->> (org-ml-parse-this-item)
  (org-ml-set-property :bullet 1)
  (org-ml-set-property :checkbox 'on)
  (org-ml-set-property :counter 2)
  (org-ml-set-property :tag '("tmsu"))
  (org-ml-to-trimmed-string))
 ;; => "1. [@2] [X] tmsu :: thing"

;; Given the following contents:
; * not valuable

;; Throw error when setting a property that doesn't exist
(->> (org-ml-parse-this-headline)
  (org-ml-set-property :value "wtf")
  (org-ml-to-trimmed-string))
Error

;; Throw error when setting to an improper type
(->> (org-ml-parse-this-headline)
  (org-ml-set-property :title 666)
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-get-property `(prop node)`

Return the value of **`prop`** of **`node`**.

```el
;; Given the following contents:
; #+call: ktulu(x=4) :exports results

(->> (org-ml-parse-this-element)
  (org-ml-get-property :call))
 ;; => "ktulu"

(->> (org-ml-parse-this-element)
  (org-ml-get-property :inside-header))
 ;; => nil

;; Given the following contents:
; * not arguable

;; Throw error when requesting a property that doesn't exist
(->> (org-ml-parse-this-headline)
  (org-ml-get-property :value))
Error

```

#### org-ml-map-property `(prop fun node)`

Return **`node`** with **`fun`** applied to the value of **`prop`**.

**`fun`** is a unary function which takes the current value of **`prop`** and
returns a new value to which **`prop`** will be set.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; ~learn to~

(->> (org-ml-parse-this-object)
  (org-ml-map-property :value #'s-upcase)
  (org-ml-to-trimmed-string))
 ;; => "~LEARN TO~"

;; Throw error if property doesn't exist
(->> (org-ml-parse-this-object)
  (org-ml-map-property :title #'s-upcase)
  (org-ml-to-trimmed-string))
Error

;; Throw error if function doesn't return proper type
(->> (org-ml-parse-this-object)
  (org-ml-map-property* :value (if it 1 0))
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-toggle-property `(prop node)`

Return **`node`** with the value of **`prop`** flipped.

This function only applies to properties that are booleans.

The following types and properties are supported:

entity
- :use-brackets-p

example-block
- :preserve-indent

headline
- :archivedp
- :commentedp
- :footnote-section-p

src-block
- :preserve-indent

subscript
- :use-brackets-p

superscript
- :use-brackets-p

```el
;; Given the following contents:
; \pi

(->> (org-ml-parse-this-object)
  (org-ml-toggle-property :use-brackets-p)
  (org-ml-to-trimmed-string))
 ;; => "\\pi{}"

;; Given the following contents:
; - [ ] nope

;; Throw an error when trying to toggle a non-boolean property
(->> (org-ml-parse-this-item)
  (org-ml-toggle-property :checkbox)
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-shift-property `(prop n node)`

Return **`node`** with **`prop`** shifted by **`n`** (an integer).

This only applies the properties that are represented as integers.

The following types and properties are supported:

all elements
- :post-blank

footnote-definition
- :pre-blank

headline
- :level
- :pre-blank
- :priority

item
- :pre-blank
- :counter

```el
;; Given the following contents:
; * no priorities

;; Do nothing if there is nothing to shift.
(->> (org-ml-parse-this-headline)
  (org-ml-shift-property :priority 1)
  (org-ml-to-trimmed-string))
 ;; => "* no priorities"

;; Given the following contents:
; * [#A] priorities

(->> (org-ml-parse-this-headline)
  (org-ml-shift-property :priority -1)
  (org-ml-to-trimmed-string))
 ;; => "* [#B] priorities"

;; Wrap priority around when crossing the min or max
(->> (org-ml-parse-this-headline)
  (org-ml-shift-property :priority 1)
  (org-ml-to-trimmed-string))
 ;; => "* [#C] priorities"

;; Given the following contents:
; * TODO or not todo

;; Throw error when shifting an unshiftable property
(->> (org-ml-parse-this-headline)
  (org-ml-shift-property :todo-keyword 1)
  (org-ml-to-string))
Error

```

#### org-ml-insert-into-property `(prop index string node)`

Return **`node`** with **`string`** inserted at **`index`** into **`prop`**.

This only applies to properties that are represented as lists of
strings.

The following types and properties are supported:

babel-call
- :arguments

example-block
- :switches

headline
- :tags

inline-babel-call
- :arguments

macro
- :args

src-block
- :switches

table
- :tblfm

```el
;; Given the following contents:
; #+call: ktulu(y=1)

(->> (org-ml-parse-this-element)
  (org-ml-insert-into-property :arguments 0 "x=4")
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu(x=4,y=1)"

;; Do nothing if the string is already in the list
(->> (org-ml-parse-this-element)
  (org-ml-insert-into-property :arguments 0 "y=1")
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu(y=1)"

;; Throw error when inserting into a property that is not a list of strings
(->> (org-ml-parse-this-element)
  (org-ml-insert-into-property :end-header 0 "html")
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-remove-from-property `(prop string node)`

Return **`node`** with **`string`** removed from **`prop`** if present.

This only applies to properties that are represented as lists of
strings.

See [`org-ml-insert-into-property`](#org-ml-insert-into-property-prop-index-string-node) for a list of supported elements
and properties that may be used with this function.

```el
;; Given the following contents:
; #+call: ktulu(y=1)

(->> (org-ml-parse-this-element)
  (org-ml-remove-from-property :arguments "y=1")
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu()"

;; Do nothing if the string does not exist
(->> (org-ml-parse-this-element)
  (org-ml-remove-from-property :arguments "d=666")
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu(y=1)"

;; Throw error when removing from property that is not a string list
(->> (org-ml-parse-this-element)
  (org-ml-remove-from-property :end-header ":results")
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-plist-put-property `(prop key value node)`

Return **`node`** with **`value`** corresponding to **`key`** inserted into **`prop`**.

**`key`** is a keyword and **`value`** is a symbol. This only applies to
properties that are represented as plists.

The following types and properties are supported:

babel-call
- :inside-header
- :end-header

dynamic-block
- :arguments

inline-babel-call
- :inside-header
- :end-header

inline-src-block
- :parameters

src-block
- :parameters

```el
;; Given the following contents:
; #+call: ktulu[:cache no]()

(->> (org-ml-parse-this-element)
  (org-ml-plist-put-property :end-header :results 'html)
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu[:cache no]() :results html"

;; Change the value of key if it already is present
(->> (org-ml-parse-this-element)
  (org-ml-plist-put-property :inside-header :cache 'yes)
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu[:cache yes]()"

;; Do nothing if the key and value already exist
(->> (org-ml-parse-this-element)
  (org-ml-plist-put-property :inside-header :cache 'no)
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu[:cache no]()"

;; Throw error if setting property that isn't a plist
(->> (org-ml-parse-this-element)
  (org-ml-plist-put-property :arguments :cache 'no)
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-plist-remove-property `(prop key node)`

Return **`node`** with **`key`** and its corresponding value removed from **`prop`**.

**`key`** is a keyword. This only applies to properties that are
represented as plists.

See [`org-ml-plist-put-property`](#org-ml-plist-put-property-prop-key-value-node) for a list of supported elements
and properties that may be used with this function.

```el
;; Given the following contents:
; #+call: ktulu() :results html

(->> (org-ml-parse-this-element)
  (org-ml-plist-remove-property :end-header :results)
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu()"

;; Do nothing if the key is not present
(->> (org-ml-parse-this-element)
  (org-ml-plist-remove-property :inside-header :cache)
  (org-ml-to-trimmed-string))
 ;; => "#+call: ktulu() :results html"

;; Throw error if trying to remove key from non-plist property
(->> (org-ml-parse-this-element)
  (org-ml-plist-remove-property :arguments :cache)
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-get-properties `(props node)`

Return all the values of **`props`** from **`node`**.
**`props`** is a list of all the properties desired, and the returned
list will be the values of these properties in the order
requested. To get the raw plist of **`node`**, use
`org-ml--get-all-properties`.

```el
;; Given the following contents:
; call_ktulu[:cache no](x=4)[:exports results]

(->> (org-ml-parse-this-object)
  (org-ml-get-properties '(:call :inside-header :arguments :end-header)))
 ;; => '("ktulu" (:cache no) ("x=4") (:exports results))

```

#### org-ml-get-all-properties `(node)`

Return the properties list of **`node`**.

```el
;; Given the following contents:
; *bold*

(--> (org-ml-parse-this-object)
  (org-ml-get-all-properties it)
  (plist-put it :parent nil))
 ;; => '(:begin 1 :end 7 :contents-begin 2 :contents-end 6 :post-blank 0 :parent nil)

```

#### org-ml-set-properties `(plist node)`

Return **`node`** with all properties set to the values according to **`plist`**.

**`plist`** is a list of property-value pairs that corresponds to the
property list in **`node`**.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; - thing

(->> (org-ml-parse-this-item)
  (org-ml-set-properties (list :bullet 1 :checkbox 'on :counter 2 :tag '("tmsu")))
  (org-ml-to-trimmed-string))
 ;; => "1. [@2] [X] tmsu :: thing"

;; Given the following contents:
; - plain

(->> (org-ml-parse-this-element)
  (org-ml-set-properties (list :name "plain name" :attr_XXX '("tmsu")))
  (org-ml-to-trimmed-string))
 ;; => "#+name: plain name
 ;      #+attr_xxx: tmsu
 ;      - plain"

```

#### org-ml-map-properties `(plist node)`

Return **`node`** with functions applied to the values of properties.

**`plist`** is a property list where the keys are properties of **`node`** and
its values are unary functions to be mapped to these properties.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; #+KEY: VAL

(->> (org-ml-parse-this-element)
  (org-ml-map-properties (list :key (-partial #'s-prepend "OM_")
				:value (-partial #'s-prepend "OM_")))
  (org-ml-to-trimmed-string))
 ;; => "#+om_key: OM_VAL"

```

#### org-ml-get-parents `(node)`

Return parents of **`node`** as a list.
The toplevel parent will be the left-most member, and **`node`** itself
will be the rightmost member.

```el
;; Given the following contents:
; * one
; ** two
; *** three

(->> (org-ml-parse-this-subtree)
  (org-ml-get-parents)
  (--map (org-ml-get-property :begin it)))
 ;; => '(1)

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-subheadlines)
  (car)
  (org-ml-headline-get-subheadlines)
  (car)
  (org-ml-get-parents)
  (--map (org-ml-get-property :begin it)))
 ;; => '(1 7 14)

```

#### org-ml-remove-parent `(node)`

Return **`node`** with the :parent property set to nil.

Short synopsis:

Use this function to declutter a node if you are trying to print
its literal list representation or you are running into infinite
loops caused by self-referential lists (there are probably other
valid reasons but these are the main ones).

Gory details:

The :parent property refers to the node one level higher in the
tree that contains **`node`** as a child. It will be present in a node
that is generated from a parse operation with
`org-ml-parse-this-buffer` or related. This property offers a
nice shortcut to traverse up the node tree from a child. Besides
this, it is not necessary as the tree structure itself already
encodes all parent-child relationships. Further, it is not used
by org-element internally to convert nodes into strings (such as
with [`org-ml-to-string`](#org-ml-to-string-node)) and thus can be thought of as a
'read-only' property. This is why :parent will be set to nil when
building a new node with the 'org-ml-build-' family of functions
and why [`org-ml-set-property`](#org-ml-set-property-prop-value-node) forbids setting this property.

In many cases, one can safely ignore :parent unless, of course,
one actually needs to read it with [`org-ml-get-parents`](#org-ml-get-parents-node) or
[`org-ml-get-property`](#org-ml-get-property-prop-node). However, it heavily clutters the list
representation of nodes, and therefore it is nice to remove this
property whenever literal node lists are printed/visualized (eg
for debugging). Note that for deep trees, each parent will itself
have a :parent property pointing to its own parent, with this
pattern repeating until the top of the tree.

Furthermore, each parent will itself contain its own child node,
which implies a circular/self-referential list. For the most
part, this won't matter. However, some functions don't like
dealing with circular lists and will complain about infinite
recursion. If this is happening, the :parent property is likely
to blame, and setting it to nil has a high probability of fixing
the issue.

```el
;; Given the following contents:
; one

;; This is actually a paragraph node, but parsing the object will directly
;; return a plain-text node with the :parent pointing to the paragraph
(->> (org-ml-parse-this-object)
  (org-ml-remove-parent))
 ;; => "one"

;; Given the following contents:
; * headline

(->> (org-ml-parse-this-element)
  (org-ml-remove-parents))
 ;; => '(headline (:raw-value "headline" :begin 1 :end 11 :pre-blank 0 :contents-begin nil :contents-end nil :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 1 :title ("headline") :parent nil))

;; Given the following contents:
; - tag :: thingy

(->> (org-ml-parse-this-item)
  (org-ml-remove-parents))
 ;; => '(item (:bullet "- " :begin 1 :end 16 :contents-begin 10 :contents-end 16 :checkbox nil :counter nil :structure ((1 0 "- " nil nil "tag" 16)) :pre-blank 0 :post-blank 0 :post-affiliated 1 :tag ("tag") :parent nil) (paragraph (:begin 10 :end 16 :contents-begin 10 :contents-end 16 :post-blank 0 :post-affiliated 10 :parent nil) "thingy"))

```


### Clock

#### org-ml-clock-is-running `(clock)`

Return t if **`clock`** element is running (eg is open).

```el
;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]

(->> (org-ml-parse-this-element)
  (org-ml-clock-is-running))
 ;; => t

;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00

(->> (org-ml-parse-this-element)
  (org-ml-clock-is-running))
 ;; => nil

```


### Entity

#### org-ml-entity-get-replacement `(key entity)`

Return replacement string or symbol for **`entity`** node.

**`key`** is one of:
- `:latex` (the entity's latex representation)
- `:latex-math-p` (t if the latex representation requires math mode,
    nil otherwise)
- `:html` (the entity's html representation)
- `:ascii` (the entity's ascii representation)
- `:latin1` (the entity's Latin1 representation)
- `:utf-8` (the entity's utf8 representation)

Any other keys will trigger an error.

```el
;; Given the following contents:
; \pi{}

(->> (org-ml-parse-this-object)
  (org-ml-entity-get-replacement :latex))
 ;; => "\\pi"

(->> (org-ml-parse-this-object)
  (org-ml-entity-get-replacement :latex-math-p))
 ;; => t

(->> (org-ml-parse-this-object)
  (org-ml-entity-get-replacement :html))
 ;; => "&pi;"

(->> (org-ml-parse-this-object)
  (org-ml-entity-get-replacement :ascii))
 ;; => "pi"

(->> (org-ml-parse-this-object)
  (org-ml-entity-get-replacement :latin1))
 ;; => "pi"

(->> (org-ml-parse-this-object)
  (org-ml-entity-get-replacement :utf-8))
 ;; => "π"

```


### Headline

#### org-ml-headline-set-title! `(title-text stats-cookie-value headline)`

Return **`headline`** node with new title.

**`title-text`** is a string to be parsed into object nodes for the title
via [`org-ml-build-secondary-string!`](#org-ml-build-secondary-string-string) (see that function for restrictions)
and **`stats-cookie-value`** is a list described in
[`org-ml-build-statistics-cookie`](#org-ml-build-statistics-cookie-value-key-post-blank).

```el
;; Given the following contents:
; * really impressive title

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-title! "really *impressive* title" '(2 3))
  (org-ml-to-trimmed-string))
 ;; => "* really *impressive* title [2/3]"

```

#### org-ml-headline-is-done `(headline)`

Return t if **`headline`** node has a done todo-keyword.

```el
;; Given the following contents:
; * TODO darn

(->> (org-ml-parse-this-headline)
  (org-ml-headline-is-done))
 ;; => nil

;; Given the following contents:
; * DONE yay

(->> (org-ml-parse-this-headline)
  (org-ml-headline-is-done))
 ;; => t

```

#### org-ml-headline-has-tag `(tag headline)`

Return t if **`headline`** node is tagged with **`tag`**.

```el
;; Given the following contents:
; * dummy

(->> (org-ml-parse-this-headline)
  (org-ml-headline-has-tag "tmsu"))
 ;; => nil

;; Given the following contents:
; * dummy                  :tmsu:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-has-tag "tmsu"))
 ;; => t

```

#### org-ml-headline-get-statistics-cookie `(headline)`

Return the statistics cookie node from **`headline`** if it exists.

```el
;; Given the following contents:
; * statistically significant [10/10]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-statistics-cookie)
  (org-ml-to-string))
 ;; => "[10/10]"

;; Given the following contents:
; * not statistically significant

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-statistics-cookie))
 ;; => nil

```


### Item

#### org-ml-item-toggle-checkbox `(item)`

Return **`item`** node with its checkbox state flipped.
This only affects item nodes with checkboxes in the `on` or `off`
states; return **`item`** node unchanged if the checkbox property is `trans`
or nil.

```el
;; Given the following contents:
; - [ ] one

(->> (org-ml-parse-this-item)
  (org-ml-item-toggle-checkbox)
  (org-ml-to-trimmed-string))
 ;; => "- [X] one"

;; Given the following contents:
; - [-] one

;; Ignore trans state checkboxes
(->> (org-ml-parse-this-item)
  (org-ml-item-toggle-checkbox)
  (org-ml-to-trimmed-string))
 ;; => "- [-] one"

;; Given the following contents:
; - one

;; Do nothing if there is no checkbox
(->> (org-ml-parse-this-item)
  (org-ml-item-toggle-checkbox)
  (org-ml-to-trimmed-string))
 ;; => "- one"

```


### Planning

#### org-ml-planning-set-timestamp! `(prop planning-list planning)`

Return **`planning`** node with **`prop`** set to **`planning-list`**.

**`prop`** is one of `:closed`, `:deadline`, or `:scheduled`. **`planning-list`**
is the same as that described in [`org-ml-build-planning!`](#org-ml-build-planning-key-closed-deadline-scheduled-post-blank).

```el
;; Given the following contents:
; * dummy
; CLOSED: [2019-01-01 Tue]

;; Change an existing timestamp in planning
(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-planning)
  (org-ml-planning-set-timestamp! :closed '(2019 1 2 &warning all 1 day &repeater cumulate 2 month))
  (org-ml-to-trimmed-string))
 ;; => "CLOSED: [2019-01-02 Wed +2m -1d]"

;; Add a new timestamp and remove another
(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-planning)
  (org-ml-planning-set-timestamp! :deadline '(2112 1 1))
  (org-ml-planning-set-timestamp! :closed nil)
  (org-ml-to-trimmed-string))
 ;; => "DEADLINE: <2112-01-01 Fri>"

```


### Statistics Cookie

#### org-ml-statistics-cookie-is-complete `(statistics-cookie)`

Return t is **`statistics-cookie`** node is complete.

```el
;; Given the following contents:
; * statistically significant [10/10]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-statistics-cookie)
  (org-ml-statistics-cookie-is-complete))
 ;; => t

;; Given the following contents:
; * statistically significant [1/10]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-statistics-cookie)
  (org-ml-statistics-cookie-is-complete))
 ;; => nil

;; Given the following contents:
; * statistically significant [100%]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-statistics-cookie)
  (org-ml-statistics-cookie-is-complete))
 ;; => t

;; Given the following contents:
; * statistically significant [33%]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-statistics-cookie)
  (org-ml-statistics-cookie-is-complete))
 ;; => nil

```


### Timestamp (Auxiliary)


Functions to work with timestamp data

#### org-ml-time-is-long `(time)`

Return t if **`time`** is a long format time list.

```el
no examples :(
```

#### org-ml-time-to-unixtime `(time)`

Return the unix time (integer seconds) of time list **`time`**.
The returned value is dependent on the time zone of the operating
system.

```el
no examples :(
```

#### org-ml-unixtime-to-time-long `(unixtime)`

Return the long time list of **`unixtime`**.
The list will be formatted like `(year month day hour min)`.

```el
no examples :(
```

#### org-ml-unixtime-to-time-short `(unixtime)`

Return the short time list of **`unixtime`**.
The list will be formatted like `(year month day nil nil)`.

```el
no examples :(
```


### Timestamp (Standard)

#### org-ml-timestamp-get-start-time `(timestamp)`

Return the time list for start time of **`timestamp`** node.
The return value will be a list as specified by the `time` argument in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-start-time))
 ;; => '(2019 1 1 nil nil)

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-start-time))
 ;; => '(2019 1 1 nil nil)

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-start-time))
 ;; => '(2019 1 1 0 0)

```

#### org-ml-timestamp-get-end-time `(timestamp)`

Return the end time list for end time of **`timestamp`** or nil if not a range.
The return value will be a list as specified by the `time` argument in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-end-time))
 ;; => nil

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-end-time))
 ;; => '(2019 1 2 nil nil)

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-end-time))
 ;; => '(2019 1 1 nil nil)

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-end-time))
 ;; => '(2019 1 1 12 0)

```

#### org-ml-timestamp-get-range `(timestamp)`

Return the range of **`timestamp`** node in seconds as an integer.
If non-ranged, this function will return 0. If ranged but
the start time is in the future relative to end the time, return
a negative integer.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-range))
 ;; => 0

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-range))
 ;; => 86400

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-range))
 ;; => 43200

```

#### org-ml-timestamp-is-active `(timestamp)`

Return t if **`timestamp`** node is active.

```el
;; Given the following contents:
; <2019-01-01 Tue>

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-is-active))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-is-active))
 ;; => nil

```

#### org-ml-timestamp-is-ranged `(timestamp)`

Return t if **`timestamp`** node is ranged.

```el
;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-is-ranged))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-is-ranged))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-is-ranged))
 ;; => nil

```

#### org-ml-timestamp-range-contains-p `(unixtime timestamp)`

Return t if **`unixtime`** is between start and end time of **`timestamp`** node.
The boundaries are inclusive. If **`timestamp`** has a range of zero, then
only return t if **`unixtime`** is the same as **`timestamp`**. **`timestamp`** will be
interpreted according to the localtime of the operating system.

```el
;; Given the following contents:
; [2019-01-01 Tue 00:00]

(let ((ut (org-ml-time-to-unixtime '(2019 1 1 0 0))))
  (->> (org-ml-parse-this-object)
    (org-ml-timestamp-range-contains-p ut)))
 ;; => t

(let ((ut (org-ml-time-to-unixtime '(2019 1 1 0 30))))
  (->> (org-ml-parse-this-object)
    (org-ml-timestamp-range-contains-p ut)))
 ;; => nil

;; Given the following contents:
; [2019-01-01 Tue 00:00-01:00]

(let ((ut (org-ml-time-to-unixtime '(2019 1 1 0 30))))
  (->> (org-ml-parse-this-object)
    (org-ml-timestamp-range-contains-p ut)))
 ;; => t

```

#### org-ml-timestamp-set-collapsed `(flag timestamp)`

Return **`timestamp`** with collapsed set to **`flag`**.

If timestamp is ranged but not outside of one day, it may be collapsed
(**`flag`** is t) to short format like [yyyy-mm-dd xxx hh:mm-hh:mm] or
expanded (**`flag`** is nil) to long format like [yyyy-mm-dd xxx
hh:mm]--[yyyy-mm-dd xxx hh:mm]. If these conditions are not met,
return **`timestamp`** untouched regardless of **`flag`**.

Note: the default for all timestamp functions in `om.el` is to favor
collapsed format.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00-13:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-collapsed nil)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00]"

;; Given the following contents:
; [2019-01-01 Tue 12:00-13:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-collapsed nil)
  (org-ml-timestamp-set-collapsed t)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00-13:00]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-collapsed nil)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-collapsed nil)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```

#### org-ml-timestamp-get-warning `(timestamp)`

Return the warning component of **`timestamp`**.
Return a list like `(type value unit)`.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-warning))
 ;; => '(nil nil nil)

;; Given the following contents:
; [2019-01-01 Tue 12:00 -1d]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-warning))
 ;; => '(all 1 day)

```

#### org-ml-timestamp-set-warning `(warning timestamp)`

Set the warning of **`timestamp`** to **`warning`**.

**`warning`** is a list like `(type value unit)`. `type` is one of 'year',
'month', 'week', or 'day'. `value` and is an integer. `unit` is one
of 'year', 'month', 'week', or 'day'.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-warning '(nil nil nil))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00]"

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-warning '(all 1 day))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00 -1d]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-warning nil)
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00]"

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-warning '(all 1 year))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00 -1y]"

```

#### org-ml-timestamp-map-warning `(fun timestamp)`

Apply **`fun`** to the warning of **`timestamp`**.
**`fun`** is a function that takes a warning list like and returns a
new warning list. The same rules that apply to
[`org-ml-timestamp-set-warning`](#org-ml-timestamp-set-warning-warning-timestamp) and [`org-ml-timestamp-get-warning`](#org-ml-timestamp-get-warning-timestamp)
apply here.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00 -1d]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-map-warning* (-let (((y v u)
					 it))
				   `(,y ,(1+ v)
					 ,u)))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00 -2d]"

```

#### org-ml-timestamp-get-repeater `(timestamp)`

Return the repeater component of **`timestamp`**.
Return a list like `(type value unit)`. If `org-ml-parse-habits` is
t, return a list like `(type value unit habit-value habit-unit)`.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-repeater))
 ;; => '(nil nil nil)

;; Given the following contents:
; [2019-01-01 Tue 12:00 +1d]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-repeater))
 ;; => '(cumulate 1 day)

;; Given the following contents:
; [2019-01-01 Tue 12:00 +1d/3d]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-get-repeater))
 ;; => '(cumulate 1 day)

;; Given the following contents:
; [2019-01-01 Tue 12:00 +1d/3d]

(let ((org-ml-parse-habits t))
  (->> (org-ml-parse-this-object)
    (org-ml-timestamp-get-repeater)))
 ;; => '(cumulate 1 day 3 day)

```

#### org-ml-timestamp-set-repeater `(repeater timestamp)`

Set the repeater of **`timestamp`** to **`repeater`**.

**`repeater`** is a list like `(type value unit)` or `(type value unit
habit-value habit-unit)`; if `org-ml-parse-habits` is nil, only
accept the former and error on the latter (and vice versa). `type`
is one of 'year', 'month', 'week', or 'day'. `value` and
`habit-value` are integers. `unit` and `habit-unit` are one of 'year',
'month', 'week', or 'day'.

In order to remove the repeater entirely, set **`repeater`** to nil
or `(nil nil nil)`. To delete just the habit (if it exists and
`org-ml-parse-habits` is t) set **`repeater`** to (`type` `value` `unit` nil
nil).

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-repeater nil)
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00]"

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-repeater '(restart 1 day))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00 .+1d]"

(let ((org-ml-parse-habits t))
  (->> (org-ml-parse-this-object)
    (org-ml-timestamp-set-repeater '(restart 1 day nil nil))
    (org-ml-to-string)))
 ;; => "[2019-01-01 Tue 12:00 .+1d]"

(let ((org-ml-parse-habits t))
  (->> (org-ml-parse-this-object)
    (org-ml-timestamp-set-repeater '(restart 1 day 3 day))
    (org-ml-to-string)))
 ;; => "[2019-01-01 Tue 12:00 .+1d/3d]"

```

#### org-ml-timestamp-map-repeater `(fun timestamp)`

Apply **`fun`** to the warning of **`timestamp`**.
**`fun`** is a function that takes a repeater list like and returns a
new repeater list. The same rules that apply to
[`org-ml-timestamp-set-repeater`](#org-ml-timestamp-set-repeater-repeater-timestamp) and
[`org-ml-timestamp-get-repeater`](#org-ml-timestamp-get-repeater-timestamp) apply here.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00 +1d]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-map-repeater* (-let (((y v u)
					  it))
				    `(,y ,(1+ v)
					  ,u)))
  (org-ml-to-string))
 ;; => "[2019-01-01 Tue 12:00 +2d]"

```

#### org-ml-timestamp-set-start-time `(time timestamp)`

Return **`timestamp`** node with start time set to **`time`**.
**`time`** is a list analogous to the same argument specified in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-02 Wed]

;; If not a range this will turn into a range by moving only the start time.
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-start-time '(2019 1 1))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Set a different time with different precision.
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-start-time '(2019 1 1 10 0))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 10:00]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-02 Wed 12:00]

;; If not a range and set within a day, use short format
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-start-time '(2019 1 1 0 0))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 00:00-12:00]"

```

#### org-ml-timestamp-set-end-time `(time timestamp)`

Return **`timestamp`** node with end time set to **`time`**.
**`time`** is a list analogous to the same argument specified in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Add the end time
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-end-time '(2019 1 2))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Remove the end time
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-end-time nil)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; Use short range format
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-end-time '(2019 1 1 13 0))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00-13:00]"

```

#### org-ml-timestamp-set-single-time `(time timestamp)`

Return **`timestamp`** node with start and end times set to **`time`**.
**`time`** is a list analogous to the same argument specified in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Don't make a range
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-single-time '(2019 1 2))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Output is not a range despite input being ranged
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-single-time '(2019 1 3))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-03 Thu]"

```

#### org-ml-timestamp-set-double-time `(time1 time2 timestamp)`

Return **`timestamp`** node with start/end times set to **`time1`**/**`time2`** respectively.
**`time1`** and **`time2`** are lists analogous to the `time` argument specified in
[`org-ml-build-timestamp!`](#org-ml-build-timestamp-start-key-end-active-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Make a range
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-double-time '(2019 1 2)
				     '(2019 1 3))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-02 Wed]--[2019-01-03 Thu]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-double-time '(2019 1 4)
				     '(2019 1 5))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-04 Fri]--[2019-01-05 Sat]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-double-time '(2019 1 1 0 0)
				     '(2019 1 1 1 0))
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 00:00-01:00]"

```

#### org-ml-timestamp-set-range `(range timestamp)`

Return **`timestamp`** node with range set to **`range`**.
If **`timestamp`** is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for **`range`** are in minutes
if **`timestamp`** is in long format and days if **`timestamp`** is in short
format.

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Use days as the unit for short format
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-range 1)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue 00:00]

;; Use minutes as the unit for long format
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-range 3)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 00:00-00:03]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Wed]

;; Set range to 0 to remove end time
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-range 0)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### org-ml-timestamp-set-active `(flag timestamp)`

Return **`timestamp`** node with active type if **`flag`** is t.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-active t)
  (org-ml-to-trimmed-string))
 ;; => "<2019-01-01 Tue>"

;; Given the following contents:
; <2019-01-01 Tue>

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-set-active nil)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### org-ml-timestamp-shift `(n unit timestamp)`

Return **`timestamp`** node with time shifted by **`n`** **`unit`**`s.

This function will move the start and end times together; therefore
ranged inputs will always output ranged timestamps and same for
non-ranged. To move the start and end time independently, use
[`org-ml-timestamp-shift-start`](#org-ml-timestamp-shift-start-n-unit-timestamp) or [`org-ml-timestamp-shift-end`](#org-ml-timestamp-shift-end-n-unit-timestamp).

**`n`** is a positive or negative integer and **`unit`** is one of `minute`,
`hour`, `day`, `month`, or `year`. Overflows will wrap around
transparently; for instance, supplying `minute` for **`unit`** and 90 for **`n`**
will increase the hour property by 1 and the minute property by 30.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; Change each unit, and wrap around to the next unit as needed.
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift 30 'minute)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:30]"

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift 13 'month)
  (org-ml-to-trimmed-string))
 ;; => "[2020-02-01 Sat 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]

;; Error when shifting hour/minute in short format
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift 30 'minute)
  (org-ml-to-trimmed-string))
Error

```

#### org-ml-timestamp-shift-start `(n unit timestamp)`

Return **`timestamp`** node with start time shifted by **`n`** **`unit`**`s.

**`n`** and **`unit`** behave the same as those in [`org-ml-timestamp-shift`](#org-ml-timestamp-shift-n-unit-timestamp).

If **`timestamp`** is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of **`timestamp`**. If this
behavior is not desired, use [`org-ml-timestamp-shift`](#org-ml-timestamp-shift-n-unit-timestamp).

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; If not a range, change start time and leave implicit end time.
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift-start -1 'year)
  (org-ml-to-trimmed-string))
 ;; => "[2018-01-01 Mon 12:00]--[2019-01-01 Tue 12:00]"

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift-start -1 'hour)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 11:00-12:00]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Thu]

;; Change only start time if a range
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift-start 1 'day)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-02 Wed]--[2019-01-03 Thu]"

```

#### org-ml-timestamp-shift-end `(n unit timestamp)`

Return **`timestamp`** node with end time shifted by **`n`** **`unit`**`s.

**`n`** and **`unit`** behave the same as those in [`org-ml-timestamp-shift`](#org-ml-timestamp-shift-n-unit-timestamp).

If **`timestamp`** is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of **`timestamp`**. If this
behavior is not desired, use [`org-ml-timestamp-shift`](#org-ml-timestamp-shift-n-unit-timestamp).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Shift implicit end time if not a range.
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift-end 1 'day)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Move only the second time if a range.
(->> (org-ml-parse-this-object)
  (org-ml-timestamp-shift-end 1 'day)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-03 Thu]"

```

#### org-ml-timestamp-toggle-active `(timestamp)`

Return **`timestamp`** node with its active/inactive type flipped.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-toggle-active)
  (org-ml-to-trimmed-string))
 ;; => "<2019-01-01 Tue>"

;; Given the following contents:
; <2019-01-01 Tue>--<2019-01-02 Wed>

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-toggle-active)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```

#### org-ml-timestamp-truncate `(timestamp)`

Return **`timestamp`** node with start/end times forced to short format.

```el
;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]--[2019-01-02 Wed 13:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```

#### org-ml-timestamp-truncate-start `(timestamp)`

Return **`timestamp`** node with start time forced to short format.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate-start)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]--[2019-01-02 Wed 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate-start)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate-start)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### org-ml-timestamp-truncate-end `(timestamp)`

Return **`timestamp`** node with end time forced to short format.

```el
;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate-end)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]--[2019-01-02 Wed 13:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate-end)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue 12:00]

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-truncate-end)
  (org-ml-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]"

```


### Timestamp (diary)

#### org-ml-timestamp-diary-set-value `(form timestamp-diary)`

Return **`timestamp-diary`** node with value set to **`form`**.
The node must have a type `eq` to `diary`. **`form`** is a quoted list.

```el
;; Given the following contents:
; <%%(diary-float t 4 2)>

(->> (org-ml-parse-this-object)
  (org-ml-timestamp-diary-set-value '(diary-float 1 3 2))
  (org-ml-to-string))
 ;; => "<%%(diary-float 1 3 2)>"

```


### Affiliated Keywords

#### org-ml-get-affiliated-keyword `(key node)`

Get the value of affiliated keyword **`key`** in **`node`**.

See [`org-ml-set-affiliated-keyword`](#org-ml-set-affiliated-keyword-key-value-node) for the meaning of **`key`**.

`warning`: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
[`org-ml-get-property`](#org-ml-get-property-prop-node).

```el
;; Given the following contents:
; #+name: name
; #+attr_foo: bar
; #+attr_foo: BAR
; #+plot: poo
; #+results[hash]: res
; #+header: h1
; #+begin_src
; echo test for echo
; #+end_src

;; Simply return NAME and PLOT
(->> (org-ml-parse-this-element)
  (org-ml-get-affiliated-keyword :name))
 ;; => "name"

(->> (org-ml-parse-this-element)
  (org-ml-get-affiliated-keyword :plot))
 ;; => "poo"

;; Attribute FOO has multiple entries so return a list of all
(->> (org-ml-parse-this-element)
  (org-ml-get-affiliated-keyword :attr_foo))
 ;; => '("BAR" "bar")

;; HEADER may have multiple values so return a singleton list
(->> (org-ml-parse-this-element)
  (org-ml-get-affiliated-keyword :header))
 ;; => '("h1")

;; RESULTS returns a cons cell with the optional part
(->> (org-ml-parse-this-element)
  (org-ml-get-affiliated-keyword :results))
 ;; => '("res" . "hash")

```

#### org-ml-set-affiliated-keyword `(key value node)`

Set affiliated keyword **`key`** in **`node`** to **`value`**.
This is just like `org-ml--set-property-nocheck` except it will
delete **`key`** from the plist if **`value`** is nil.

`note` that **`value`** should reflect the required value of affiliated
keyword given by **`key`**. The format for each keyword is given below:
- `name` ``string``: ``string``
- `plot` ``string``: ``string``
- `results`[``string1``] ``string2``: `(string2 . string1)`
    where ``string1`` may be nil
- `caption`[``string1``] ``string2``: `((string2 . string1) ...)`
    where ``string1`` may be nil and multiple list members
    correspond to multiple caption entries
- `headers` ``string``: `(string ...)` where multiple list members
    correspond to multiple headers entries
- `caption`[``string``] ``secstring``: `((string . secstring) ...)`
    where ``string`` may be nil and multiple list members
    correspond to multiple caption entries

In the case of `attr`_`backend`, **`key`** is like `:attr_x` where `x`
corresponds to `backend` and **`value`** is a list of strings
corresponding to multiple entries of the attribute.

`warning`: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
[`org-ml-set-property`](#org-ml-set-property-prop-value-node)

```el
;; Given the following contents:
; short paragraph

(->> (org-ml-parse-this-element)
  (org-ml-set-affiliated-keyword :name "foo")
  (org-ml-to-trimmed-string))
 ;; => "#+name: foo
 ;      short paragraph"

(->> (org-ml-parse-this-element)
  (org-ml-set-affiliated-keyword :attr_bar '("foo"))
  (org-ml-to-trimmed-string))
 ;; => "#+attr_bar: foo
 ;      short paragraph"

(->> (org-ml-parse-this-element)
  (org-ml-set-affiliated-keyword :header '("h1" "h2"))
  (org-ml-to-trimmed-string))
 ;; => "#+header: h2
 ;      #+header: h1
 ;      short paragraph"

(->> (org-ml-parse-this-element)
  (org-ml-set-affiliated-keyword :results '("foo" . "bar"))
  (org-ml-to-trimmed-string))
 ;; => "#+results[bar]: foo
 ;      short paragraph"

;; Given the following contents:
; #+name: deleteme
; short paragraph

(->> (org-ml-parse-this-element)
  (org-ml-set-affiliated-keyword :name nil)
  (org-ml-to-trimmed-string))
 ;; => "short paragraph"

```

#### org-ml-map-affiliated-keyword `(key fun node)`

Apply **`fun`** to value of affiliated keyword **`key`** in **`node`**.

See [`org-ml-set-affiliated-keyword`](#org-ml-set-affiliated-keyword-key-value-node) for the meaning of **`key`**.

`warning`: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
[`org-ml-map-property`](#org-ml-map-property-prop-fun-node).

```el
;; Given the following contents:
; #+name: foo
; short paragraph

(->> (org-ml-parse-this-element)
  (org-ml-map-affiliated-keyword :name #'upcase)
  (org-ml-to-trimmed-string))
 ;; => "#+name: FOO
 ;      short paragraph"

;; Given the following contents:
; #+header: foo
; short paragraph

(->> (org-ml-parse-this-element)
  (org-ml-map-affiliated-keyword* :header (cons "bar" it))
  (org-ml-to-trimmed-string))
 ;; => "#+header: foo
 ;      #+header: bar
 ;      short paragraph"

```

#### org-ml-set-caption! `(caption node)`

Set the caption affiliated keyword of **`node`**.

**`caption`** can be one of the following:
- `string`: produces #+**`caption`**: ``string``
- `(string1 string2)`: produces #+**`caption`**[``string2``]: ``string1``
- `((string1 string2) ...)`: like above but makes multiple
    caption entries
- nil: removes all captions

`warning`: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
[`org-ml-set-property`](#org-ml-set-property-prop-value-node).

```el
;; Given the following contents:
; short paragraph

(->> (org-ml-parse-this-element)
  (org-ml-set-caption! "cap")
  (org-ml-to-trimmed-string))
 ;; => "#+caption: cap
 ;      short paragraph"

(->> (org-ml-parse-this-element)
  (org-ml-set-caption! '("foo" "cap"))
  (org-ml-to-trimmed-string))
 ;; => "#+caption[foo]: cap
 ;      short paragraph"

(->> (org-ml-parse-this-element)
  (org-ml-set-caption! '("foo" "cap"))
  (org-ml-to-trimmed-string))
 ;; => "#+caption[foo]: cap
 ;      short paragraph"

(->> (org-ml-parse-this-element)
  (org-ml-set-caption! '(("foo" "cap")
			 ("FOO" "CAP")))
  (org-ml-to-trimmed-string))
 ;; => "#+caption[FOO]: CAP
 ;      #+caption[foo]: cap
 ;      short paragraph"

;; Given the following contents:
; #+caption: cap
; short paragraph

(->> (org-ml-parse-this-element)
  (org-ml-set-caption! nil)
  (org-ml-to-trimmed-string))
 ;; => "short paragraph"

```


## Branch/Child Manipulation


Set, get, and map the children of branch nodes.


### Polymorphic

#### org-ml-children-contain-point `(point branch-node)`

Return t if **`point`** is within the boundaries of **`branch-node`**`s children.

```el
;; Given the following contents:
; * headline
; findme

(->> (org-ml-parse-this-headline)
  (org-ml-children-contain-point 2))
 ;; => nil

(->> (org-ml-parse-this-headline)
  (org-ml-children-contain-point 15))
 ;; => t

```

#### org-ml-get-children `(branch-node)`

Return the children of **`branch-node`** as a list.

```el
;; Given the following contents:
; /this/ is a *paragraph*

;; Return child nodes for branch nodes
(->> (org-ml-parse-this-element)
  (org-ml-get-children)
  (-map #'org-ml-get-type))
 ;; => '(italic plain-text bold)

;; Given the following contents:
; * headline

;; Return nil if no children
(->> (org-ml-parse-this-subtree)
  (org-ml-get-children)
  (-map #'org-ml-get-type))
 ;; => nil

```

#### org-ml-set-children `(children branch-node)`

Return **`branch-node`** with its children set to **`children`**.
**`children`** is a list of nodes; the types permitted in this list depend
on the type of `node`.

```el
;; Given the following contents:
; /this/ is a *paragraph*

;; Set children for branch object
(->> (org-ml-parse-this-element)
  (org-ml-set-children (list "this is lame"))
  (org-ml-to-trimmed-string))
 ;; => "this is lame"

;; Given the following contents:
; * headline

;; Set children for branch element nodes
(->> (org-ml-parse-this-subtree)
  (org-ml-set-children (list (org-ml-build-headline! :title-text "only me" :level 2)))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      ** only me"

```

#### org-ml-map-children `(fun branch-node)`

Return **`branch-node`** with **`fun`** applied to its children.
**`fun`** is a unary function that takes the current list of children and
returns a modified list of children.

```el
;; Given the following contents:
; /this/ is a *paragraph*

(->> (org-ml-parse-this-element)
  (org-ml-map-children (lambda (objs)
			 (append objs (list " ...yeah"))))
  (org-ml-to-trimmed-string))
 ;; => "/this/ is a *paragraph* ...yeah"

;; Given the following contents:
; * headline
; ** subheadline

(->> (org-ml-parse-this-subtree)
  (org-ml-map-children* (--map (org-ml-shift-property :level 1 it)
			       it))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      *** subheadline"

```

#### org-ml-is-childless `(branch-node)`

Return t if **`branch-node`** has no children.

```el
;; Given the following contents:
; * dummy
; filled with useless knowledge

(->> (org-ml-parse-this-headline)
  (org-ml-is-childless))
 ;; => nil

;; Given the following contents:
; * dummy

(->> (org-ml-parse-this-headline)
  (org-ml-is-childless))
 ;; => t

```


### Object Nodes

#### org-ml-unwrap `(object-node)`

Return the children of **`object-node`** as a secondary string.
If **`object-node`** is a plain-text node, wrap it in a list and return.
Else add the post-blank property of **`object-node`** to the last member
of its children and return children as a secondary string.

```el
;; Given the following contents:
; _1 *2* 3 */4/* 5 /6/_

;; Remove the outer underline formatting
(->> (org-ml-parse-this-object)
  (org-ml-unwrap)
  (apply #'org-ml-build-paragraph)
  (org-ml-to-trimmed-string))
 ;; => "1 *2* 3 */4/* 5 /6/"

```

#### org-ml-unwrap-types-deep `(types object-node)`

Return the children of **`object-node`** as a secondary string.
If **`object-node`** is a plain-text node, wrap it in a list and return.
Else recursively descend into the children of **`object-node`** and splice
the children of nodes with type in **`types`** in place of said node and
return the result as a secondary string.

```el
;; Given the following contents:
; _1 *2* 3 */4/* 5 /6/_

;; Remove bold formatting at any level
(->> (org-ml-parse-this-object)
  (org-ml-unwrap-types-deep '(bold))
  (apply #'org-ml-build-paragraph)
  (org-ml-to-trimmed-string))
 ;; => "_1 2 3 /4/ 5 /6/_"

```

#### org-ml-unwrap-deep `(object-node)`

Return the children of **`object-node`** as plain-text wrapped in a list.

```el
;; Given the following contents:
; _1 *2* 3 */4/* 5 /6/_

;; Remove all formatting
(->> (org-ml-parse-this-object)
  (org-ml-unwrap-deep)
  (apply #'org-ml-build-paragraph)
  (org-ml-to-trimmed-string))
 ;; => "1 2 3 4 5 6"

```


### Secondary Strings

#### org-ml-flatten `(secondary-string)`

Return **`secondary-string`** with its first level unwrapped.
The unwrap operation will be done with [`org-ml-unwrap`](#org-ml-unwrap-object-node).

```el
;; Given the following contents:
; This (1 *2* 3 */4/* 5 /6/) is randomly formatted

;; Remove first level of formatting
(->> (org-ml-parse-this-element)
  (org-ml-map-children #'org-ml-flatten)
  (org-ml-to-trimmed-string))
 ;; => "This (1 2 3 /4/ 5 6) is randomly formatted"

```

#### org-ml-flatten-types-deep `(types secondary-string)`

Return **`secondary-string`** with object nodes in **`types`** unwrapped.
The unwrap operation will be done with [`org-ml-unwrap-types-deep`](#org-ml-unwrap-types-deep-types-object-node).

```el
;; Given the following contents:
; This (1 *2* 3 */4/* 5 /6/) is randomly formatted

;; Remove italic formatting at any level
(->> (org-ml-parse-this-element)
  (org-ml-map-children* (org-ml-flatten-types-deep '(italic)
						    it))
  (org-ml-to-trimmed-string))
 ;; => "This (1 *2* 3 *4* 5 6) is randomly formatted"

```

#### org-ml-flatten-deep `(secondary-string)`

Return **`secondary-string`** with all object nodes unwrapped to plain-text.
The unwrap operation will be done with [`org-ml-unwrap-deep`](#org-ml-unwrap-deep-object-node).

```el
;; Given the following contents:
; This (1 *2* 3 */4/* 5 /6/) is randomly formatted

;; Remove italic formatting at any level
(->> (org-ml-parse-this-element)
  (org-ml-map-children #'org-ml-flatten-deep)
  (org-ml-to-trimmed-string))
 ;; => "This (1 2 3 4 5 6) is randomly formatted"

```


### Item

#### org-ml-item-get-paragraph `(item)`

Return the first paragraph's children of **`item`** or nil if none.

```el
;; Given the following contents:
; - one

(->> (org-ml-parse-this-item)
  (org-ml-item-get-paragraph))
 ;; => '("one")

;; Given the following contents:
; - 

(->> (org-ml-parse-this-item)
  (org-ml-item-get-paragraph))
 ;; => nil

```

#### org-ml-item-set-paragraph `(secondary-string item)`

Set the first paragraph's children of **`item`** to **`secondary-string`**.

```el
;; Given the following contents:
; - one

(->> (org-ml-parse-this-item)
  (org-ml-item-set-paragraph '("two"))
  (org-ml-to-string))
 ;; => "- two
 ;      "

;; Given the following contents:
; - one

(->> (org-ml-parse-this-item)
  (org-ml-item-set-paragraph nil)
  (org-ml-to-string))
 ;; => "- 
 ;      "

```

#### org-ml-item-map-paragraph `(fun item)`

Apply **`fun`** to the first paragraph's children in **`item`**.
**`fun`** is a `unary` function that takes the secondary-string of the
first paragraph and returns modified secondary-string.

```el
;; Given the following contents:
; - one

(->> (org-ml-parse-this-item)
  (org-ml-item-map-paragraph* (-map #'upcase it))
  (org-ml-to-string))
 ;; => "- ONE
 ;      "

```


### Headline

#### org-ml-headline-get-section `(headline)`

Return children of section node in **`headline`** node or nil if none.

```el
;; Given the following contents:
; * headline 1
; sectional stuff
; ** headline 2
; ** headline 3

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-section)
  (-map #'org-ml-to-trimmed-string))
 ;; => '("sectional stuff")

;; Given the following contents:
; * headline 1
; ** headline 2
; ** headline 3

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-section)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-headline-set-section `(children headline)`

Return **`headline`** with section node containing **`children`**.
If **`children`** is nil, return **`headline`** with no section node.

```el
;; Given the following contents:
; * headline

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-set-section (list (org-ml-build-paragraph! "x-section")))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      x-section"

;; Given the following contents:
; * headline
; x-section

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-set-section (list (org-ml-build-paragraph! "x-guard")))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      x-guard"

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-set-section nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

```

#### org-ml-headline-map-section `(fun headline)`

Return **`headline`** node with child section node modified by **`fun`**.

**`fun`** is a unary function that takes a section node's children as a list
returns a modified child list.

```el
;; Given the following contents:
; * headline
; x-section

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-map-section* (cons (org-ml-build-planning! :closed '(2019 1 1))
				      it))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue]
 ;      x-section"

```

#### org-ml-headline-get-subheadlines `(headline)`

Return list of child headline nodes in **`headline`** node or nil if none.

```el
;; Given the following contents:
; * headline 1
; sectional stuff
; ** headline 2
; ** headline 3

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-subheadlines)
  (-map #'org-ml-to-trimmed-string))
 ;; => '("** headline 2" "** headline 3")

;; Given the following contents:
; * headline 1
; sectional stuff

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-subheadlines)
  (-map #'org-ml-to-trimmed-string))
 ;; => nil

```

#### org-ml-headline-set-subheadlines `(subheadlines headline)`

Return **`headline`** node with **`subheadlines`** set to child subheadlines.

```el
;; Given the following contents:
; * headline 1
; sectional stuff
; ** headline 2
; ** headline 3

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-set-subheadlines (list (org-ml-build-headline! :level 2 :title-text "headline x")))
  (org-ml-to-trimmed-string))
 ;; => "* headline 1
 ;      sectional stuff
 ;      ** headline x"

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-set-subheadlines nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline 1
 ;      sectional stuff"

```

#### org-ml-headline-map-subheadlines `(fun headline)`

Return **`headline`** node with child headline nodes modified by **`fun`**.

**`fun`** is a unary function that takes a list of headlines and returns
a modified list of headlines.

```el
;; Given the following contents:
; * headline 1
; ** headline 2
; ** headline 3

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-map-subheadlines* (--map (org-ml-set-property :todo-keyword "TODO" it)
					    it))
  (org-ml-to-trimmed-string))
 ;; => "* headline 1
 ;      ** TODO headline 2
 ;      ** TODO headline 3"

```


### Headline (metadata)

#### org-ml-headline-get-planning `(headline)`

Return the planning node in **`headline`** or nil if none.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-planning)
  (org-ml-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-planning)
  (org-ml-to-trimmed-string))
 ;; => ""

```

#### org-ml-headline-set-planning `(planning headline)`

Return **`headline`** node with planning components set to **`planning`** node.

```el
;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-planning (org-ml-build-planning! :closed '(2019 1 1)))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue]"

;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-planning (org-ml-build-planning! :scheduled '(2019 1 1)))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      SCHEDULED: <2019-01-01 Tue>"

;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-planning nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

```

#### org-ml-headline-map-planning `(fun headline)`

Return **`headline`** node with planning node modified by **`fun`**.

**`fun`** is a unary function that takes a planning node and returns a
modified planning node.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-map-planning* (org-ml-map-property* :closed (org-ml-timestamp-shift 1 'day it)
							it))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      CLOSED: [2019-01-02 Wed]"

```

#### org-ml-headline-get-node-properties `(headline)`

Return a list of node-properties nodes in **`headline`** or nil if none.

```el
;; Given the following contents:
; * headline
; :PROPERTIES:
; :Effort:   1:00
; :ID:       minesfake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-node-properties)
  (-map #'org-ml-to-trimmed-string))
 ;; => '(":Effort:   1:00" ":ID:       minesfake")

;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-node-properties)
  (-map #'org-ml-to-trimmed-string))
 ;; => nil

```

#### org-ml-headline-set-node-properties `(node-properties headline)`

Return **`headline`** node with property drawer containing **`node-properties`**.
**`node-properties`** is a list of node-property nodes.

```el
;; Given the following contents:
; * headline
; :PROPERTIES:
; :Effort:   1:00
; :ID:       minesfake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-node-properties (--map (apply #'org-ml-build-node-property it)
					      '(("Effort" "0:01")
						("ID" "easy"))))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :PROPERTIES:
 ;      :Effort:   0:01
 ;      :ID:       easy
 ;      :END:"

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-node-properties nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

```

#### org-ml-headline-map-node-properties `(fun headline)`

Return **`headline`** node with property-drawer node modified by **`fun`**.

**`fun`** is a unary function that takes a property-drawer node and returns
a modified property-drawer node.

```el
;; Given the following contents:
; * headline
; :PROPERTIES:
; :Effort:   1:00
; :ID:       minesfake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-map-node-properties* (cons (org-ml-build-node-property "New" "world man")
					      it))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :PROPERTIES:
 ;      :New:      world man
 ;      :Effort:   1:00
 ;      :ID:       minesfake
 ;      :END:"

```

#### org-ml-headline-get-node-property `(key headline)`

Return value of property with **`key`** in **`headline`** or nil if not found.
If multiple properties with **`key`** are present, only return the first.

```el
;; Given the following contents:
; * headline
; :PROPERTIES:
; :ID:       fake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-node-property "ID"))
 ;; => "fake"

;; Given the following contents:
; * headline
; :PROPERTIES:
; :ID:       fake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-node-property "READ_ID"))
 ;; => nil

```

#### org-ml-headline-set-node-property `(key value headline)`

Return **`headline`** with node property matching **`key`** set to **`value`**.
If a property matching **`key`** is present, set it to **`value`**. If multiple
properties matching **`key`** are present, only set the first.

```el
;; Given the following contents:
; * headline
; :PROPERTIES:
; :ID:       fake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-node-property "ID" "real")
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :PROPERTIES:
 ;      :ID:       real
 ;      :END:"

;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-node-property "ID" "real")
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :PROPERTIES:
 ;      :ID:       real
 ;      :END:"

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-node-property "ID" nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; :PROPERTIES:
; :ID:       real
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-node-property "ID" nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline"

```

#### org-ml-headline-map-node-property `(key fun headline)`

Return **`headline`** node with property value matching **`key`** modified by **`fun`**.

**`fun`** is a unary function that takes a node-property value and returns
a modified node-property value.

```el
;; Given the following contents:
; * headline
; :PROPERTIES:
; :ID:       fake
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-map-node-property "ID" #'s-upcase)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :PROPERTIES:
 ;      :ID:       FAKE
 ;      :END:"

```


### Headline (logbook and contents)

#### org-ml-headline-get-supercontents `(config headline)`

Return the supercontents of **`headline`** node.

Supercontents will be like `((:logbook lb) (:contents contents))`
where `lb` is another alist representing the logbook, and `contents`
is everything under the headline after the logbook and before the
first subheadline (if present).

The logbook will be have keys :items, :clocks, and :unknown,
where the first two will include the item and clock nodes of the
logbook respectively, and the third will contain anything that
could not be identified as a valid logbook entry. Note that items
are actually stored under a plain-list node but will be returned
here as a flat list of items for convenience. Also note that the
:clocks slot can also include item nodes if clock notes are
returned.

**`config`** is a plist representing the logbook configuration to
target and will contain the following keys;
- :log-into-drawer - corresponds to the value of
    symbol `org-log-into-drawer` and carriers the same meaning
- :clock-into-drawer - corresponds to the value of
    symbol `org-clock-into-drawer` and carriers the same meaning
- :clock-out-notes - corresponds to the value of
    `org-log-note-clock-out`

Any values not given will default to nil. Note that there is no
way to infer what the logbook configuration should be, and thus
this controls how the logbook will be parsed; this means it also
determines which nodes will be returned in the :items/:clocks
slots and which will be deemed :unknown (see above) so be sure
this plist is set according to your desired target configuration.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-get-supercontents config)
    (org-ml-supercontents-get-logbook)
    (org-ml-logbook-get-items)
    (-map #'org-ml-to-trimmed-string)))
 ;; => '("- Note taken on [2018-12-31 Mon 00:00] \\\\
 ;       log note")

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-get-supercontents config)
    (org-ml-supercontents-get-logbook)
    (org-ml-logbook-get-clocks)
    (-map #'org-ml-to-trimmed-string)))
 ;; => '("CLOCK: [2019-01-01 Tue 00:00]")

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-get-supercontents config)
    (org-ml-supercontents-get-logbook)
    (alist-get :unknown)
    (-map #'org-ml-to-trimmed-string)))
 ;; => nil

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-get-supercontents config)
    (org-ml-supercontents-get-contents)
    (-map #'org-ml-to-trimmed-string)))
 ;; => '("contents")

```

#### org-ml-headline-set-supercontents `(config supercontents headline)`

Set logbook and contents of **`headline`** according to **`supercontents`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of **`config`**
and the structure of the **`supercontents`** list.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort:    0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-set-supercontents config `((:logbook nil)
						(:contents ,(org-ml-build-paragraph! "new contents"))))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue 00:00]
 ;      :PROPERTIES:
 ;      :Effort:   0:30
 ;      :END:
 ;      new contents"

```

#### org-ml-headline-map-supercontents `(config fun headline)`

Map a function over the supercontents of **`headline`**.
**`fun`** is a unary function that takes a supercontents list and
returns a modified supercontents list. See
[`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of **`config`** and
the structure of the supercontents list.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort:    0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-map-supercontents* config (org-ml-supercontents-map-contents* (cons (org-ml-build-paragraph! "new contents")
											 it)
										   it))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue 00:00]
 ;      :PROPERTIES:
 ;      :Effort:   0:30
 ;      :END:
 ;      :LOGGING:
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        log note
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2019-01-01 Tue 00:00]
 ;      :END:
 ;      new contents
 ;      contents"

```

#### org-ml-headline-get-logbook-items `(config headline)`

Return the logbook items of **`headline`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. The returned items will be a flat list of item nodes,
not a plain-list node.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-get-logbook-items config)
    (-map #'org-ml-to-trimmed-string)))
 ;; => '("- Note taken on [2018-12-31 Mon 00:00] \\\\
 ;       log note")

```

#### org-ml-headline-set-logbook-items `(config items headline)`

Set the logbook items of **`headline`** to **`items`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. **`items`** must be supplied as a flat list of valid logbook
item nodes, not as a plain-list node.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-set-logbook-items config nil)
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue 00:00]
 ;      :PROPERTIES:
 ;      :Effort:   0:30
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2019-01-01 Tue 00:00]
 ;      :END:
 ;      contents"

```

#### org-ml-headline-map-logbook-items `(config fun headline)`

Map a function over the logbook items of **`headline`**.
**`fun`** is a unary function that takes a list of item nodes and
returns a modified list of item nodes. See
[`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of **`config`**.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-map-logbook-items* config (--map (org-ml-map-children* (--map (org-ml-map-children* (--map-when (org-ml-is-type 'plain-text it)
														     (upcase it)
														     it)
													 it)
										   it)
									    it)
						      it))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue 00:00]
 ;      :PROPERTIES:
 ;      :Effort:   0:30
 ;      :END:
 ;      :LOGGING:
 ;      - NOTE TAKEN ON [2018-12-31 Mon 00:00] \\\\
 ;        LOG NOTE
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2019-01-01 Tue 00:00]
 ;      :END:
 ;      contents"

```

#### org-ml-headline-get-logbook-clocks `(config headline)`

Return the logbook clocks of **`headline`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. The returned list will include clock nodes and maybe item
nodes if :clock-out-notes is t in **`config`**.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-get-logbook-clocks config)
    (-map #'org-ml-to-trimmed-string)))
 ;; => '("CLOCK: [2019-01-01 Tue 00:00]")

```

#### org-ml-headline-set-logbook-clocks `(config clocks headline)`

Set the logbook clocks of **`headline`** to **`clocks`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. **`clocks`** must be supplied as a flat list of valid clock
nodes and optionally item nodes if :clock-out-notes is t in
**`config`**.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-set-logbook-clocks config nil)
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue 00:00]
 ;      :PROPERTIES:
 ;      :Effort:   0:30
 ;      :END:
 ;      :LOGGING:
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        log note
 ;      :END:
 ;      contents"

```

#### org-ml-headline-map-logbook-clocks `(config fun headline)`

Map a function over the logbook clocks of **`headline`**.
**`fun`** is a unary function that takes a list of clock nodes and
optionally item nodes to represent the clock notes and returns a
modified list of said nodes. [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline)
for the meaning of **`config`**.

```el
;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :PROPERTIES:
; :Effort: 0:30
; :END:
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; contents

(let ((config (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-map-logbook-clocks* config (--map (org-ml-map-property* :value (org-ml-timestamp-shift 1 'day it)
									      it)
						       it))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      CLOSED: [2019-01-01 Tue 00:00]
 ;      :PROPERTIES:
 ;      :Effort:   0:30
 ;      :END:
 ;      :LOGGING:
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        log note
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2019-01-02 Wed 00:00]
 ;      :END:
 ;      contents"

```

#### org-ml-headline-get-contents `(config headline)`

Return the contents of **`headline`**.
Contents is everything in the headline after the logbook and will
be returned as a flat list of nodes. See
[`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of **`config`**.

```el
;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t))
  (-map #'org-ml-to-trimmed-string))
 ;; => nil

;; Given the following contents:
; * headline
; something

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("something")

;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :LOGBOOK:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; 
; - not log

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("- not log")

;; Given the following contents:
; * headline
; CLOSED: [2019-01-01 Tue 00:00]
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log note
; :END:
; :CLOCKING:
; CLOCK: [2019-01-01 Tue 00:00]
; :END:
; 
; - not log

(->> (org-ml-parse-this-headline)
  (org-ml-headline-get-contents (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING"))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("- not log")

```

#### org-ml-headline-set-contents `(config contents headline)`

Set the contents of **`headline`** to **`contents`**.
Contents is everything in the headline after the logbook, and
**`contents`** must be a flat list of nodes. See
[`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of **`config`**.

```el
;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
				(list (org-ml-build-paragraph! "I'm new")))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      I'm new"

;; Given the following contents:
; * headline
; something

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
				(list (org-ml-build-paragraph! "I'm new")))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      I'm new"

;; Given the following contents:
; * headline
; :LOGBOOK:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log1
; :END:
; something

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
				(list (org-ml-build-paragraph! "I'm new")))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        log1
 ;      :END:
 ;      I'm new"

;; Given the following contents:
; * headline
; :LOGBOOK:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   log1
; :END:
; something

(->> (org-ml-parse-this-headline)
  (org-ml-headline-set-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
				nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        log1
 ;      :END:"

```

#### org-ml-headline-map-contents `(config fun headline)`

Map a function over the contents of **`headline`**.
Contents is everything in the headline after the logbook. **`fun`** is
a unary function that takes a list of nodes representing the
contents and returns a modified list of nodes. See
[`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of **`config`**.

```el
;; Given the following contents:
; * headline
; something

(->> (org-ml-parse-this-headline)
  (org-ml-headline-map-contents* (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
    (cons (org-ml-build-paragraph! "I'm new")
	  it))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      I'm new
 ;      something"

```

#### org-ml-headline-logbook-append-item `(config item headline)`

Append **`item`** to the logbook of **`headline`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. **`item`** must be a valid logbook item. The logbook will be
started if it does not already exist, else **`item`** will be added in
chronological order.

```el
;; Given the following contents:
; * headline

(let ((ut (- 1546300800 (car (current-time-zone)))))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-logbook-append-item (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					 (org-ml-build-log-note ut "new note"))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      - Note taken on [2019-01-01 Tue 00:00] \\\\
 ;        new note
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGBOOK:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   old note
; :END:

(let ((ut (- 1546300800 (car (current-time-zone)))))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-logbook-append-item (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					 (org-ml-build-log-note ut "new note"))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      - Note taken on [2019-01-01 Tue 00:00] \\\\
 ;        new note
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        old note
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGGING:
; - Note taken on [2018-12-31 Mon 00:00] \\
;   old note
; :END:
; :CLOCKING:
; CLOCK: [2112-01-01 Fri]
; :END:

(let ((ut (- 1546300800 (car (current-time-zone)))))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-logbook-append-item (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")
					 (org-ml-build-log-note ut "new note"))
    (org-ml-to-trimmed-string)))
 ;; => "* headline
 ;      :LOGGING:
 ;      - Note taken on [2019-01-01 Tue 00:00] \\\\
 ;        new note
 ;      - Note taken on [2018-12-31 Mon 00:00] \\\\
 ;        old note
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2112-01-01 Fri]
 ;      :END:"

```

#### org-ml-headline-logbook-append-open-clock `(config unixtime headline)`

Append an open clock to the logbook of **`headline`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. **`unixtime`** will set the start time of the clock. The
logbook will be started if it does not already exist, else the
new clock will be added in chronological order.

```el
;; Given the following contents:
; * headline

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-append-open-clock (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					     (- 1546300800 (car (current-time-zone))))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      CLOCK: [2019-01-01 Tue 00:00]
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGBOOK:
; - note taken on [2018-12-30 Sun 00:00]
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-append-open-clock (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					     (- 1546300800 (car (current-time-zone))))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      CLOCK: [2019-01-01 Tue 00:00]
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGGING:
; - note taken on [2018-12-30 Sun 00:00]
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-append-open-clock (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING")
					     (- 1546300800 (car (current-time-zone))))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGGING:
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2019-01-01 Tue 00:00]
 ;      :END:"

```

#### org-ml-headline-logbook-close-open-clock `(config unixtime note headline)`

Close an open clock to the logbook of **`headline`**.
See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline) for the meaning of
**`config`**. **`unixtime`** will set the end time of the clock. This will
only close an open clock if it is the most recent clock; else it
will do nothing. **`note`** is a string representing the clock-out
note (or nil if not desired). Note that supplying a non-nil
clock-note when it is not allowed by **`config`** will trigger an
error.

```el
;; Given the following contents:
; * headline
; :LOGBOOK:
; - note taken on [2018-12-30 Sun 00:00]
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-close-open-clock (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					    (- 1546300800 (car (current-time-zone)))
					    nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGBOOK:
; CLOCK: [2018-12-31 Mon 00:00]
; - note taken on [2018-12-30 Sun 00:00]
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-close-open-clock (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					    (- 1546300800 (car (current-time-zone)))
					    nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:"

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-close-open-clock (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t)
					    (- 1546300800 (car (current-time-zone)))
					    "new note")
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
 ;      - new note
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGGING:
; - note taken on [2018-12-30 Sun 00:00]
; :END:
; :CLOCKING:
; CLOCK: [2018-12-31 Mon 00:00]
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-close-open-clock '(:log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING" :clock-out-notes t)
					     (- 1546300800 (car (current-time-zone)))
					     nil)
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGGING:
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
 ;      :END:"

```

#### org-ml-headline-logbook-convert-config `(config1 config2 headline)`

Convert the logbook of **`headline`** to a new configuration.
**`config1`** is the current config and **`config2`** is the target config.
Note that any logbook nodes that are invalid under **`config1`** will
be silently dropped, and nodes which do not conform to **`config2`**
will trigger an error. See [`org-ml-headline-get-supercontents`](#org-ml-headline-get-supercontents-config-headline)
for the structure of both config lists.

```el
;; Given the following contents:
; * headline
; CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
; - note taken on [2018-12-30 Sun 00:00]

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-convert-config nil (list :log-into-drawer t :clock-into-drawer t))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGBOOK:
 ;      CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:"

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-convert-config nil (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING"))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGGING:
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
 ;      :END:"

;; Given the following contents:
; * headline
; :LOGBOOK:
; CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
; - note taken on [2018-12-30 Sun 00:00]
; :END:

(->> (org-ml-parse-this-headline)
  (org-ml-headline-logbook-convert-config (list :log-into-drawer t :clock-into-drawer t)
					  (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING"))
  (org-ml-to-trimmed-string))
 ;; => "* headline
 ;      :LOGGING:
 ;      - note taken on [2018-12-30 Sun 00:00]
 ;      :END:
 ;      :CLOCKING:
 ;      CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00
 ;      :END:"

```


### Headline (misc)

#### org-ml-headline-get-path `(headline)`

Return tree path of **`headline`** node.

The return value is a list of headline titles (including that from
**`headline`**) leading to the root node.

```el
;; Given the following contents:
; * one
; ** two
; *** three

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-path))
 ;; => '("one")

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-get-subheadlines)
  (car)
  (org-ml-headline-get-subheadlines)
  (car)
  (org-ml-headline-get-path))
 ;; => '("one" "two" "three")

```

#### org-ml-headline-update-item-statistics `(headline)`

Return **`headline`** node with updated statistics cookie via items.

The percent/fraction will be computed as the number of checked items
over the number of items with checkboxes (non-checkbox items will
not be considered).

```el
;; Given the following contents:
; * statistically significant [/]
; - irrelevant data
; - [ ] good data
; - [X] bad data

(->> (org-ml-parse-this-headline)
  (org-ml-headline-update-item-statistics)
  (org-ml-to-trimmed-string))
 ;; => "* statistically significant [1/2]
 ;      - irrelevant data
 ;      - [ ] good data
 ;      - [X] bad data"

;; Given the following contents:
; * statistically significant
; - irrelevant data
; - [ ] good data
; - [X] bad data

;; Do nothing if nothing to update
(->> (org-ml-parse-this-headline)
  (org-ml-headline-update-item-statistics)
  (org-ml-to-trimmed-string))
 ;; => "* statistically significant
 ;      - irrelevant data
 ;      - [ ] good data
 ;      - [X] bad data"

```

#### org-ml-headline-update-todo-statistics `(headline)`

Return **`headline`** node with updated statistics cookie via subheadlines.

The percent/fraction will be computed as the number of done
subheadlines over the number of todo subheadlines (eg non-todo
subheadlines will not be counted).

```el
;; Given the following contents:
; * statistically significant [/]
; ** irrelevant data
; ** TODO good data
; ** DONE bad data

(->> (org-ml-parse-this-subtree)
  (org-ml-headline-update-todo-statistics)
  (org-ml-to-trimmed-string))
 ;; => "* statistically significant [1/2]
 ;      ** irrelevant data
 ;      ** TODO good data
 ;      ** DONE bad data"

;; Given the following contents:
; * statistically significant
; ** irrelevant data
; ** TODO good data
; ** DONE bad data

;; Do nothing if nothing to update
(->> (org-ml-parse-this-subtree)
  (org-ml-headline-update-todo-statistics)
  (org-ml-to-trimmed-string))
 ;; => "* statistically significant
 ;      ** irrelevant data
 ;      ** TODO good data
 ;      ** DONE bad data"

```

#### org-ml-headline-demote-subheadline `(index headline)`

Return **`headline`** node with child headline at **`index`** demoted.
Unlike [`org-ml-headline-demote-subtree`](#org-ml-headline-demote-subtree-index-headline) this will not demote the
demoted headline node's children.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four

(->> (org-ml-parse-element-at 1)
  (org-ml-headline-demote-subheadline 0)
  (org-ml-to-trimmed-string))
Error

(->> (org-ml-parse-element-at 1)
  (org-ml-headline-demote-subheadline 1)
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      *** three
 ;      *** four"

```

#### org-ml-headline-demote-subtree `(index headline)`

Return **`headline`** node with child headline at **`index`** demoted.
Unlike [`org-ml-headline-demote-subheadline`](#org-ml-headline-demote-subheadline-index-headline) this will also demote the
demoted headline node's children.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four

(->> (org-ml-parse-element-at 1)
  (org-ml-headline-demote-subtree 1)
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      *** three
 ;      **** four"

```

#### org-ml-headline-promote-subheadline `(index child-index headline)`

Return **`headline`** node with a child headline under **`index`** promoted.
The specific child headline to promote is selected by **`child-index`**.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four
; *** four
; *** four

(->> (org-ml-parse-element-at 1)
  (org-ml-headline-promote-subheadline 1 1)
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** three
 ;      *** four
 ;      ** four
 ;      *** four"

```

#### org-ml-headline-promote-all-subheadlines `(index headline)`

Return **`headline`** node with all child headlines under **`index`** promoted.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four
; *** four
; *** four

(->> (org-ml-parse-element-at 1)
  (org-ml-headline-promote-all-subheadlines 1)
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** three
 ;      ** four
 ;      ** four
 ;      ** four"

```


### Plain List

#### org-ml-plain-list-set-type `(type plain-list)`

Return **`plain-list`** node with type property set to **`type`**.
**`type`** is one of the symbols `unordered` or `ordered`.

```el
;; Given the following contents:
; - [ ] one
; - [X] two

(->> (org-ml-parse-this-element)
  (org-ml-plain-list-set-type 'ordered)
  (org-ml-to-trimmed-string))
 ;; => "1. [ ] one
 ;      2. [X] two"

;; Given the following contents:
; 1. [ ] one
; 2. [X] two

(->> (org-ml-parse-this-element)
  (org-ml-plain-list-set-type 'unordered)
  (org-ml-to-trimmed-string))
 ;; => "- [ ] one
 ;      - [X] two"

```

#### org-ml-plain-list-indent-item `(index plain-list)`

Return **`plain-list`** node with child item at **`index`** indented.
Unlike `org-ml-item-indent-item-tree` this will not indent the indented
item node's children.

```el
;; Given the following contents:
; - one
; - two
;   - three
; - four

;; It makes no sense to indent the first item
(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-indent-item 0)
  (org-ml-to-trimmed-string))
Error

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-indent-item 1)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;        - two
 ;        - three
 ;      - four"

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-indent-item 2)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;        - four"

```

#### org-ml-plain-list-indent-item-tree `(index plain-list)`

Return **`plain-list`** node with child item at **`index`** indented.
Unlike `org-ml-item-indent-item` this will also indent the indented item
node's children.

```el
;; Given the following contents:
; - one
; - two
;   - three
; - four

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-indent-item-tree 1)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;        - two
 ;          - three
 ;      - four"

```

#### org-ml-plain-list-outdent-item `(index child-index plain-list)`

Return **`plain-list`** node with a child item under **`index`** outdented.
The specific child item to outdent is selected by **`child-index`**.

```el
;; Given the following contents:
; - one
; - two
;   - three
;   - three
;   - three
; - four

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-outdent-item 1 0)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;      - three
 ;        - three
 ;        - three
 ;      - four"

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-outdent-item 1 1)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;      - three
 ;        - three
 ;      - four"

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-outdent-item 2 1)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;        - three
 ;        - three
 ;      - four"

```

#### org-ml-plain-list-outdent-all-items `(index plain-list)`

Return **`plain-list`** node with all child items under **`index`** outdented.

```el
;; Given the following contents:
; - one
; - two
;   - three
;   - three
;   - three
; - four

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-outdent-all-items 1)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;      - three
 ;      - three
 ;      - three
 ;      - four"

(->> (org-ml-parse-element-at 1)
  (org-ml-plain-list-outdent-all-items 2)
  (org-ml-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;        - three
 ;        - three
 ;      - four"

```


### Table

#### org-ml-table-get-cell `(row-index column-index table)`

Return table-cell node at **`row-index`** and **`column-index`** in **`table`** node.
Rule-type rows do not count toward row indices.

```el
;; Given the following contents:
; | 1 | 2 | 3 |
; |---+---+---|
; | a | b | c |

(->> (org-ml-parse-this-element)
  (org-ml-table-get-cell 0 0)
  (org-ml-get-children)
  (car))
 ;; => "1"

(->> (org-ml-parse-this-element)
  (org-ml-table-get-cell 1 1)
  (org-ml-get-children)
  (car))
 ;; => "b"

(->> (org-ml-parse-this-element)
  (org-ml-table-get-cell -1 -1)
  (org-ml-get-children)
  (car))
 ;; => "c"

```

#### org-ml-table-delete-column `(column-index table)`

Return **`table`** node with column at **`column-index`** deleted.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (org-ml-parse-this-element)
  (org-ml-table-delete-column 0)
  (org-ml-to-trimmed-string))
 ;; => "| b |
 ;      |---|
 ;      | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-delete-column 1)
  (org-ml-to-trimmed-string))
 ;; => "| a |
 ;      |---|
 ;      | c |"

(->> (org-ml-parse-this-element)
  (org-ml-table-delete-column -1)
  (org-ml-to-trimmed-string))
 ;; => "| a |
 ;      |---|
 ;      | c |"

```

#### org-ml-table-delete-row `(row-index table)`

Return **`table`** node with row at **`row-index`** deleted.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (org-ml-parse-this-element)
  (org-ml-table-delete-row 0)
  (org-ml-to-trimmed-string))
 ;; => "|---+---|
 ;      | c | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-delete-row 1)
  (org-ml-to-trimmed-string))
 ;; => "| a | b |
 ;      | c | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-delete-row -1)
  (org-ml-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|"

```

#### org-ml-table-insert-column! `(column-index column-text table)`

Return **`table`** node with **`column-text`** inserted at **`column-index`**.

**`column-index`** is the index of the column and **`column-text`** is a list of
strings to be made into table-cells to be inserted following the same
syntax as [`org-ml-build-table-cell!`](#org-ml-build-table-cell-string).

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (org-ml-parse-this-element)
  (org-ml-table-insert-column! 1 '("x" "y"))
  (org-ml-to-trimmed-string))
 ;; => "| a | x | b |
 ;      |---+---+---|
 ;      | c | y | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-insert-column! -1 '("x" "y"))
  (org-ml-to-trimmed-string))
 ;; => "| a | b | x |
 ;      |---+---+---|
 ;      | c | d | y |"

```

#### org-ml-table-insert-row! `(row-index row-text table)`

Return **`table`** node with **`row-text`** inserted at **`row-index`**.

**`row-index`** is the index of the column and **`row-text`** is a list of strings
to be made into table-cells to be inserted following the same syntax
as [`org-ml-build-table-row!`](#org-ml-build-table-row-row-list).

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (org-ml-parse-this-element)
  (org-ml-table-insert-row! 1 '("x" "y"))
  (org-ml-to-trimmed-string))
 ;; => "| a | b |
 ;      | x | y |
 ;      |---+---|
 ;      | c | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-insert-row! 2 '("x" "y"))
  (org-ml-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | x | y |
 ;      | c | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-insert-row! -1 '("x" "y"))
  (org-ml-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | c | d |
 ;      | x | y |"

```

#### org-ml-table-replace-cell! `(row-index column-index cell-text table)`

Return **`table`** node with a table-cell node replaced by **`cell-text`**.

If **`cell-text`** is a string, it will replace the children of the
table-cell at **`row-index`** and **`column-index`** in **`table`**. **`cell-text`** will be
processed the same as the argument given to [`org-ml-build-table-cell!`](#org-ml-build-table-cell-string).

If **`cell-text`** is nil, it will set the cell to an empty string.

```el
;; Given the following contents:
; | 1 | 2 |
; |---+---|
; | a | b |

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-cell! 0 0 "2")
  (org-ml-to-trimmed-string))
 ;; => "| 2 | 2 |
 ;      |---+---|
 ;      | a | b |"

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-cell! 0 0 nil)
  (org-ml-to-trimmed-string))
 ;; => "|   | 2 |
 ;      |---+---|
 ;      | a | b |"

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-cell! -1 -1 "B")
  (org-ml-to-trimmed-string))
 ;; => "| 1 | 2 |
 ;      |---+---|
 ;      | a | B |"

```

#### org-ml-table-replace-column! `(column-index column-text table)`

Return **`table`** node with the column at **`column-index`** replaced by **`column-text`**.

If **`column-text`** is a list of strings, it will replace the table-cells
at **`column-index`**. Each member of **`column-text`** will be processed the
same as the argument given to [`org-ml-build-table-cell!`](#org-ml-build-table-cell-string).

If **`column-text`** is nil, it will clear all cells at **`column-index`**.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-column! 0 '("A" "B"))
  (org-ml-to-trimmed-string))
 ;; => "| A | b |
 ;      |---+---|
 ;      | B | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-column! 0 nil)
  (org-ml-to-trimmed-string))
 ;; => "|   | b |
 ;      |---+---|
 ;      |   | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-column! -1 '("A" "B"))
  (org-ml-to-trimmed-string))
 ;; => "| a | A |
 ;      |---+---|
 ;      | c | B |"

```

#### org-ml-table-replace-row! `(row-index row-text table)`

Return **`table`** node with the row at **`row-index`** replaced by **`row-text`**.

If **`row-text`** is a list of strings, it will replace the cells at
**`row-index`**. Each member of **`row-text`** will be processed the same as
the argument given to [`org-ml-build-table-row!`](#org-ml-build-table-row-row-list).

If **`row-text`** is nil, it will clear all cells at **`row-index`**.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-row! 0 '("A" "B"))
  (org-ml-to-trimmed-string))
 ;; => "| A | B |
 ;      |---+---|
 ;      | c | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-row! 0 nil)
  (org-ml-to-trimmed-string))
 ;; => "|   |   |
 ;      |---+---|
 ;      | c | d |"

(->> (org-ml-parse-this-element)
  (org-ml-table-replace-row! -1 '("A" "B"))
  (org-ml-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | A | B |"

```


## Node Matching


Use pattern-matching to selectively perform operations on nodes in trees.

#### org-ml-match `(pattern node)`

Return a list of child nodes matching **`pattern`** in **`node`**.

**`pattern`** is a list like `([slicer [x] [y]] [sub1 ...])`.

`slicer` is an optional prefix to the pattern describing how many
and which matches to return. If not given, all matches are
returned. Possible values are:

- `:first` - return the first match
- `:last` - return the last match
- `:nth` `x` - return the nth match where `x` is an integer denoting
    the index to return (starting at 0). `x` may be a negative number
    to start counting at the end of the match list, in which case
    -1 is the last index. Using 0 and -1 for `x` is equivalent to
    using `:first` and `:last` respectively
- `:sub` `x` `y` - return a sublist between indices `x` and `y`. `x` may
    not be greater than `y`, and both must either be non-negative
    integers or negative integers. In the case of negative
    integers, the indices refer to the same counterparts as
    described in `:nth`. If `x` and `y` are equal, this slicer has the
    same behavior as `:nth`.

`subx` denotes subpatterns that that match nodes in the parse tree.
Subpatterns may either be wildcards or conditions.

Conditions match exactly one level of the node tree being
searched based on the node's type (the symbol returned by
[`org-ml-get-type`](#org-ml-get-type-node)), properties (the value returned by
[`org-ml-get-property`](#org-ml-get-property-prop-node) for a valid property keyword), and
index (the position of the node in the list returned by
[`org-ml-get-children`](#org-ml-get-children-branch-node)). For index, both left indices (where zero
refers to the left end of the list) and right indices (where -1
refers to the right end of the list) are understood. Conditions
may either be atomic or compound, where compound conditions are
themselves composed of atomic or compound conditions.

The types of atomic conditions are:

- `type` - match when the node's type is `eq` to `type` (a symbol)
- `index` - match when the node's index is `=` to `index` (an
    integer)
- `(op index)` - match when `(op node-index index)` returns t. `op` is
    one of `<`, `>`, `<=`, or `>=` and `node-index` is the index of
    the node being evaluated
- `(prop val)` - match nodes whose property `prop` (a keyword) is
    `equal` to `val`; `val` is obtained by evaluating
    [`org-ml-get-property`](#org-ml-get-property-prop-node) with `prop` and the current node; if `prop`
    is invalid, an error will be thrown
- `(:pred pred)` - match when `pred` evaluates to t; `pred` is a symbol
    for a unary function that takes the current node as its
    argument

Compound conditions start with an operator followed by their
component conditions. The types of compound conditions are:

- `(:and c1 c2 [c3 ...])` - match when all ``c`` are true
- `(:or c1 c2 [c3 ...])` - match when at least one ``c`` is true
- `(:not c)` - match when ``c`` is not true

In addition, `subx` may be a wildcard keyword or symbol. These are
analogous to the special characters found in `posix` extended
regular expression syntax. Specifically, `[` and `]` correspond
to `{` and `}` respectively and `:any` corresponds to the `.`
operator. All other characters have the same meaning between this
function and `posix` extended regular expressions.:

- `:any` - always match exactly one node
- `sub` `?` - match `sub` zero or once
- `sub` `*` - match `sub` zero or more times
- `sub` `+` - match `sub` one or more times
- `sub` [`n`] - match `sub` `n` times
- `sub` [`m` `n`] - match `sub` `m` to `n` times (inclusive); if `m` or `n` is
    nil, this will match 'at most `n` times' or 'at least `m` times'
    respectively
- `(alt-a1 [alt-a2 ...] | alt-b1 [alt-b2 ...] [| ...])` - match
    any of the `alt` expressions separated by `|` where `alt` is a list
    of subpatterns as described above or nil to match nothing;
    these expressions may be nested

If **`pattern`** is nil, return **`node`**. Likewise, if any wildcard
patterns match the nil pattern, also return **`node`** along with
anything else the wildcard matches. Examples of this would
be `(sub *)`, `(sub ?)`, and `((nil | sub))`.

For increased performance, this function (and all others that
consume a **`pattern`** parameter) can be memoized using
`org-ml-memoize-match-patterns`. If nil, **`pattern`** is processed
into a lambda form for every function call. If t, the resulting
lambda forms are cached for each unique **`pattern`**, running
generation step only once if multiple instances of the same
**`pattern`** are used. Note that `org-ml-memoize-match-patterns` is
shared between all functions that consume a **`pattern`** parameter.

```el
;; Given the following contents:
; * headline 1
; ** TODO headline 2
; stuff
; - item 1
; - item 2
; - item 3
; ** DONE headline 3
; - item 4
; - item 5
; - item 6
; ** TODO COMMENT headline 4
; - item 7
; - item 8
; - item 9

;; Match items (excluding the first) in headlines that are marked "TODO" and not
;; commented. The :many keyword matches the section and plain-list nodes holding
;; the items.
(->> (org-ml-parse-this-subtree)
  (org-ml-match '((:and (:todo-keyword "TODO")
			(:commentedp nil))
		  :any * (:and item (> 0))))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("- item 2" "- item 3")

;; Given the following contents:
; *one* *two* *three* *four* *five* *six*

;; Return all bold nodes
(->> (org-ml-parse-this-element)
  (org-ml-match '(bold))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("*one*" "*two*" "*three*" "*four*" "*five*" "*six*")

;; Return first bold node
(->> (org-ml-parse-this-element)
  (org-ml-match '(:first bold))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("*one*")

;; Return last bold node
(->> (org-ml-parse-this-element)
  (org-ml-match '(:last bold))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("*six*")

;; Return a select bold node
(->> (org-ml-parse-this-element)
  (org-ml-match '(:nth 2 bold))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("*three*")

;; Return a sublist of matched bold nodes
(->> (org-ml-parse-this-element)
  (org-ml-match '(:sub 1 3 bold))
  (-map #'org-ml-to-trimmed-string))
 ;; => '("*two*" "*three*" "*four*")

```

#### org-ml-match-delete `(pattern node)`

Return **`node`** without children matching **`pattern`**.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * headline one
; ** headline two
; ** headline three
; ** headline four

;; Selectively delete headlines
(->> (org-ml-parse-this-subtree)
  (org-ml-match-delete '(headline))
  (org-ml-to-trimmed-string))
 ;; => "* headline one"

(->> (org-ml-parse-this-subtree)
  (org-ml-match-delete '(:first headline))
  (org-ml-to-trimmed-string))
 ;; => "* headline one
 ;      ** headline three
 ;      ** headline four"

(->> (org-ml-parse-this-subtree)
  (org-ml-match-delete '(:last headline))
  (org-ml-to-trimmed-string))
 ;; => "* headline one
 ;      ** headline two
 ;      ** headline three"

```

#### org-ml-match-extract `(pattern node)`

Remove nodes matching **`pattern`** from **`node`**.
Return cons cell where the car is a list of all removed nodes and
the cdr is the modified **`node`**.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; pull me /under/

(--> (org-ml-parse-this-element)
  (org-ml-match-extract '(:any * italic)
			 it)
  (cons (-map #'org-ml-to-trimmed-string (car it))
	(org-ml-to-trimmed-string (cdr it))))
 ;; => '(("/under/") . "pull me")

```

#### org-ml-match-map `(pattern fun node)`

Return **`node`** with **`fun`** applied to children matching **`pattern`**.
**`fun`** is a unary function that takes a node and returns a new node
which will replace the original.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** headline three
; ** headline four

;; Selectively mark headlines as DONE
(->> (org-ml-parse-this-subtree)
  (org-ml-match-map '(headline)
    (lambda (it)
      (org-ml-set-property :todo-keyword "DONE" it)))
  (org-ml-to-trimmed-string))
 ;; => "* headline one
 ;      ** DONE headline two
 ;      ** DONE headline three
 ;      ** DONE headline four"

(->> (org-ml-parse-this-subtree)
  (org-ml-match-map* '(:first headline)
    (org-ml-set-property :todo-keyword "DONE" it))
  (org-ml-to-trimmed-string))
 ;; => "* headline one
 ;      ** DONE headline two
 ;      ** headline three
 ;      ** headline four"

(->> (org-ml-parse-this-subtree)
  (org-ml-match-map '(:last headline)
    (-partial #'org-ml-set-property :todo-keyword "DONE"))
  (org-ml-to-trimmed-string))
 ;; => "* headline one
 ;      ** TODO headline two
 ;      ** headline three
 ;      ** DONE headline four"

```

#### org-ml-match-mapcat `(pattern fun node)`

Return **`node`** with **`fun`** applied to children matching **`pattern`**.
**`fun`** is a unary function that takes a node and returns a list of new
nodes which will be spliced in place of the original node.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two

(->> (org-ml-parse-this-subtree)
  (org-ml-match-mapcat* '(:first headline)
    (list (org-ml-build-headline! :title-text "1.5" :level 2)
	  it))
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** 1.5
 ;      ** two"

```

#### org-ml-match-replace `(pattern node* node)`

Return **`node`** with **`node*`** in place of children matching **`pattern`**.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; *1* 2 *3* 4 *5* 6 *7* 8 *9* 10

(->> (org-ml-parse-this-element)
  (org-ml-match-replace '(:any * bold)
    (org-ml-build-bold :post-blank 1 "0"))
  (org-ml-to-trimmed-string))
 ;; => "*0* 2 *0* 4 *0* 6 *0* 8 *0* 10"

```

#### org-ml-match-insert-before `(pattern node* node)`

Return **`node`** with **`node*`** inserted before children matching **`pattern`**.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (org-ml-parse-this-subtree)
  (org-ml-match-insert-before '(headline)
    (org-ml-build-headline! :title-text "new" :level 2))
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** new
 ;      ** two
 ;      ** new
 ;      ** three"

```

#### org-ml-match-insert-after `(pattern node* node)`

Return **`node`** with **`node*`** inserted after children matching **`pattern`**.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (org-ml-parse-this-subtree)
  (org-ml-match-insert-after '(headline)
    (org-ml-build-headline! :title-text "new" :level 2))
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** new
 ;      ** three
 ;      ** new"

```

#### org-ml-match-insert-within `(pattern index node* node)`

Return **`node`** with **`node*`** inserted at **`index`** in children matching **`pattern`**.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node) with the exception
that **`pattern`** may be nil. In this case **`node*`** will be inserted at **`index`**
in the immediate, top level children of **`node`**.

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (org-ml-parse-this-subtree)
  (org-ml-match-insert-within '(headline)
      0 (org-ml-build-headline! :title-text "new" :level 3))
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      *** new
 ;      ** three
 ;      *** new"

;; The nil pattern denotes top-level element
(->> (org-ml-parse-this-subtree)
  (org-ml-match-insert-within nil 1 (org-ml-build-headline! :title-text "new" :level 2))
  (org-ml-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** new
 ;      ** three"

```

#### org-ml-match-splice `(pattern nodes* node)`

Return **`node`** with **`nodes*`** spliced in place of children matching **`pattern`**.
**`nodes*`** is a list of nodes.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(let ((L (list (org-ml-build-headline! :title-text "new0" :level 2)
	       (org-ml-build-headline! :title-text "new1" :level 2))))
  (->> (org-ml-parse-this-subtree)
    (org-ml-match-splice '(0)
      L)
    (org-ml-to-trimmed-string)))
 ;; => "* one
 ;      ** new0
 ;      ** new1
 ;      ** three"

```

#### org-ml-match-splice-before `(pattern nodes* node)`

Return **`node`** with **`nodes*`** spliced before children matching **`pattern`**.
**`nodes*`** is a list of nodes.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(let ((L (list (org-ml-build-headline! :title-text "new0" :level 2)
	       (org-ml-build-headline! :title-text "new1" :level 2))))
  (->> (org-ml-parse-this-subtree)
    (org-ml-match-splice-before '(0)
      L)
    (org-ml-to-trimmed-string)))
 ;; => "* one
 ;      ** new0
 ;      ** new1
 ;      ** two
 ;      ** three"

```

#### org-ml-match-splice-after `(pattern nodes* node)`

Return **`node`** with **`nodes*`** spliced after children matching **`pattern`**.
**`nodes*`** is a list of nodes.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(let ((L (list (org-ml-build-headline! :title-text "new0" :level 2)
	       (org-ml-build-headline! :title-text "new1" :level 2))))
  (->> (org-ml-parse-this-subtree)
    (org-ml-match-splice-after '(0)
      L)
    (org-ml-to-trimmed-string)))
 ;; => "* one
 ;      ** two
 ;      ** new0
 ;      ** new1
 ;      ** three"

```

#### org-ml-match-splice-within `(pattern index nodes* node)`

Return **`node`** with **`nodes*`** spliced at **`index`** in children matching **`pattern`**.
**`nodes*`** is a list of nodes.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node) with the exception
that **`pattern`** may be nil. In this case **`nodes*`** will be inserted at **`index`**
in the immediate, top level children of **`node`**.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four

(let ((L (list (org-ml-build-headline! :title-text "new0" :level 3)
	       (org-ml-build-headline! :title-text "new1" :level 3))))
  (->> (org-ml-parse-this-subtree)
    (org-ml-match-splice-within '(headline)
	0 L)
    (org-ml-to-trimmed-string)))
 ;; => "* one
 ;      ** two
 ;      *** new0
 ;      *** new1
 ;      ** three
 ;      *** new0
 ;      *** new1
 ;      *** four"

(let ((L (list (org-ml-build-headline! :title-text "new0" :level 2)
	       (org-ml-build-headline! :title-text "new1" :level 2))))
  (->> (org-ml-parse-this-subtree)
    (org-ml-match-splice-within nil 1 L)
    (org-ml-to-trimmed-string)))
 ;; => "* one
 ;      ** two
 ;      ** new0
 ;      ** new1
 ;      ** three
 ;      *** four"

```

#### org-ml-match-do `(pattern fun node)`

Like [`org-ml-match-map`](#org-ml-match-map-pattern-fun-node) but for side effects only.
**`fun`** is a unary function that has side effects and is applied to the
matches from **`node`** using **`pattern`**. This function itself returns nil.

**`pattern`** follows the same rules as [`org-ml-match`](#org-ml-match-pattern-node).

```el
no examples :(
```


## Buffer Side Effects


Map node manipulations into buffers.


### Insert

#### org-ml-insert `(point node)`

Convert **`node`** to a string and insert at **`point`** in the current buffer.
**`node`** may be a node or a list of nodes. Return **`node`**.

```el
;; Given the following contents:
; * one
; 

;; Insert single node
(->> (org-ml-build-headline! :title-text "two")
  (org-ml-insert (point-max)))
 ;; Output these buffer contents
 ;; $> "* one
 ;      * two"

;; Insert multiple nodes
(->> (org-ml-build-headline! :title-text "two")
  (list (org-ml-build-headline! :title-text "more"))
  (org-ml-insert (point-max)))
 ;; Output these buffer contents
 ;; $> "* one
 ;      * more
 ;      * two"

;; Given the following contents:
; a *game* or a /boy/

(->> (org-ml-build-paragraph! "we don't care if you're")
  (org-ml-insert (point-min)))
 ;; Output these buffer contents
 ;; $> "we don't care if you're
 ;      a *game* or a /boy/"

```

#### org-ml-insert-tail `(point node)`

Like [`org-ml-insert`](#org-ml-insert-point-node) but insert **`node`** at **`point`** and move to end of insertion.

```el
no examples :(
```


### Update

#### org-ml-update `(fun node)`

Replace **`node`** in the current buffer with a new one.
**`fun`** is a unary function that takes **`node`** and returns a modified node
or list of nodes.

The modified **`node`** will be converted to a string and then compared
to the old buffer string using the Myers diff algorithm. This has
an average time complexity of `o`(`m`+`n`+`d`^2) where `m` and `n` are the
lengths of the old and new strings respectively and `d` is the
number of inserts or deletes required to change one into the
other. At the cost of performance, only the parts of the buffer
that need to be modified will actually be changed, which is less
likely to disturb overlays and move the cursor (and is also more
like how org-mode's build-in imperative functions behave).

If one does not need this level of precision, use the function
`org-ml~update` and supply nil for the `diff-mode` argument. This
will simply replace the old node's string representation with the
modified node's string in its entirety. This will likely be
faster but could destroy overlays (eg folding) and will
reposition the cursor to the beginning of **`node`** if it is in the
middle of **`node`**.

```el
;; Given the following contents:
; * TODO win grammy

(->> (org-ml-parse-this-headline)
  (org-ml-update (lambda (hl)
		   (org-ml-set-property :todo-keyword "DONE" hl))))
 ;; Output these buffer contents
 ;; $> "* DONE win grammy"

;; Given the following contents:
; * win grammy [0/0]
; - [ ] write punk song
; - [ ] get new vocalist
; - [ ] sell 2 singles

(->> (org-ml-parse-this-headline)
  (org-ml-update* (->> (org-ml-match-map '(:any * item)
			 #'org-ml-item-toggle-checkbox it)
		    (org-ml-headline-update-item-statistics))))
 ;; Output these buffer contents
 ;; $> "* win grammy [3/3]
 ;      - [X] write punk song
 ;      - [X] get new vocalist
 ;      - [X] sell 2 singles"

```

#### org-ml-update-object-at `(point fun)`

Update object under **`point`** using **`fun`**.
**`fun`** takes an object and returns a modified object

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; [[http://example.com][desc]]

(org-ml-update-object-at* (point)
  (org-ml-set-property :path "//buymoreram.com" it))
 ;; Output these buffer contents
 ;; $> "[[http://buymoreram.com][desc]]"

```

#### org-ml-update-element-at `(point fun)`

Update element under **`point`** using **`fun`**.
**`fun`** takes an element and returns a modified element

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; #+call: ktulu()

(org-ml-update-element-at* (point)
  (org-ml-set-properties (list :call "cthulhu" :inside-header '(:cache no)
				:arguments '("x=4")
				:end-header '(:results html))
			 it))
 ;; Output these buffer contents
 ;; $> "#+call: cthulhu[:cache no](x=4) :results html"

```

#### org-ml-update-table-row-at `(point fun)`

Update table-row under **`point`** using **`fun`**.
**`fun`** takes an table-row and returns a modified table-row

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; | a | b |

(org-ml-update-table-row-at* (point)
  (org-ml-map-children* (cons (org-ml-build-table-cell! "0")
			      it)
			it))
 ;; Output these buffer contents
 ;; $> "| 0 | a | b |"

```

#### org-ml-update-item-at `(point fun)`

Update item under **`point`** using **`fun`**.
**`fun`** takes an item and returns a modified item

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; - [ ] thing

(org-ml-update-item-at* (point)
  (org-ml-item-toggle-checkbox it))
 ;; Output these buffer contents
 ;; $> "- [X] thing"

```

#### org-ml-update-headline-at `(point fun)`

Update headline under **`point`** using **`fun`**.
**`fun`** takes an headline and returns a modified headline

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; * TODO might get done
; * DONE no need to update

(org-ml-update-headline-at* (point)
  (org-ml-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* DONE might get done
 ;      * DONE no need to update"

```

#### org-ml-update-subtree-at `(point fun)`

Update subtree under **`point`** using **`fun`**.
**`fun`** takes an subtree and returns a modified subtree

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; * not updated

(org-ml-update-subtree-at* (point)
  (org-ml-headline-demote-subheadline 1 it))
 ;; Output these buffer contents
 ;; $> "* one
 ;      ** two
 ;      *** three
 ;      * not updated"

```

#### org-ml-update-section-at `(point fun)`

Update section under **`point`** using **`fun`**.
**`fun`** takes an section and returns a modified section

This function uses the Myers diff algorithm.
See [`org-ml-update`](#org-ml-update-fun-node) for what this means.

```el
;; Given the following contents:
; #+key1: VAL1
; #+key2: VAL2
; * irrelevant headline

;; Update the top buffer section before the headlines start
(org-ml-update-section-at* (point)
  (org-ml-map-children* (--map (org-ml-map-property :value #'s-downcase it)
			       it)
			it))
 ;; Output these buffer contents
 ;; $> "#+key1: val1
 ;      #+key2: val2
 ;      * irrelevant headline"

```

#### org-ml-update-headlines `(which fun)`

Update some headlines in the current using **`fun`**.

See [`org-ml-parse-headlines`](#org-ml-parse-headlines-which) for the meaning of **`which`**.

Headlines are updated using `org-ml~update` with `diff-arg` set to
nil (see this for use and meaning of **`fun`**).

```el
;; Given the following contents:
; * one
; * two
; * three

(org-ml-update-headlines* 0 (org-ml-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* DONE one
 ;      * two
 ;      * three"

(org-ml-update-headlines* '(0 1)
  (org-ml-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* DONE one
 ;      * DONE two
 ;      * three"

(org-ml-update-headlines* [2 nil]
  (org-ml-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* one
 ;      * DONE two
 ;      * DONE three"

(org-ml-update-headlines* [2 10]
  (org-ml-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* one
 ;      * DONE two
 ;      * three"

;; Given the following contents:
; * one
; * two
; * three

(org-ml-update-headlines* 'all (org-ml-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* DONE one
 ;      * DONE two
 ;      * DONE three"

```

#### org-ml-update-subtrees `(which fun)`

Update some toplevel subtrees in the current buffer using **`fun`**.

See [`org-ml-parse-subtrees`](#org-ml-parse-subtrees-which) for the meaning of **`which`**.

Subtrees are updated using `org-ml~update` with `diff-arg` set to
nil (see this for use and meaning of **`fun`**).

```el
;; Given the following contents:
; * one [/]
; ** DONE _one
; * two [/]
; ** DONE _one
; * three [/]
; ** DONE _one


;; Given the following contents:
; * one [1/1]
; ** DONE _one
; * two [/]
; ** DONE _one
; * three [/]
; ** DONE _one


;; Given the following contents:
; * one [1/1]
; ** DONE _one
; * two [1/1]
; ** DONE _one
; * three [/]
; ** DONE _one


;; Given the following contents:
; * one [/]
; ** DONE _one
; * two [1/1]
; ** DONE _one
; * three [1/1]
; ** DONE _one


;; Given the following contents:
; * one [1/1]
; ** DONE _one
; * two [/]
; ** DONE _one
; * three [/]
; ** DONE _one


;; Given the following contents:
; * one [2/2]
; ** DONE _one
; ** DONE _two
; * two [2/2]
; ** DONE _one
; ** DONE _two

```


### Misc

#### org-ml-fold `(node)`

Fold the children of **`node`** if they exist.

```el
no examples :(
```

#### org-ml-unfold `(node)`

Unfold the children of **`node`** if they exist.

```el
no examples :(
```
Version: 5.7.1