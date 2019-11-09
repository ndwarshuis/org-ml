
### Printing functions


print shit

* [om-elem-to-string](#om-elem-to-string-elem) `(elem)`
* [om-elem-to-trimmed-string](#om-elem-to-trimmed-string-elem) `(elem)`

### Object Builders


build shit

* [om-elem-build-code](#om-elem-build-code-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-inline-babel-call](#om-elem-build-inline-babel-call-call-key-post-blank-arguments-inside-header-end-header) `(call &key post-blank arguments inside-header end-header)`
* [om-elem-build-inline-src-block](#om-elem-build-inline-src-block-language-value-key-parameters-post-blank) `(language value &key parameters post-blank)`
* [om-elem-build-line-break](#om-elem-build-line-break-key-post-blank) `(&key post-blank)`
* [om-elem-build-statistics-cookie](#om-elem-build-statistics-cookie-optional-num-dem-key-post-blank) `(&optional num dem &key post-blank)`
* [om-elem-build-target](#om-elem-build-target-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-timestamp](#om-elem-build-timestamp-type-time1-optional-time2-key-post-blank-repeater-type-repeater-unit-repeater-value-warning-type-warning-unit-warning-value) `(type time1 &optional time2 &key post-blank repeater-type repeater-unit repeater-value warning-type warning-unit warning-value)`
* [om-elem-build-verbatim](#om-elem-build-verbatim-value-key-post-blank) `(value &key post-blank)`

### Recursive Object Builders

* [om-elem-build-bold](#om-elem-build-bold-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-footnote-reference](#om-elem-build-footnote-reference-key-post-blank-label-1-rest-objs) `(&key post-blank (label 1) &rest objs)`
* [om-elem-build-italic](#om-elem-build-italic-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-link](#om-elem-build-link-path-key-post-blank-type-fuzzy-rest-objs) `(path &key post-blank (type fuzzy) &rest objs)`
* [om-elem-build-radio-target](#om-elem-build-radio-target-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-strike-through](#om-elem-build-strike-through-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-superscript](#om-elem-build-superscript-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-subscript](#om-elem-build-subscript-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-table-cell](#om-elem-build-table-cell-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-underline](#om-elem-build-underline-key-post-blank-rest-objs) `(&key post-blank &rest objs)`

### Element Builders

* [om-elem-build-babel-call](#om-elem-build-babel-call-call-key-post-blank-arguments-inside-header-end-header) `(call &key post-blank arguments inside-header end-header)`
* [om-elem-build-clock](#om-elem-build-clock-time1-optional-time2-key-post-blank) `(time1 &optional time2 &key post-blank)`
* [om-elem-build-comment](#om-elem-build-comment-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-comment-block](#om-elem-build-comment-block-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-diary-sexp](#om-elem-build-diary-sexp-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-example-block](#om-elem-build-example-block-value-key-post-blank-switches-preserve-indent) `(value &key post-blank switches preserve-indent)`
* [om-elem-build-export-block](#om-elem-build-export-block-type-value-key-post-blank) `(type value &key post-blank)`
* [om-elem-build-fixed-width](#om-elem-build-fixed-width-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-horizontal-rule](#om-elem-build-horizontal-rule-key-post-blank) `(&key post-blank)`
* [om-elem-build-keyword](#om-elem-build-keyword-key-value-key-post-blank) `(key value &key post-blank)`
* [om-elem-build-latex-environment](#om-elem-build-latex-environment-env-text-key-post-blank) `(env text &key post-blank)`
* [om-elem-build-node-property](#om-elem-build-node-property-key-value-key-post-blank) `(key value &key post-blank)`
* [om-elem-build-planning](#om-elem-build-planning-key-closed-scheduled-deadline-post-blank) `(&key closed scheduled deadline post-blank)`
* [om-elem-build-src-block](#om-elem-build-src-block-value-key-language-switches-parameters-preserve-indent-post-blank) `(value &key language switches parameters preserve-indent post-blank)`
* [om-elem-build-table-row-hline](#om-elem-build-table-row-hline-key-post-blank) `(&key post-blank)`

### Container Element Builders


### Greater Element Builders


Build shit

* [om-elem-build-center-block](#om-elem-build-center-block-key-post-blank-rest-elems) `(&key post-blank &rest elems)`
* [om-elem-build-drawer](#om-elem-build-drawer-drawer-name-key-post-blank-rest-elems) `(drawer-name &key post-blank &rest elems)`
* [om-elem-build-footnote-definition](#om-elem-build-footnote-definition-label-key-post-blank-rest-elems) `(label &key post-blank &rest elems)`
* [om-elem-build-headline](#om-elem-build-headline-key-title-level-1-post-blank-pre-blank-todo-keyword-tags-priority-footnote-section-p-commentedp-archivedp-rest-elems) `(&key title (level 1) post-blank pre-blank todo-keyword tags priority footnote-section-p commentedp archivedp &rest elems)`
* [om-elem-build-item](#om-elem-build-item-key-pre-blank-0-post-blank-bullet-quote---checkbox-tag-counter-rest-elems) `(&key (pre-blank 0) post-blank (bullet '-) checkbox tag counter &rest elems)`
* [om-elem-build-plain-list](#om-elem-build-plain-list-key-post-blank-rest-elems) `(&key post-blank &rest elems)`
* [om-elem-build-property-drawer](#om-elem-build-property-drawer-key-post-blank-rest-elems) `(&key post-blank &rest elems)`
* [om-elem-build-quote-block](#om-elem-build-quote-block-key-post-blank-rest-elems) `(&key post-blank &rest elems)`
* [om-elem-build-section](#om-elem-build-section-key-post-blank-rest-elems) `(&key post-blank &rest elems)`
* [om-elem-build-table](#om-elem-build-table-key-tblfm-post-blank-rest-elems) `(&key tblfm post-blank &rest elems)`

### Element matching functions


match shit

* [om-elem-find](#om-elem-find-queries-elem) `(queries elem)`
* [om-elem-find-first](#om-elem-find-first-queries-elem) `(queries elem)`
* [om-elem-find-last](#om-elem-find-last-queries-elem) `(queries elem)`
* [om-elem-delete](#om-elem-delete-queries-elem) `(queries elem)`
* [om-elem-extract](#om-elem-extract-queries-elem) `(queries elem)`
* [om-elem-map](#om-elem-map-queries-fun-elem) `(queries fun elem)`
* [om-elem-mapcat](#om-elem-mapcat-queries-fun-elem) `(queries fun elem)`
* [om-elem-replace](#om-elem-replace-queries-rep-elem) `(queries rep elem)`
* [om-elem-insert-before](#om-elem-insert-before-queries-elem-elem) `(queries elem* elem)`
* [om-elem-insert-after](#om-elem-insert-after-queries-elem-elem) `(queries elem* elem)`
* [om-elem-insert-within](#om-elem-insert-within-queries-index-elem-elem) `(queries index elem* elem)`
* [om-elem-splice-before](#om-elem-splice-before-queries-elem-elem) `(queries elem* elem)`
* [om-elem-splice-after](#om-elem-splice-after-queries-elem-elem) `(queries elem* elem)`
* [om-elem-splice-within](#om-elem-splice-within-queries-index-elem-elem) `(queries index elem* elem)`

### Element get functions


get shit

* [om-elem-headline-get-subheadlines](#om-elem-headline-get-subheadlines-headline) `(headline)`
* [om-elem-headline-get-section](#om-elem-headline-get-section-headline) `(headline)`
* [om-elem-headline-get-drawer](#om-elem-headline-get-drawer-name-headline) `(name headline)`
* [om-elem-item-get-sublist](#om-elem-item-get-sublist-item) `(item)`
* [om-elem-item-get-paragraph](#om-elem-item-get-paragraph-item) `(item)`
* [om-elem-table-get-cell](#om-elem-table-get-cell-row-column-table) `(row column table)`
* [om-elem-timestamp-get-start](#om-elem-timestamp-get-start-timestamp) `(timestamp)`
* [om-elem-timestamp-get-end](#om-elem-timestamp-get-end-timestamp) `(timestamp)`

### Element predicate functions


pred shit

* [om-elem-is-empty-p](#om-elem-is-empty-p-elem) `(elem)`
* [om-elem-property-is-nil-p](#om-elem-property-is-nil-p-prop-elem) `(prop elem)`
* [om-elem-property-is-non-nil-p](#om-elem-property-is-non-nil-p-prop-elem) `(prop elem)`
* [om-elem-property-is-eq-p](#om-elem-property-is-eq-p-prop-val-elem) `(prop val elem)`
* [om-elem-property-is-equal-p](#om-elem-property-is-equal-p-prop-val-elem) `(prop val elem)`
* [om-elem-property-is-predicate-p](#om-elem-property-is-predicate-p-prop-fun-elem) `(prop fun elem)`
* [om-elem-contains-point-p](#om-elem-contains-point-p-point-elem) `(point elem)`
* [om-elem-contents-contains-point-p](#om-elem-contents-contains-point-p-point-elem) `(point elem)`
* [om-elem-is-type-p](#om-elem-is-type-p-type-elem) `(type elem)`
* [om-elem-is-any-type-p](#om-elem-is-any-type-p-types-elem) `(types elem)`
* [om-elem-clock-is-running-p](#om-elem-clock-is-running-p-clock) `(clock)`
* [om-elem-headline-is-done-p](#om-elem-headline-is-done-p-headline) `(headline)`
* [om-elem-headline-is-scheduled-p](#om-elem-headline-is-scheduled-p-headline) `(headline)`
* [om-elem-headline-is-deadlined-p](#om-elem-headline-is-deadlined-p-headline) `(headline)`
* [om-elem-headline-is-closed-p](#om-elem-headline-is-closed-p-headline) `(headline)`
* [om-elem-headline-is-archived-p](#om-elem-headline-is-archived-p-headline) `(headline)`
* [om-elem-headline-is-commented-p](#om-elem-headline-is-commented-p-headline) `(headline)`
* [om-elem-headline-has-tag-p](#om-elem-headline-has-tag-p-tag-headline) `(tag headline)`
* [om-elem-item-is-unchecked-p](#om-elem-item-is-unchecked-p-item) `(item)`
* [om-elem-item-is-checked-p](#om-elem-item-is-checked-p-item) `(item)`
* [om-elem-item-is-trans-p](#om-elem-item-is-trans-p-item) `(item)`
* [om-elem-timestamp-is-active-p](#om-elem-timestamp-is-active-p-timestamp) `(timestamp)`
* [om-elem-timestamp-is-inactive-p](#om-elem-timestamp-is-inactive-p-timestamp) `(timestamp)`
* [om-elem-timestamp-is-ranged-p](#om-elem-timestamp-is-ranged-p-timestamp) `(timestamp)`

### Element setter functions


set shit

* [om-elem-headline-set-todo](#om-elem-headline-set-todo-todo-headline) `(todo headline)`
* [om-elem-headline-set-archived](#om-elem-headline-set-archived-flag-headline) `(flag headline)`
* [om-elem-headline-set-commented](#om-elem-headline-set-commented-flag-headline) `(flag headline)`
* [om-elem-headline-set-priority](#om-elem-headline-set-priority-priority-headline) `(priority headline)`
* [om-elem-headline-set-title](#om-elem-headline-set-title-title-headline) `(title headline)`
* [om-elem-item-set-checkbox](#om-elem-item-set-checkbox-state-item) `(state item)`
* [om-elem-item-set-bullet](#om-elem-item-set-bullet-bullet-item) `(bullet item)`
* [om-elem-item-set-tag](#om-elem-item-set-tag-tag-item) `(tag item)`
* [om-elem-plain-list-set-type](#om-elem-plain-list-set-type-type-plain-list) `(type plain-list)`
* [om-elem-node-property-set-key](#om-elem-node-property-set-key-key-node-property) `(key node-property)`
* [om-elem-node-property-set-value](#om-elem-node-property-set-value-value-node-property) `(value node-property)`
* [om-elem-link-set-path](#om-elem-link-set-path-path-link) `(path link)`
* [om-elem-link-set-type](#om-elem-link-set-type-type-link) `(type link)`
* [om-elem-timestamp-set-time](#om-elem-timestamp-set-time-time-timestamp) `(time timestamp)`
* [om-elem-timestamp-set-time-end](#om-elem-timestamp-set-time-end-time-timestamp) `(time timestamp)`
* [om-elem-timestamp-set-type](#om-elem-timestamp-set-type-type-timestamp) `(type timestamp)`

### Element shifter functions


shift shit

* [om-elem-shift-property](#om-elem-shift-property-prop-n-elem) `(prop n elem)`
* [om-elem-headline-shift-priority](#om-elem-headline-shift-priority-shift-headline) `(shift headline)`
* [om-elem-timestamp-shift-time-start](#om-elem-timestamp-shift-time-start-unit-value-timestamp) `(unit value timestamp)`
* [om-elem-timestamp-shift-time-end](#om-elem-timestamp-shift-time-end-unit-value-timestamp) `(unit value timestamp)`
* [om-elem-timestamp-shift-time](#om-elem-timestamp-shift-time-unit-value-timestamp) `(unit value timestamp)`

### Element toggle functions.


Toggle shit

* [om-elem-toggle-property](#om-elem-toggle-property-prop-elem) `(prop elem)`
* [om-elem-headline-toggle-commented](#om-elem-headline-toggle-commented-headline) `(headline)`
* [om-elem-item-toggle-checkbox](#om-elem-item-toggle-checkbox-item) `(item)`
* [om-elem-timestamp-toggle-active](#om-elem-timestamp-toggle-active-timestamp) `(timestamp)`

### Element parsers


parse shit

* [om-elem-parse-object-at](#om-elem-parse-object-at-point-optional-type) `(point &optional type)`
* [om-elem-parse-element-at](#om-elem-parse-element-at-point-optional-type) `(point &optional type)`
* [om-elem-parse-headline-at](#om-elem-parse-headline-at-point) `(point)`
* [om-elem-parse-subtree-at](#om-elem-parse-subtree-at-point) `(point)`
* [om-elem-parse-item-at](#om-elem-parse-item-at-point) `(point)`
* [om-elem-parse-table-row-at](#om-elem-parse-table-row-at-point) `(point)`
* [om-elem-parse-section-at](#om-elem-parse-section-at-point) `(point)`


## Printing functions


print shit

#### om-elem-to-string `(elem)`

Return `elem` as an interpreted string without text properties.

```el
(om-elem-to-string '(bold (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
				"text"))
 ;; => "*text*"

(om-elem-to-string '(bold (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
				"text"))
 ;; => "*text*   "

(om-elem-to-string nil)
 ;; => nil

```

#### om-elem-to-trimmed-string `(elem)`

Like [`om-elem-to-string`](#om-elem-to-string-elem) but strip whitespace when returning `elem`.

```el
(om-elem-to-trimmed-string '(bold (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
					"text"))
 ;; => "*text*"

(om-elem-to-trimmed-string '(bold (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
					"text"))
 ;; => "*text*"

(om-elem-to-trimmed-string nil)
 ;; => nil

```


## Object Builders


build shit

#### om-elem-build-code `(value &key post-blank)`

Build a code object from `value`.

```el
(->> (om-elem-build-code "text")
     (om-elem-to-trimmed-string))
 ;; => "~text~"

```

#### om-elem-build-inline-babel-call `(call &key post-blank arguments inside-header end-header)`

Build an inline-babel-call element for `name`.
Optionally provide `args`, inside header args `inside`, and end header
args `end`.

```el
(->> (om-elem-build-inline-babel-call "name")
     (om-elem-to-trimmed-string))
 ;; => "call_name()"

(->> (om-elem-build-inline-babel-call "name" :arguments "args")
     (om-elem-to-trimmed-string))
 ;; => "call_name(args)"

(->> (om-elem-build-inline-babel-call "name" :inside-header "in")
     (om-elem-to-trimmed-string))
 ;; => "call_name[in]()"

(->> (om-elem-build-inline-babel-call "name" :end-header "end")
     (om-elem-to-trimmed-string))
 ;; => "call_name()[end]"

```

#### om-elem-build-inline-src-block `(language value &key parameters post-blank)`

Build an inline-src-block object with `language` and `value`.
Optionally provide `parameters`.

```el
(->> (om-elem-build-inline-src-block "lang" "value")
     (om-elem-to-trimmed-string))
 ;; => "src_lang{value}"

(->> (om-elem-build-inline-src-block "lang" "value" :parameters "params")
     (om-elem-to-trimmed-string))
 ;; => "src_lang[params]{value}"

```

#### om-elem-build-line-break `(&key post-blank)`

Build a line-break object.

```el
(->> (om-elem-build-line-break)
     (om-elem-to-trimmed-string))
 ;; => "\\\\"

```

#### om-elem-build-statistics-cookie `(&optional num dem &key post-blank)`

Build a statistics cookie object with `num` and `dem`.

```el
(->> (om-elem-build-statistics-cookie 50)
     (om-elem-to-trimmed-string))
 ;; => "[50%]"

(->> (om-elem-build-statistics-cookie 1 3)
     (om-elem-to-trimmed-string))
 ;; => "[1/3]"

```

#### om-elem-build-target `(value &key post-blank)`

Build a target object with `value`.

```el
(->> (om-elem-build-target "text")
     (om-elem-to-trimmed-string))
 ;; => "<<text>>"

```

#### om-elem-build-timestamp `(type time1 &optional time2 &key post-blank repeater-type repeater-unit repeater-value warning-type warning-unit warning-value)`

Build a timestamp object with `type`, `time1`, and optionally `time2`.
`type` is either 'inactive' or 'active', `time1` and `time2` are lists of
digits specifying the time formatted like '(year month day)' or
'(year month day hour minute)'. Supplying `time2` will create a
timestamp range.

```el
(->> (om-elem-build-timestamp 'inactive
			      '(2019 1 15))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-15 Tue]"

(->> (om-elem-build-timestamp 'inactive
			      '(2019 1 15 12 30))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-15 Tue 12:30]"

(->> (om-elem-build-timestamp 'inactive
			      '(2019 1 15)
			      '(2020 1 1))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-15 Tue]--[2020-01-01 Wed]"

```

#### om-elem-build-verbatim `(value &key post-blank)`

Build a verbatim object with `value`.

```el
(->> (om-elem-build-verbatim "text")
     (om-elem-to-trimmed-string))
 ;; => "=text="

```


## Recursive Object Builders

#### om-elem-build-bold `(&key post-blank &rest objs)`

Build a bold object containing `objs`.

```el
(->> (om-elem-build-bold "text")
     (om-elem-to-trimmed-string))
 ;; => "*text*"

```

#### om-elem-build-footnote-reference `(&key post-blank (label 1) &rest objs)`

Build a footnote reference object to `target`.
Optionally make the reference inline by setting `inline` to t.

```el
(->> (om-elem-build-footnote-reference)
     (om-elem-to-trimmed-string))
 ;; => "[fn:1]"

(->> (om-elem-build-footnote-reference :label "label")
     (om-elem-to-trimmed-string))
 ;; => "[fn:label]"

(->> (om-elem-build-footnote-reference :label "label" "content")
     (om-elem-to-trimmed-string))
 ;; => "[fn:label:content]"

```

#### om-elem-build-italic `(&key post-blank &rest objs)`

Build an italic object from `string`.

```el
(->> (om-elem-build-italic "text")
     (om-elem-to-trimmed-string))
 ;; => "/text/"

```

#### om-elem-build-link `(path &key post-blank (type fuzzy) &rest objs)`

Build a link object from `target` with `objs` as the description.

```el
(->> (om-elem-build-link "target")
     (om-elem-to-trimmed-string))
 ;; => "[[target]]"

(->> (om-elem-build-link "target" :type "file")
     (om-elem-to-trimmed-string))
 ;; => "[[file:target]]"

(->> (om-elem-build-link "target" "desc")
     (om-elem-to-trimmed-string))
 ;; => "[[target][desc]]"

```

#### om-elem-build-radio-target `(&key post-blank &rest objs)`

Build a radio target object from `string`.

```el
(->> (om-elem-build-radio-target "text")
     (om-elem-to-trimmed-string))
 ;; => "<<<text>>>"

```

#### om-elem-build-strike-through `(&key post-blank &rest objs)`

Build a strike-through object from `string`.

```el
(->> (om-elem-build-strike-through "text")
     (om-elem-to-trimmed-string))
 ;; => "+text+"

```

#### om-elem-build-superscript `(&key post-blank &rest objs)`

Build a superscript object from `string`.

```el
(->> (om-elem-build-superscript "text")
     (om-elem-to-trimmed-string))
 ;; => "^text"

```

#### om-elem-build-subscript `(&key post-blank &rest objs)`

Build a subscript object from `string`.

```el
(->> (om-elem-build-subscript "text")
     (om-elem-to-trimmed-string))
 ;; => "_text"

```

#### om-elem-build-table-cell `(&key post-blank &rest objs)`

Build a table cell object containing `text`.

```el
(->> (om-elem-build-table-cell "text")
     (om-elem-build-table-row)
     (om-elem-to-trimmed-string))
 ;; => "| text |"

```

#### om-elem-build-underline `(&key post-blank &rest objs)`

Build an underline object from `string`.

```el
(->> (om-elem-build-underline "text")
     (om-elem-to-trimmed-string))
 ;; => "_text_"

```


## Element Builders

#### om-elem-build-babel-call `(call &key post-blank arguments inside-header end-header)`

Build a babel-call element for `name`.
Optionally provide `args`, inside header args `inside`, and end header
args `end`.

```el
(->> (om-elem-build-babel-call "name")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name()"

(->> (om-elem-build-babel-call "name" :arguments "args")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name(args)"

(->> (om-elem-build-babel-call "name" :inside-header "inside")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name[inside]()"

(->> (om-elem-build-babel-call "name" :end-header "end")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name() end"

```

#### om-elem-build-clock `(time1 &optional time2 &key post-blank)`

Build a clock element with `time1`.
Optionally supply `time2` to create a closed clock.

```el
(->> (om-elem-build-clock '(2019 1 1 0 0))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 00:00]"

(->> (om-elem-build-clock '(2019 1 1 0 0)
			  '(2019 1 1 1 0))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-01 Tue 01:00] =>  1:00"

```

#### om-elem-build-comment `(value &key post-blank)`

Build a comment element with `value`.

```el
(->> (om-elem-build-comment "text")
     (om-elem-to-trimmed-string))
 ;; => "# text"

```

#### om-elem-build-comment-block `(value &key post-blank)`

Build a comment block element from `value`.

```el
(->> (om-elem-build-comment-block "text")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_COMMENT
 ;;    text
 ;;    #+END_COMMENT"

```

#### om-elem-build-diary-sexp `(value &key post-blank)`

Build a diary sexp element from `value`.
`value` is the part inside the '%%(value)' part of the sexp.

```el
(->> (om-elem-build-diary-sexp "text")
     (om-elem-to-trimmed-string))
 ;; => "%%(text)"

```

#### om-elem-build-example-block `(value &key post-blank switches preserve-indent)`

Build a example block element from `string`.

```el
(->> (om-elem-build-example-block "text")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXAMPLE
 ;;    text
 ;;    #+END_EXAMPLE"

(->> (om-elem-build-example-block "text" :switches "switches")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXAMPLE switches
 ;;    text
 ;;    #+END_EXAMPLE"

```

#### om-elem-build-export-block `(type value &key post-blank)`

Build an export-block element with `type` and `value`.

```el
(->> (om-elem-build-export-block "type" "value
")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXPORT type
 ;;    value
 ;;    #+END_EXPORT"

```

#### om-elem-build-fixed-width `(value &key post-blank)`

Build a fixed-width element from `string`.

```el
(->> (om-elem-build-fixed-width "text")
     (om-elem-to-trimmed-string))
 ;; => ": text"

```

#### om-elem-build-horizontal-rule `(&key post-blank)`

Build a horizontal-rule element.

```el
(->> (om-elem-build-horizontal-rule)
     (om-elem-to-trimmed-string))
 ;; => "-----"

```

#### om-elem-build-keyword `(key value &key post-blank)`

Build keyword element with keyword `key` and value `val`.

```el
(->> (om-elem-build-keyword "FILETAGS" "tmsu")
     (om-elem-to-trimmed-string))
 ;; => "#+FILETAGS: tmsu"

```

#### om-elem-build-latex-environment `(env text &key post-blank)`

Build a latex-environment element with environment `env` and `text`.

```el
(->> (om-elem-build-latex-environment "env" "text")
     (om-elem-to-trimmed-string))
 ;; => "\\begin{env}
 ;;    text
 ;;    \\end{env}"

```

#### om-elem-build-node-property `(key value &key post-blank)`

Build a node property object with `key` and `val`.

```el
(->> (om-elem-build-node-property "key" "val")
     (om-elem-to-trimmed-string))
 ;; => ":key:      val"

```

#### om-elem-build-planning `(&key closed scheduled deadline post-blank)`

Build planning element with `type` and `time`.

```el
(->> (om-elem-build-planning :closed '(2019 1 1))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

(->> (om-elem-build-planning :scheduled '(2019 1 1))
     (om-elem-to-trimmed-string))
 ;; => "SCHEDULED: [2019-01-01 Tue]"

(->> (om-elem-build-planning :deadline '(2019 1 1))
     (om-elem-to-trimmed-string))
 ;; => "DEADLINE: [2019-01-01 Tue]"

```

#### om-elem-build-src-block `(value &key language switches parameters preserve-indent post-blank)`



```el
(->> (om-elem-build-src-block "body")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC
 ;;      body
 ;;    #+END_SRC"

(->> (om-elem-build-src-block "body" :language "emacs-lisp")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC emacs-lisp
 ;;      body
 ;;    #+END_SRC"

(->> (om-elem-build-src-block "body" :switches "switches")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC switches
 ;;      body
 ;;    #+END_SRC"

(->> (om-elem-build-src-block "body" :parameters "params")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC params
 ;;      body
 ;;    #+END_SRC"

```

#### om-elem-build-table-row-hline `(&key post-blank)`



```el
(->> (om-elem-build-table (om-elem-build-table-row (om-elem-build-table-cell "text"))
			  (om-elem-build-table-row-hline))
     (om-elem-to-trimmed-string))
 ;; => "| text |
 ;;    |------|"

```


## Container Element Builders


## Greater Element Builders


Build shit

#### om-elem-build-center-block `(&key post-blank &rest elems)`

Build a center block greater element with `elems` as contents.

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-build-center-block)
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_CENTER
 ;;    text
 ;;    #+END_CENTER"

```

#### om-elem-build-drawer `(drawer-name &key post-blank &rest elems)`

Create drawer greater element with `name` and `elems` as contents.

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-build-drawer "NAME")
     (om-elem-to-trimmed-string))
 ;; => ":NAME:
 ;;    text
 ;;    :END:"

```

#### om-elem-build-footnote-definition `(label &key post-blank &rest elems)`

Build a footnote-definition greater element for `label`.
Optionally provide `elems` as contents.

```el
(->> (om-elem-build-paragraph "footnote contents")
     (om-elem-build-footnote-definition "label")
     (om-elem-to-trimmed-string))
 ;; => "[fn:label] footnote contents"

```

#### om-elem-build-headline `(&key title (level 1) post-blank pre-blank todo-keyword tags priority footnote-section-p commentedp archivedp &rest elems)`

Build a headline.

```el
(->> (om-elem-build-headline)
     (om-elem-to-trimmed-string))
 ;; => "*"

(->> (om-elem-build-headline :title "dummy")
     (om-elem-to-trimmed-string))
 ;; => "* dummy"

(->> (om-elem-build-headline :title "dummy" :level 3)
     (om-elem-to-trimmed-string))
 ;; => "*** dummy"

(->> (om-elem-build-headline :title "dummy" :todo-keyword "DONE")
     (om-elem-to-trimmed-string))
 ;; => "* DONE dummy"

(->> (om-elem-build-headline :title "dummy" :priority 65)
     (om-elem-to-trimmed-string))
 ;; => "* [#A] dummy"

(->> (om-elem-build-headline :title "dummy" :footnote-section-p t)
     (om-elem-to-trimmed-string))
 ;; => "* Footnotes"

(->> (om-elem-build-headline :title "dummy" :commentedp t)
     (om-elem-to-trimmed-string))
 ;; => "* COMMENT dummy"

```

#### om-elem-build-item `(&key (pre-blank 0) post-blank (bullet '-) checkbox tag counter &rest elems)`

Build a plain-list greater element with `elems` as contents.

```el
(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item)
     (om-elem-to-trimmed-string))
 ;; => "- item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :bullet 1)
     (om-elem-to-trimmed-string))
 ;; => "1. item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :checkbox 'on)
     (om-elem-to-trimmed-string))
 ;; => "- [X] item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :tag "tmsu")
     (om-elem-to-trimmed-string))
 ;; => "- tmsu :: item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :counter 10)
     (om-elem-to-trimmed-string))
 ;; => "- [@10] item contents"

```

#### om-elem-build-plain-list `(&key post-blank &rest elems)`

Build a plain-list greater element with `elems` as contents.

```el
(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item)
     (om-elem-build-plain-list)
     (om-elem-to-trimmed-string))
 ;; => "- item contents"

```

#### om-elem-build-property-drawer `(&key post-blank &rest elems)`

Build a property-drawer greater element with `elems` as contents.

```el
(->> (om-elem-build-node-property "key" "val")
     (om-elem-build-property-drawer)
     (om-elem-to-trimmed-string))
 ;; => ":PROPERTIES:
 ;;    :key:      val
 ;;    :END:"

```

#### om-elem-build-quote-block `(&key post-blank &rest elems)`

Build a quote-block greater element with `elems` as contents.

```el
(->> (om-elem-build-paragraph "quoted stuff")
     (om-elem-build-quote-block)
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_QUOTE
 ;;    quoted stuff
 ;;    #+END_QUOTE"

```

#### om-elem-build-section `(&key post-blank &rest elems)`

Build a section grater element with `elems` as contents.

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-build-section)
     (om-elem-to-trimmed-string))
 ;; => "text"

```

#### om-elem-build-table `(&key tblfm post-blank &rest elems)`

Build a section grater element with `elems` as contents.

```el
(->> (om-elem-build-table-cell "cell")
     (om-elem-build-table-row)
     (om-elem-build-table)
     (om-elem-to-trimmed-string))
 ;; => "| cell |"

```


## Element matching functions


match shit

#### om-elem-find `(queries elem)`

docstring

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** COMMENT headline three
; ** headline four

;; Use a symbol to match a type, in this case all  headlines.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '(headline))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** TODO headline two" "** COMMENT headline three" "** headline four")

;; Use integers specify the index to return. Negative  integers count from the end. Out of range integers  return nil
(->> (om-elem-parse-this-subtree)
     (om-elem-find '(1))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '(-1))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** headline four")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '(3))
     (--map (om-elem-to-trimmed-string it)))
 ;; => nil

;; Use a two-membered list with an operator and an  integer to match a range of indices. Allowed  operators are <, >, <=, and and >=.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '((> 0)))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three" "** headline four")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '((>= 1)))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three" "** headline four")

;; Use a plist to match based on an elements properties.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '((:todo-keyword "TODO")))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** TODO headline two")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '((:todo-keyword nil)))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three" "** headline four")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '((:todo-keyword "DONE")))
     (--map (om-elem-to-trimmed-string it)))
 ;; => nil

;; Given the following contents:
; * headline one
; this is *text1* of *text2*
; ** headline two
; here is more *text3*
; *** headline three
; and here is even more *text4* and *text5*
; **** headline 4

;; Specify multiple levels of matching using multiple  queries.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '(section paragraph bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*text1*" "*text2*")

;; Use the keyword :any as a wildcard to match any  element at a particular level.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '(:any :any bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*text1*" "*text2*")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '(section paragraph :any))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("this is" "*text1*" "of" "*text2*" "")

(->> (om-elem-parse-this-subtree)
     (om-elem-find '(:any bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => nil

;; Use the keyword :many to match one or more levels  of any element.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '(:many bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*text1*" "*text2*" "*text3*" "*text4*" "*text5*")

;; Use the keyword :many! to match one or more levels,  except unlike :many do not match within any elements  that have already matched.
(->> (om-elem-parse-this-subtree)
     (om-elem-find '(headline :many! headline))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*** headline three
 ;;    and here is even more *text4* and *text5*
 ;;    **** headline 4")

```

#### om-elem-find-first `(queries elem)`

Find first object in `elem` matching `queries`.
The rules for `queries` are the same as [`om-elem-find`](#om-elem-find-queries-elem)

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** COMMENT headline three
; ** headline four

;; Find the first subheadline
(->> (om-elem-parse-this-subtree)
     (om-elem-find-first '(headline))
     (om-elem-to-trimmed-string))
 ;; => "** TODO headline two"

```

#### om-elem-find-last `(queries elem)`

Find last object in `elem` matching `queries`.
The rules for `queries` are the same as [`om-elem-find`](#om-elem-find-queries-elem)

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** COMMENT headline three
; ** headline four

;; Find the last subheadline
(->> (om-elem-parse-this-subtree)
     (om-elem-find-last '(headline))
     (om-elem-to-trimmed-string))
 ;; => "** headline four"

```

#### om-elem-delete `(queries elem)`

Remove matching targets from contents of `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el
;; Given the following contents:
; * headline one
; ** headline two
; ** headline three
; ** headline four

;; Selectively delete headlines
(->> (om-elem-parse-this-subtree)
     (om-elem-delete '(headline))
     (om-elem-to-trimmed-string))
 ;; => "* headline one"

(->> (om-elem-parse-this-subtree)
     (om-elem-delete-first '(headline))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;;    ** headline three
 ;;    ** headline four"

(->> (om-elem-parse-this-subtree)
     (om-elem-delete-last '(headline))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;;    ** headline two
 ;;    ** headline three"

```

#### om-elem-extract `(queries elem)`

Remove matching targets from contents of `elem`.
Return cons cell where the car is a list of all removed targets
and the cdr is the modified `elem` with targets removed.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-map `(queries fun elem)`

Apply `fun` to targets matching `queries` in the contents of `elem`.
`fun` is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** headline three
; ** headline four

;; Selectively mark headlines as DONE
(->> (om-elem-parse-this-subtree)
     (om-elem-map '(headline)
		  (lambda (it)
		    (om-elem-headline-set-todo "DONE" it)))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;;    ** DONE headline two
 ;;    ** DONE headline three
 ;;    ** DONE headline four"

(->> (om-elem-parse-this-subtree)
     (om-elem-map-first* '(headline)
			 (om-elem-headline-set-todo "DONE" it))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;;    ** DONE headline two
 ;;    ** headline three
 ;;    ** headline four"

(->> (om-elem-parse-this-subtree)
     (om-elem-map-last '(headline)
		       (-partial (function om-elem-headline-set-todo)
				 "DONE"))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;;    ** TODO headline two
 ;;    ** headline three
 ;;    ** DONE headline four"

```

#### om-elem-mapcat `(queries fun elem)`

Apply `fun` over `elem` and return modified `elem`.
`fun` takes an element/object as its only argument and returns
a list of elements/objects. Targets within `elem` are found that match
`queries`, `fun` is applied to each target, and the resulting list is
spliced in place of the original target (as opposed to [`om-elem-map`](#om-elem-map-queries-fun-elem)
which replaces the original target with a modified target).

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-replace `(queries rep elem)`

Replace matching targets in `elem` with `rep`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-insert-before `(queries elem* elem)`

Insert `elem`* before every target matched by `queries` in `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-insert-after `(queries elem* elem)`

Insert `elem`* after every target matched by `queries` in `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-insert-within `(queries index elem* elem)`

Insert new element `elem`* into the contents of `elem` at `index`.
Will insert into any target matched by `queries`. If `queries` is not
supplied, `elem`* will be inserted directly into the toplevel contents
of `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-splice-before `(queries elem* elem)`

Splice `elems`* before every target matched by `queries` in `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-splice-after `(queries elem* elem)`

Splice `elems`* after every target matched by `queries` in `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```

#### om-elem-splice-within `(queries index elem* elem)`

Insert list of `elems`* into the contents of `elem` at `index`.
Will insert into any target matched by `queries`. If `queries` is not
supplied, `elem`* will be inserted directly into the toplevel contents
of `elem`.

`queries` follows the same rules as [`om-elem-find`](#om-elem-find-queries-elem).

```el

```


## Element get functions


get shit

#### om-elem-headline-get-subheadlines `(headline)`

Return list of subheadlines for `headline` element or nil if none.

```el
;; Given the following contents:
; * headline 1
; sectional stuff
; ** headline 2
; ** headline 3

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-get-subheadlines)
     (-map (function om-elem-to-trimmed-string)))
 ;; => '("** headline 2" "** headline 3")

;; Given the following contents:
; * headline 1
; sectional stuff

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-get-subheadlines)
     (-map (function om-elem-to-trimmed-string)))
 ;; => nil

```

#### om-elem-headline-get-section `(headline)`

Return section for headline `headline` element or nil if none.

```el
;; Given the following contents:
; * headline 1
; sectional stuff
; ** headline 2
; ** headline 3

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-get-section)
     (om-elem-to-trimmed-string))
 ;; => "sectional stuff"

;; Given the following contents:
; * headline 1
; ** headline 2
; ** headline 3

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-get-section)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-headline-get-drawer `(name headline)`

Return first drawer with `name` in `headline` element or nil if none.

```el
;; Given the following contents:
; * headline 1
; :LOGBOOK:
; - random note
; :END:
; rest of the section
; ** headline 2

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-get-drawer "LOGBOOK")
     (om-elem-to-trimmed-string))
 ;; => ":LOGBOOK:
 ;;    - random note
 ;;    :END:"

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-get-drawer "OTHER")
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-item-get-sublist `(item)`

Return plain-list under `item` element or nil if none.

```el
;; Given the following contents:
; - one
;   - two
;   - three
; - four

(->> (om-elem-parse-this-item)
     (om-elem-item-get-sublist)
     (om-elem-to-trimmed-string))
 ;; => "- two
 ;;    - three"

;; Given the following contents:
; - one
; - two

(->> (om-elem-parse-this-item)
     (om-elem-item-get-sublist)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-item-get-paragraph `(item)`

Return paragraph under `item` element or nil if none.

```el
;; Given the following contents:
; - one

(->> (om-elem-parse-this-item)
     (om-elem-item-get-paragraph)
     (om-elem-to-trimmed-string))
 ;; => "one"

;; Given the following contents:
; - [ ] one

(->> (om-elem-parse-this-item)
     (om-elem-item-get-paragraph)
     (om-elem-to-trimmed-string))
 ;; => "one"

;; Given the following contents:
; - tmsu :: one

(->> (om-elem-parse-this-item)
     (om-elem-item-get-paragraph)
     (om-elem-to-trimmed-string))
 ;; => "one"

;; Given the following contents:
; - tmsu ::

(->> (om-elem-parse-this-item)
     (om-elem-item-get-paragraph)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-table-get-cell `(row column table)`

Return table-cell element at `row` and `column` indices in `table` element.
Hlines do not count toward row indices, and all indices are
zero-indexed.

```el
;; Given the following contents:
; | 1 | 2 | 3 |
; |---+---+---|
; | a | b | c |

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell 0 0)
     (om-elem-contents)
     (car))
 ;; => "1"

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell 1 0)
     (om-elem-contents)
     (car))
 ;; => "a"

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell 0 2)
     (om-elem-contents)
     (car))
 ;; => "3"

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell 0 3)
     (om-elem-contents)
     (car))
 ;; => nil

```

#### om-elem-timestamp-get-start `(timestamp)`

Return the start of `timestamp` element.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-start)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-start)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-start)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 00:00]"

```

#### om-elem-timestamp-get-end `(timestamp)`

Return the end of `timestamp` element or nil if not present.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-end)
     (om-elem-to-trimmed-string))
 ;; => nil

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-end)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-end)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]"

```


## Element predicate functions


pred shit

#### om-elem-is-empty-p `(elem)`

Return t if `elem` has no contents.

```el
;; Given the following contents:
; * dummy
filled with useless knowledge

(->> (om-elem-parse-this-headline)
     (om-elem-is-empty-p))
 ;; => nil

;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-is-empty-p))
 ;; => t

```

#### om-elem-property-is-nil-p `(prop elem)`

Return t if `prop` in `elem` is nil.

```el
;; Given the following contents:
; * TODO dummy

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-nil-p :todo-keyword))
 ;; => nil

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-nil-p :commentedp))
 ;; => t

```

#### om-elem-property-is-non-nil-p `(prop elem)`

Return t if `prop` in `elem` is not nil.

```el
;; Given the following contents:
; * TODO dummy

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-non-nil-p :todo-keyword))
 ;; => t

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-non-nil-p :commentedp))
 ;; => nil

```

#### om-elem-property-is-eq-p `(prop val elem)`

Return t if `prop` in `elem` is `eq` to `val`.

```el
;; Given the following contents:
; * [#A] dummy

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-eq-p :priority 65))
 ;; => t

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-eq-p :priority 66))
 ;; => nil

```

#### om-elem-property-is-equal-p `(prop val elem)`

Return t if `prop` in `elem` is `equal` to `val`.

```el
;; Given the following contents:
; * TODO dummy

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-equal-p :todo-keyword "TODO"))
 ;; => t

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-equal-p :todo-keyword "DONE"))
 ;; => nil

```

#### om-elem-property-is-predicate-p `(prop fun elem)`

Return t if `fun` applied to the value of `prop` in `elem` results not nil.
`fun` is a predicate function that takes one argument.

```el
;; Given the following contents:
; * this is a dummy

(->> (om-elem-parse-this-headline)
     (om-elem-property-is-predicate-p* :title (s-contains? "dummy" (car it))))
 ;; => t

```

#### om-elem-contains-point-p `(point elem)`

Return t if integer `point` is within the beginning and end of `elem`.

```el
;; Given the following contents:
; * headline 1
; * headline 2

;; The headline is parsed from 'point-min'
(->> (om-elem-parse-this-headline)
     (om-elem-contains-point-p (point-min)))
 ;; => t

(->> (om-elem-parse-this-headline)
     (om-elem-contains-point-p (point-max)))
 ;; => nil

```

#### om-elem-contents-contains-point-p `(point elem)`

Return t if integer `point` is within the beginning and end of `elem``s contents.

```el
;; Given the following contents:
; * this is a dummy
; filled with nonsense

(->> (om-elem-parse-this-headline)
     (om-elem-contents-contains-point-p (point-min)))
 ;; => nil

(->> (om-elem-parse-this-headline)
     (om-elem-contents-contains-point-p (point-max)))
 ;; => t

```

#### om-elem-is-type-p `(type elem)`

Return t if `elem``s type is `eq` to `type` (a symbol).

```el
;; Given the following contents:
; *ziltoid*

(->> (om-elem-parse-this-object)
     (om-elem-is-type-p 'bold))
 ;; => t

(->> (om-elem-parse-this-object)
     (om-elem-is-type-p 'italic))
 ;; => nil

```

#### om-elem-is-any-type-p `(types elem)`

Return t if `elem``s type is any in `types` (a list of symbols).

```el
;; Given the following contents:
; *ziltoid*

(->> (om-elem-parse-this-object)
     (om-elem-is-any-type-p '(bold)))
 ;; => t

(->> (om-elem-parse-this-object)
     (om-elem-is-any-type-p '(bold italic)))
 ;; => t

(->> (om-elem-parse-this-object)
     (om-elem-is-any-type-p '(italic)))
 ;; => nil

```

#### om-elem-clock-is-running-p `(clock)`

Return t if `clock` element is running (eg is open).

```el
;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]

(->> (om-elem-parse-this-element)
     (om-elem-clock-is-running-p))
 ;; => t

;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00

(->> (om-elem-parse-this-element)
     (om-elem-clock-is-running-p))
 ;; => nil

```

#### om-elem-headline-is-done-p `(headline)`

Return t if `headline` element has a `done` todo keyword.

```el
;; Given the following contents:
; * TODO darn

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-done-p))
 ;; => nil

;; Given the following contents:
; * DONE yay

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-done-p))
 ;; => t

```

#### om-elem-headline-is-scheduled-p `(headline)`

Return t if `headline` element is scheduled.

```el
;; Given the following contents:
; * lazy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-scheduled-p))
 ;; => nil

;; Given the following contents:
; * proactive
; SCHEDULED: [2019-01-01 Tue]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-scheduled-p))
 ;; => t

```

#### om-elem-headline-is-deadlined-p `(headline)`

Return t if `headline` element has a deadline.

```el
;; Given the following contents:
; * lazy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-deadlined-p))
 ;; => nil

;; Given the following contents:
; * proactive
; DEADLINE: [2019-01-01 Tue]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-deadlined-p))
 ;; => t

```

#### om-elem-headline-is-closed-p `(headline)`

Return t if `headline` element is closed.

```el
;; Given the following contents:
; * lazy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-closed-p))
 ;; => nil

;; Given the following contents:
; * proactive
; CLOSED: [2019-01-01 Tue]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-closed-p))
 ;; => t

```

#### om-elem-headline-is-archived-p `(headline)`

Return t if `headline` element is archived.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-archived-p))
 ;; => nil

;; Given the following contents:
; * dummy                                                             :ARCHIVE:

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-archived-p))
 ;; => t

```

#### om-elem-headline-is-commented-p `(headline)`

Return t if `headline` element is commented.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-commented-p))
 ;; => nil

;; Given the following contents:
; * COMMENT dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-commented-p))
 ;; => t

```

#### om-elem-headline-has-tag-p `(tag headline)`

Return t if `headline` element is tagged with `tag`.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-has-tag-p "tmsu"))
 ;; => nil

;; Given the following contents:
; * dummy                                                             :tmsu:

(->> (om-elem-parse-this-headline)
     (om-elem-headline-has-tag-p "tmsu"))
 ;; => t

```

#### om-elem-item-is-unchecked-p `(item)`

Return t if `item` element is unchecked.

```el
;; Given the following contents:
; - one
; - [ ] two
; - [X] three
; - [-] four

(->> (om-elem-parse-this-element)
     (om-elem-contents)
     (-map (function om-elem-item-is-unchecked-p)))
 ;; => '(nil t nil nil)

```

#### om-elem-item-is-checked-p `(item)`

Return t if `item` element is checked.

```el
;; Given the following contents:
; - one
; - [ ] two
; - [X] three
; - [-] four

(->> (om-elem-parse-this-element)
     (om-elem-contents)
     (-map (function om-elem-item-is-checked-p)))
 ;; => '(nil nil t nil)

```

#### om-elem-item-is-trans-p `(item)`

Return t if `item` element is transitional.

```el
;; Given the following contents:
; - one
; - [ ] two
; - [X] three
; - [-] four

(->> (om-elem-parse-this-element)
     (om-elem-contents)
     (-map (function om-elem-item-is-trans-p)))
 ;; => '(nil nil nil t)

```

#### om-elem-timestamp-is-active-p `(timestamp)`

Return t if `timestamp` elem is active.

```el
;; Given the following contents:
; <2019-01-01 Tue>

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-active-p))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-active-p))
 ;; => nil

```

#### om-elem-timestamp-is-inactive-p `(timestamp)`

Return t if `timestamp` elem is inactive.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-inactive-p))
 ;; => t

;; Given the following contents:
; <2019-01-01 Tue>

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-inactive-p))
 ;; => nil

```

#### om-elem-timestamp-is-ranged-p `(timestamp)`

Return t if `timestamp` elem is ranged.

```el
;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-ranged-p))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-ranged-p))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-ranged-p))
 ;; => nil

```


## Element setter functions


set shit

#### om-elem-headline-set-todo `(todo headline)`

Set the todo keyword of `headline` element to `todo`.

```el
;; Given the following contents:
; * TODO dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-todo "DONE")
     (om-elem-to-trimmed-string))
 ;; => "* DONE dummy"

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-todo nil)
     (om-elem-to-trimmed-string))
 ;; => "* dummy"

```

#### om-elem-headline-set-archived `(flag headline)`

Set the archived flag of `headline` element to `flag`.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-archived t)
     (om-elem-to-trimmed-string))
 ;; => "* dummy                                                             :ARCHIVE:"

;; Given the following contents:
; * dummy                                                             :ARCHIVE:

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-archived nil)
     (om-elem-to-trimmed-string))
 ;; => "* dummy"

```

#### om-elem-headline-set-commented `(flag headline)`

Set the commented flag of `headline` element to `flag`.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-commented t)
     (om-elem-to-trimmed-string))
 ;; => "* COMMENT dummy"

;; Given the following contents:
; * COMMENT dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-commented nil)
     (om-elem-to-trimmed-string))
 ;; => "* dummy"

```

#### om-elem-headline-set-priority `(priority headline)`

Set the priority of `headline` element to `priority`.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-priority 65)
     (om-elem-to-trimmed-string))
 ;; => "* [#A] dummy"

;; Given the following contents:
; * [#A] dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-priority nil)
     (om-elem-to-trimmed-string))
 ;; => "* dummy"

```

#### om-elem-headline-set-title `(title headline)`

Set the title of `headline` element to `title`.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-title "portishead")
     (om-elem-to-trimmed-string))
 ;; => "* portishead"

(->> (om-elem-parse-this-headline)
     (om-elem-headline-set-title nil)
     (om-elem-to-trimmed-string))
 ;; => "*"

```

#### om-elem-item-set-checkbox `(state item)`

Set the checkbox of `item` element to `state`.
`state` is one of 'on', 'off', 'trans'. Setting to nil removes the
checkbox.

```el
;; Given the following contents:
; - [ ] one

(->> (om-elem-parse-this-item)
     (om-elem-item-set-checkbox 'on)
     (om-elem-to-trimmed-string))
 ;; => "- [X] one"

(->> (om-elem-parse-this-item)
     (om-elem-item-set-checkbox nil)
     (om-elem-to-trimmed-string))
 ;; => "- one"

```

#### om-elem-item-set-bullet `(bullet item)`

Set the bullet of `item` element to `bullet`.
`bullet` is either '-' or '+' or an integer greater than zero.
Note that `org-element-item-interpreter` currently does not interpret
'+' bullets properly and will render these as '-'.

```el
;; Given the following contents:
; - one

(->> (om-elem-parse-this-item)
     (om-elem-item-set-bullet 1)
     (om-elem-to-trimmed-string))
 ;; => "1. one"

;; This is actually correct due to a bug
(->> (om-elem-parse-this-item)
     (om-elem-item-set-bullet '+)
     (om-elem-to-trimmed-string))
 ;; => "- one"

;; Given the following contents:
; 1. one

(->> (om-elem-parse-this-item)
     (om-elem-item-set-bullet '-)
     (om-elem-to-trimmed-string))
 ;; => "- one"

```

#### om-elem-item-set-tag `(tag item)`

Set the tag of `item` element to `tag` where `tag` is a string or nil.

```el
;; Given the following contents:
; - one

(->> (om-elem-parse-this-item)
     (om-elem-item-set-tag "tmsu")
     (om-elem-to-trimmed-string))
 ;; => "- tmsu :: one"

;; Given the following contents:
; - tmsu :: one

(->> (om-elem-parse-this-item)
     (om-elem-item-set-tag nil)
     (om-elem-to-trimmed-string))
 ;; => "- one"

```

#### om-elem-plain-list-set-type `(type plain-list)`

Set the type of `plain-list` greater element to `type`.
`type` is '-', '+', or 'ordered'.

```el
;; Given the following contents:
; - [ ] one
; - [X] two

(->> (om-elem-parse-this-element)
     (om-elem-plain-list-set-type 'ordered)
     (om-elem-to-trimmed-string))
 ;; => "1. [ ] one
 ;;    2. [X] two"

;; Given the following contents:
; 1. [ ] one
; 2. [X] two

(->> (om-elem-parse-this-element)
     (om-elem-plain-list-set-type '-)
     (om-elem-to-trimmed-string))
 ;; => "- [ ] one
 ;;    - [X] two"

```

#### om-elem-node-property-set-key `(key node-property)`

Set the key of `node-property` element to `key` (a string).

```el
;; Given the following contents:
; * dummy
; :PROPERTIES:
; :key:      value
; :END:

(->> (om-elem-parse-this-headline)
     (om-elem-map* '(section property-drawer node-property)
		   (om-elem-node-property-set-key "lock" it))
     (om-elem-to-trimmed-string))
 ;; => "* dummy
 ;;    :PROPERTIES:
 ;;    :lock:     value
 ;;    :END:"

```

#### om-elem-node-property-set-value `(value node-property)`

Set the value of `node-property` element to `value` (a string).

```el
;; Given the following contents:
; * dummy
; :PROPERTIES:
; :key:      value
; :END:

(->> (om-elem-parse-this-headline)
     (om-elem-map* '(section property-drawer node-property)
		   (om-elem-node-property-set-value "lock" it))
     (om-elem-to-trimmed-string))
 ;; => "* dummy
 ;;    :PROPERTIES:
 ;;    :key:      lock
 ;;    :END:"

```

#### om-elem-link-set-path `(path link)`

Set the path of `link` element to `path` (a string).

```el
;; Given the following contents:
; [[eldorado][gold]]

(->> (om-elem-parse-this-object)
     (om-elem-link-set-path "404")
     (om-elem-to-trimmed-string))
 ;; => "[[404][gold]]"

;; Given the following contents:
; [[file:eldorado][gold]]

(->> (om-elem-parse-this-object)
     (om-elem-link-set-path "404")
     (om-elem-to-trimmed-string))
 ;; => "[[file:404][gold]]"

```

#### om-elem-link-set-type `(type link)`

Set the type of `link` element to `type` (a symbol).
Setting `type` to nil will result in a 'fuzzy' type link.

```el
;; Given the following contents:
; [[eldorado]]

(->> (om-elem-parse-this-object)
     (om-elem-link-set-type 'file)
     (om-elem-to-trimmed-string))
 ;; => "[[file:eldorado]]"

;; Given the following contents:
; [[file:eldorado]]

(->> (om-elem-parse-this-object)
     (om-elem-link-set-type nil)
     (om-elem-to-trimmed-string))
 ;; => "[[eldorado]]"

(->> (om-elem-parse-this-object)
     (om-elem-link-set-type 'fuzzy)
     (om-elem-to-trimmed-string))
 ;; => "[[eldorado]]"

```

#### om-elem-timestamp-set-time `(time timestamp)`

Set start time of `timestamp` element to `time`.
`time` is a list like '(year month day)' or '(year month day hour min)'.

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Set a different time.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-time '(2019 1 2))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]"

;; Set a different time with different precision.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-time '(2019 1 1 10 0))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 10:00]"

```

#### om-elem-timestamp-set-time-end `(time timestamp)`

Set end time of `timestamp` element to `time`.
`time` is a list like '(year month day)' or '(year month day hour min)'.
This will also change the type to (un)ranged as appropriate.

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Add the end time
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-time-end '(2019 1 2))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Remove the end time
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-time-end nil)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### om-elem-timestamp-set-type `(type timestamp)`

Set type of `timestamp` element to `type`.
`type` can be either 'active' or 'inactive'.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-type 'active)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-01 Tue>"

```


## Element shifter functions


shift shit

#### om-elem-shift-property `(prop n elem)`

Shift `prop` of `elem` by `n` where `n` is a positive or negative integer.

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Shift up
(->> (om-elem-parse-this-object)
     (om-elem-shift-property :year-start 1)
     (om-elem-to-trimmed-string))
 ;; => "[2020-01-01 Wed]"

;; Shift down
(->> (om-elem-parse-this-object)
     (om-elem-shift-property :year-start -1)
     (om-elem-to-trimmed-string))
 ;; => "[2018-01-01 Mon]"

;; Do nothing
(->> (om-elem-parse-this-object)
     (om-elem-shift-property :year-start 0)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### om-elem-headline-shift-priority `(shift headline)`

Shift the priority property of `headline` element by `shift`.
`shift` is a positive or negative integer.

```el
;; Given the following contents:
; * headline

(->> (om-elem-parse-this-headline)
     (om-elem-headline-shift-priority 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * [#A] headline

(->> (om-elem-parse-this-headline)
     (om-elem-headline-shift-priority -1)
     (om-elem-to-trimmed-string))
 ;; => "* [#B] headline"

(->> (om-elem-parse-this-headline)
     (om-elem-headline-shift-priority -2)
     (om-elem-to-trimmed-string))
 ;; => "* [#C] headline"

(->> (om-elem-parse-this-headline)
     (om-elem-headline-shift-priority 1)
     (om-elem-to-trimmed-string))
 ;; => "* [#C] headline"

```

#### om-elem-timestamp-shift-time-start `(unit value timestamp)`

Shift the `unit` of `timestamp` element start time by `value`.
`value` is a positive or negative integer and `unit` is one of 'minute',
'hour', 'day', 'month', or 'year'. Value will wrap around larger units
as needed; for instance, supplying 'minute' for `unit` and 60 for `value`
will increase the hour property by 1.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; Change each unit, and wrap around to the next unit as needed.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'minute
					 30)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:30]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'minute
					 60)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 13:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'hour
					 1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 13:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'day
					 1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'day
					 31)
     (om-elem-to-trimmed-string))
 ;; => "[2019-02-01 Fri 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'month
					 1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-02-01 Fri 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'month
					 13)
     (om-elem-to-trimmed-string))
 ;; => "[2020-02-01 Sat 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'year
					 1)
     (om-elem-to-trimmed-string))
 ;; => "[2020-01-01 Wed 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'year
					 0)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]

;; Do nothing to hour and minute
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'minute
					 30)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'hour
					 30)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Thu]

;; Change only the start if a range
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-start 'day
					 1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]--[2019-01-03 Thu]"

```

#### om-elem-timestamp-shift-time-end `(unit value timestamp)`

Shift the `unit` of `timestamp` element end time by `value`.
The behavior is analogous to [`om-elem-timestamp-shift-time-start`](#om-elem-timestamp-shift-time-start-unit-value-timestamp),
except that the timestamp will be unchanged if no ending time is
present.

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Do nothing if not a range.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-end 'day
				       1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Move only the second time if a range.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time-end 'day
				       1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-03 Thu]"

```

#### om-elem-timestamp-shift-time `(unit value timestamp)`

Shift the `unit` of `timestamp` element start and end time by `value`.
The behavior is analogous to [`om-elem-timestamp-shift-time-start`](#om-elem-timestamp-shift-time-start-unit-value-timestamp) for
both timestamp halves.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; Not a range, only change the start time.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time 'year
				   1)
     (om-elem-to-trimmed-string))
 ;; => "[2020-01-01 Wed 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Thu]

;; Change both start and end if a range
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-time 'day
				   1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]--[2019-01-04 Fri]"

```


## Element toggle functions.


Toggle shit

#### om-elem-toggle-property `(prop elem)`

Toggle the state of `prop` in `elem`.

```el
;; Given the following contents:
; * headline

;; Flip the property
(->> (om-elem-parse-this-headline)
     (om-elem-toggle-property :commentedp)
     (om-elem-to-trimmed-string))
 ;; => "* COMMENT headline"

;; Flip the property twice (do nothing)
(->> (om-elem-parse-this-headline)
     (om-elem-toggle-property :commentedp)
     (om-elem-toggle-property :commentedp)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

```

#### om-elem-headline-toggle-commented `(headline)`

Toggle the commented/uncommented state of `headline` element.

```el
;; Given the following contents:
; * headline

(->> (om-elem-parse-this-headline)
     (om-elem-headline-toggle-commented)
     (om-elem-to-trimmed-string))
 ;; => "* COMMENT headline"

(->> (om-elem-parse-this-headline)
     (om-elem-headline-toggle-commented)
     (om-elem-headline-toggle-commented)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

```

#### om-elem-item-toggle-checkbox `(item)`

Toggle the checked/unchecked state of `item` element.

```el
;; Given the following contents:
; - [ ] one

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- [X] one"

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- [ ] one"

;; Given the following contents:
; - [-] one

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- [-] one"

;; Given the following contents:
; - one

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- one"

```

#### om-elem-timestamp-toggle-active `(timestamp)`

Toggle the active/inactive type of `timestamp` element.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-01 Tue>"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-01 Tue>--<2019-01-02 Wed>"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```


## Element parsers


parse shit

#### om-elem-parse-object-at `(point &optional type)`

Return the object tree under `point` or nil if not on an object.

If `type` is supplied, only return nil if the object under point is
not of that type. `type` is a symbol from `org-element-all-objects`.

```el
;; Given the following contents:
; *text*

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'bold

;; Given the following contents:
; ~text~

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'code

;; Given the following contents:
; [fn:1:text]

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'footnote-reference

;; Given the following contents:
; call_name()

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'inline-babel-call

;; Given the following contents:
; src_emacs{}

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'inline-src-block

;; Given the following contents:
; /text/

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'italic

;; Given the following contents:
; \\

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'line-break

;; Given the following contents:
; [[path][desc]]

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'link

;; Given the following contents:
; {{{macro}}}

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'macro

;; Given the following contents:
; <<<text>>>

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'radio-target

;; Given the following contents:
; [1/2]

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'statistics-cookie

;; Given the following contents:
; +text+

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'strike-through

;; Given the following contents:
; a_b

(->> (om-elem-parse-object-at 3)
     (om-elem-type))
 ;; => 'subscript

;; Given the following contents:
; a^b

(->> (om-elem-parse-object-at 3)
     (om-elem-type))
 ;; => 'superscript

;; Given the following contents:
; | a |

(->> (om-elem-parse-object-at 2)
     (om-elem-type))
 ;; => 'table-cell

;; Given the following contents:
; <<text>>

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'target

;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'timestamp

;; Given the following contents:
; _text_

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'underline

;; Given the following contents:
; =text=

(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => 'verbatim

;; Given the following contents:
; - notme

;; Return nil when parsing an element
(->> (om-elem-parse-object-at 1)
     (om-elem-type))
 ;; => nil

```

#### om-elem-parse-element-at `(point &optional type)`

Return element immediately under `point`.
For a list of all possible return types refer to
`org-element-all-elements`; this will return everything in this list
except 'section' which is ambiguous when referring to a single point.
(see [`om-elem-parse-section-at`](#om-elem-parse-section-at-point)).

If `type` is supplied, only return nil if the object under point is
not of that type. `type` is a symbol from `org-element-all-elements`.
Furthermore, setting `type` to 'table-row' will prefer table-row
elements over table elements and likewise when setting `type` to 'item'
for plain-list elements vs item elements.

```el
;; Given the following contents:
; #+CALL: of_ktulu()

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'babel-call

;; Given the following contents:
; #+BEGIN_CENTER
; #+END_CENTER

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'center-block

;; Given the following contents:
; CLOCK: [2019-01-01 Tue]

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'clock

;; Given the following contents:
; # oops I looked

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'comment

;; Given the following contents:
; #+BEGIN_COMMENT
; oops I looked again
; #+END_COMMENT

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'comment-block

;; Given the following contents:
; %%(diary of a madman)

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'diary-sexp

;; Given the following contents:
; :DRAWER:
; - underwear
; - savings account
; :END:

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'drawer

;; Given the following contents:
; #+BEGIN countdown
; #+END

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'dynamic-block

;; Given the following contents:
; #+BEGIN_EXAMPLE
; #+END_EXAMPLE

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'example-block

;; Given the following contents:
; #+BEGIN_EXPORT latex
; #+END_EXPORT

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'export-block

;; Given the following contents:
; : mini mini mini

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'fixed-width

;; Given the following contents:
; [fn:1]

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'footnote-definition

;; Given the following contents:
; * murder, young girl killed
; * desperate shooting at echo's hill

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'headline

;; Given the following contents:
; -----

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'horizontal-rule

;; Given the following contents:
; - item

;; Explicitly ask for item instead of plain-list
(->> (om-elem-parse-element-at 1 'item)
     (om-elem-type))
 ;; => 'item

;; Given the following contents:
; #+QUOTE: unquote

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'keyword

;; Given the following contents:
; * headline
; :PROPERTIES:
; :key: val
; :END:

(->> (om-elem-parse-element-at 25)
     (om-elem-type))
 ;; => 'node-property

;; Given the following contents:
; Just for the record
; The weather today is slightly sarcastic with a good chance of
; A. Indifference and B. disinterest in what the critics say

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'paragraph

;; Given the following contents:
; - plain-list

;; Give the plain-list since we didn't explicitly ask for item
(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'plain-list

;; Given the following contents:
; * deadhead
; DEADLINE: [2019-01-01 Tue]

(->> (om-elem-parse-element-at 12)
     (om-elem-type))
 ;; => 'planning

;; Given the following contents:
; * headline
; :PROPERTIES:
; :END:

(->> (om-elem-parse-element-at 12)
     (om-elem-type))
 ;; => 'property-drawer

;; Given the following contents:
; #+BEGIN_QUOTE
; Oh glorious cheeseburger, we bow to thee
; The secrets of the universe are between the buns
; #+END_QUOTE

(->> (om-elem-parse-element-at 12)
     (om-elem-type))
 ;; => 'quote-block

;; Given the following contents:
; #+begin_dot dot.png
; #+end_dot

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'special-block

;; Given the following contents:
; #+BEGIN_SRC emacs
; (launch-missiles)
; #+END_SRC

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'src-block

;; Given the following contents:
; | R | A |
; | G | E |

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'table

(->> (om-elem-parse-element-at 1 'table-row)
     (om-elem-type))
 ;; => 'table-row

;; Given the following contents:
; #+BEGIN_VERSE
; #+END_VERSE

(->> (om-elem-parse-element-at 1)
     (om-elem-type))
 ;; => 'verse-block

```

#### om-elem-parse-headline-at `(point)`

Return element tree of the headline under `point` or nil if none.
`point` does not need to be on the headline itself. Only the headline
and its section will be returned (no subheadlines).

```el
;; Given the following contents:
; * headline

;; Return the headline itself
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; section crap

;; Return headline and section
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;;    section crap"

;; Return headline when point is in the section
(->> (om-elem-parse-headline-at 12)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;;    section crap"

;; Given the following contents:
; * headline
; section crap
; ** not parsed

;; Don't parse any subheadlines
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;;    section crap"

;; Given the following contents:
; nothing nowhere

;; Return nil if not under a headline
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-parse-subtree-at `(point)`

Return element tree of the headline under `point` or nil if none.
`point` does not need to be on the headline itself. Unlike
[`om-elem-parse-headline-at`](#om-elem-parse-headline-at-point), the returned tree will include
subheadlines.

```el
;; Given the following contents:
; * headline

;; Return the headline itself
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; section crap

;; Return headline and section
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;;    section crap"

;; Return headline when point is in the section
(->> (om-elem-parse-subtree-at 12)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;;    section crap"

;; Given the following contents:
; * headline
; section crap
; ** parsed

;; Return all the subheadlines
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;;    section crap
 ;;    ** parsed"

;; Given the following contents:
; nothing nowhere

;; Return nil if not under a headline
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-parse-item-at `(point)`

Return item element under `point` or nil if not found.
Unlike [`om-elem-parse-element-at`](#om-elem-parse-element-at-point-optional-type), this will return then item even if
`point` is not at the beginning of the line.

```el
;; Given the following contents:
; - item

;; Return the item itself
(->> (om-elem-parse-item-at 1)
     (om-elem-to-trimmed-string))
 ;; => "- item"

;; Also return the item when not at beginning of line
(->> (om-elem-parse-item-at 5)
     (om-elem-to-trimmed-string))
 ;; => "- item"

;; Given the following contents:
; - item
;   - item 2

;; Return item and its subitems
(->> (om-elem-parse-item-at 1)
     (om-elem-to-trimmed-string))
 ;; => "- item
 ;;      - item 2"

;; Given the following contents:
; * not item

;; Return nil if not an item
(->> (om-elem-parse-item-at 1)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-parse-table-row-at `(point)`

Return table-row element under `point` or nil if not found.

```el
;; Given the following contents:
; | bow | stroke |

;; Return the row itself
(->> (om-elem-parse-table-row-at 1)
     (om-elem-to-trimmed-string))
 ;; => "| bow | stroke |"

;; Also return the row when not at beginning of line
(->> (om-elem-parse-table-row-at 5)
     (om-elem-to-trimmed-string))
 ;; => "| bow | stroke |"

;; Given the following contents:
; - bow and arrow choke

;; Return nil if not a table-row
(->> (om-elem-parse-table-row-at 1)
     (om-elem-to-trimmed-string))
 ;; => nil

```

#### om-elem-parse-section-at `(point)`

Return tree of the current section under `point`.
If `point` is on or within a headline, return the section under that
headline. If `point` is before the first headline (if any), return
the section at the top of the org buffer.

```el
;; Given the following contents:
; over headline
; * headline
; under headline

;; Return the section above the headline
(->> (om-elem-parse-section-at 1)
     (om-elem-to-trimmed-string))
 ;; => "over headline"

;; Return the section under headline
(->> (om-elem-parse-section-at 25)
     (om-elem-to-trimmed-string))
 ;; => "under headline"

;; Given the following contents:
; * headline
; ** subheadline

;; Return nil if no section under headline
(->> (om-elem-parse-section-at 1)
     (om-elem-to-trimmed-string))
 ;; => nil

;; Given the following contents:
; 

;; Return nil if no section at all
(->> (om-elem-parse-section-at 1)
     (om-elem-to-trimmed-string))
 ;; => nil

```


0.0.1
