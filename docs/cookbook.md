# org-ml cookbook

The following are a list of common use cases and formulationsfor `org-ml`. If a function is not available straight from theAPI it may be here.

## Adding created time

This will add a property called CREATED with a timestamp (which could be modified to hold the current time)..

```el
;; Given the following contents:
; * headine

(let ((ts (org-ml-to-string (org-ml-build-timestamp! '(2020 1 1 0 0)))))
  (->> (org-ml-parse-this-headline)
    (org-ml-headline-set-node-property "CREATED" ts)
    (org-ml-to-string)))
 ;; => "* headline
 ;      :PROPERTIES:
 ;      :CREATED:  [2020-01-01 Wed 00:00]
 ;      :END:"
```
