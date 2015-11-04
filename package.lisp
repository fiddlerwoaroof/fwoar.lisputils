;;;; package.lisp

(defpackage #:fwoar.lisputils
  (:use #:cl #:alexandria #:iterate)
  (:export #:lambda-if #:lambda-cond #:alambda #:rollup-list
           #:ensure-mapping #:alist-string-hash-table #:make-pairs
           #:copy-slots #:transform-alist #:%json-pair-transform
           #:%default-pair-transform #:default-when
           #:transform-result #:slots-to-pairs #:normalize-html))

