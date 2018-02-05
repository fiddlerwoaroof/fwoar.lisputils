;;;; package.lisp

(defpackage :fwoar.counter
  (:use :cl))

(defpackage :fwoar.anonymous-gf
  (:use :cl)
  (:export :glambda))

(defpackage :patmatch
  (:use :cl)
  (:export :let-pat*
           :handle-pattern))

(defpackage #:fwoar.lisputils
  (:use #:cl)
  (:nicknames #:fw.lu)
  (:shadow #:with)
  (:import-from :fwoar.anonymous-gf :glambda)
  (:import-from :patmatch :let-pat*)
  (:export #:lambda-if #:lambda-cond #:alambda #:rollup-list
           #:ensure-mapping #:alist-string-hash-table #:make-pairs
           #:copy-slots #:transform-alist #:%json-pair-transform
           #:%default-pair-transform #:default-when
           #:transform-result #:slots-to-pairs #:normalize-html
           #:destructuring-lambda #:let-each #:let-first #:let-second
           #:neither #:neither-null #:m-lambda #:sets #:defparameters
           #:setfs #:prog1-let #:prog1-bind #:if-let* #:with #:aconsf
           #:ensure-list #:pick #:vector-destructuring-bind #:with-accessors*
           #:skip-values #:limit-values #:substitute-values #:op #:pick/r
           #:pick-error #:twice #:glambda)) 
