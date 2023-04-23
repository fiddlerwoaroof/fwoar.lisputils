(defpackage :fwoar-lisputils.package
  (:use :cl)
  (:export))
(in-package :fwoar-lisputils.package)

(defpackage :fwoar.counter
  (:use :cl))

(defpackage :fwoar.anonymous-gf
  (:use :cl)
  (:export #:glambda))

(defpackage :fwoar-lisputils.restarts
  (:use :cl)
  (:export #:with-retry #:with-retry* #:safely-invoke-restart))

(defpackage :fwoar.lisputils
  (:use #:cl #:alexandria)
  (:nicknames #:fw.lu)
  (:shadow #:with)
  (:export #:lambda-if :lambda-cond :alambda :rollup-list
           #:ensure-mapping :alist-string-hash-table :make-pairs
           #:copy-slots :transform-alist :%json-pair-transform
           #:%default-pair-transform :default-when :transform-result
           #:slots-to-pairs :normalize-html :destructuring-lambda
           #:let-each :let-first :let-second :neither :neither-null
           #:m-lambda :sets :defparameters :setfs :prog1-let
           #:prog1-bind :if-let* :with :aconsf :pick
           #:vector-destructuring-bind :with-accessors* :skip-values
           #:limit-values :substitute-values :pick/r :pick-error
           #:twice :default-unless :transform-first-value :may
           #:defun-ct :define-cluser-entrypoint :new :make-constructor
           #:dive :empty-hash-table-like :v-assoc :defclass+ :closing
           #:inits :retry-once :hashtable-slot-mixin :hsm-doc :adjoinf
           #:it :keys :split-at :define-printer))


(defpackage :fwoar.lisputils.shortcuts
  (:use :cl :fwoar.lisputils)
  (:nicknames #:fw.lu.t)
  (:export #:~>))
