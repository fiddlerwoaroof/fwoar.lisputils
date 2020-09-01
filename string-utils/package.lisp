(in-package :cl-user)

(defpackage :fwoar.string-utils
  (:use :cl)
  (:export #:get-part-modifier
           #:split
           #:insert-at
           #:insert-where
           #:partition
           #:partition-if))

