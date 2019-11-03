(defpackage :patmatch/test
  (:use :cl)
  (:import-from :parachute
                #:true #:false #:fail #:is #:isnt #:is-values #:isnt-values
                #:of-type #:finish #:define-test)
  (:import-from :patmatch :let-pat*)
  (:export ))
(in-package :patmatch/test)

(defclass test-base ()
  ((a :initform 1)))

(defclass test-sub1 (test-base)
  ())

(defclass test-sub2 (test-base)
  ((b :initform 2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closer-mop:ensure-class 'test-base)
  (closer-mop:ensure-class 'test-sub1)
  (closer-mop:ensure-class 'test-sub2))

(defmethod patmatch:handle-pattern append ((pattern test-base) form &rest args)
  (alexandria:when-let ((arg (getf args :a)))
    (let ((val-sym (gensym "test-base-")))
      `((,val-sym ,form)
        ,@(serapeum:unsplice
           `(,arg (slot-value ,val-sym 'a)))))))

(defmethod patmatch:handle-pattern append ((pattern test-sub2) form &rest args)
  (alexandria:when-let ((arg (getf args :b)))
    (let ((val-sym (gensym "test-base-")))
      `((,val-sym ,form)
        ,@(serapeum:unsplice
           `(,arg (slot-value ,val-sym 'b)))))))
