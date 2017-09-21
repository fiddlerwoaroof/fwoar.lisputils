(in-package :patmatch)

(define-condition no-pattern (error)
  ((%pattern-obj :initarg :pattern-obj :reader pattern-obj)
   (%pattern-form :initarg :pattern-form :reader pattern-form)
   (%pattern-args :initarg :pattern-args :reader pattern-args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric handle-pattern (pattern form &rest args)
    (:method-combination append)

    (:method append ((pattern (eql 'cons)) form &rest args)
             (let ((val-sym (gensym "VAL")))
               (destructuring-bind (car cdr) args
                 `((,val-sym ,form)
                   (,car (car ,val-sym))
                   (,cdr (cdr ,val-sym))))))

    (:method append ((pattern (eql 'vector)) form &rest args)
             (let ((val-sym (gensym "VAL")))
               `((,val-sym ,form)
                 ,@ (loop for arg in args
                       for idx from 0
                       collect `(,arg (aref ,val-sym ,idx))))))

    (:method append ((pattern (eql 'hash-table)) form &rest args)
             (let* ((val-sym (gensym "VAL"))
                    (binding-forms (loop for (key sym) in args
                                      append `((,sym (gethash ',key ,val-sym))))))
               `((,val-sym ,form)
                 ,@binding-forms)))

    (:method append ((pattern symbol) form &rest args)
             (when (closer-mop:subclassp pattern 'standard-object)
               (apply #'handle-pattern
                      (closer-mop:class-prototype
                       (closer-mop:ensure-finalized (find-class pattern)))
                      form
                      args))))) 

(defmacro let-pat* ((&rest clauses) &body body)
  `(let* (,@ (loop for ((discriminator . args) val-form) in clauses
                append (apply 'handle-pattern discriminator val-form args)))
     ,@body))


(defpackage :patmatch/test
  (:use :cl :should-test)
  (:import-from :patmatch :let-pat*)
  (:export ))
(in-package :patmatch/test)

#|
(deftest let-pat*-handles-cons ()
    ""
  (should be eql
          2
          (let-pat* (((cons a b) '(2 . 3)))
            (declare (ignore b))
            a))
  (should be eql
          3
          (let-pat* (((cons a b) '(2 . 3)))
            (declare (ignore a))
            b)))

(deftest let-pat*-handles-vector ()
    ""
  (should be eql
          2
          (let-pat* (((vector a b) #(2 3)))
            (declare (ignore b))
            a))
  (should be eql
          3
          (let-pat* (((vector a b) #(2 3)))
            (declare (ignore a))
            b)))

(deftest let-pat*-handles-hash-table ()
    ""
  (should be eql
          2
          (let-pat* (((hash-table (:a a) (:b b)) (alexandria:plist-hash-table '(:a 2 :b 3))))
            (declare (ignore b))
            a))
  (should be eql
          3
          (let-pat* (((hash-table (:a a) (:b b)) (alexandria:plist-hash-table '(:a 2 :b 3))))
            (declare (ignore a))
            b)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass test-base ()
    ((a :initform 1)))
  (defclass test-sub1 (test-base)
    ())
  (defclass test-sub2 (test-base)
    ((b :initform 2)))
  (closer-mop:ensure-class 'test-base)
  (closer-mop:ensure-class 'test-sub1)
  (closer-mop:ensure-class 'test-sub2)
  )

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


(deftest let-pat*-handles-object-destructuring ()
    ""
  (should be eql
          1
          (let-pat* (((test-base :a a) (make-instance 'test-base)))
            a)))

(deftest let-pat*-handles-inheritance ()
    ""
 (should be eql
          1
          (let-pat* (((test-base :a a) (make-instance 'test-sub1)))
            a))

 (should be eql
          1
          (let-pat* (((test-sub1 :a a) (make-instance 'test-sub1)))
            a))

 (should be eql
         1
         (let-pat* (((test-sub2 :a a) (make-instance 'test-sub2)))
           a))

 (should be equal
         '(1 2)
         (let-pat* (((test-sub2 :a a :b b) (make-instance 'test-sub2)))
           (list a b))))

|#
