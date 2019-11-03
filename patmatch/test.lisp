(in-package :patmatch/test)

(define-test let-pat*)

(define-test let-pat*-handles-cons
  :parent let-pat*
  (is = 2 (let-pat* (((cons a b) '(2 . 3)))
            (declare (ignore b))
            a))

  (is = 3 (let-pat* (((cons a b) '(2 . 3)))
            (declare (ignore a))
            b)))

(define-test let-pat*-handles-vector
  :parent let-pat*
  (is = 2 (let-pat* (((vector a b) #(2 3)))
            (declare (ignore b))
            a))
  (is = 3 (let-pat* (((vector a b) #(2 3)))
            (declare (ignore a))
            b)))

(define-test let-pat*-handles-hash-table
  :parent let-pat*
  (is = 2 (let-pat* (((hash-table (:a a) (:b b)) (alexandria:plist-hash-table '(:a 2 :b 3))))
            (declare (ignore b))
            a))

  (is = 3 (let-pat* (((hash-table (:a a) (:b b)) (alexandria:plist-hash-table '(:a 2 :b 3))))
            (declare (ignore a))
            b)))


(define-test let-pat*-handles-object-destructuring
  :parent let-pat*
  (is = 1 (let-pat* (((test-base :a a) (make-instance 'test-base)))
            a)))

(define-test let-pat*-handles-inheritance
  :parent let-pat*
  (is = 1 (let-pat* (((test-base :a a) (make-instance 'test-sub1)))
            a))

  (is = 1 (let-pat* (((test-sub1 :a a) (make-instance 'test-sub1)))
            a))

  (is = 1 (let-pat* (((test-sub2 :a a) (make-instance 'test-sub2)))
            a))

  (is equal '(1 2) (let-pat* (((test-sub2 :a a :b b) (make-instance 'test-sub2)))
                     (list a b))))
