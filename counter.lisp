(defpackage :fwoar.counter
  (:use :cl))

(in-package :fwoar.counter)

(defclass counter ()
  ((%counts :type hash-table :accessor item-counts)
   (%test :initarg :test :initform 'eql :accessor counter-test)
   (%key :initarg :key :initform 'identity :accessor counter-key)))

(defmethod initialize-instance :after ((counter counter) &rest initargs &key test)
  (declare (ignorable initargs))
  (setf (item-counts counter) (make-hash-table :test test)))

(defgeneric count-sequence (sequence &key test key)
  (:documentation "take a sequence, count it using test to compare elements and key to extract values from them"))

(defgeneric update-counts (counter sequence)
  (:documentation "given a sequence, update a counter"))

(defgeneric extract-count (counter item)
  (:documentation "Given a counter and an item, return the number of times that item has been counted."))

(defgeneric format-counts (counter stream)
  (:documentation "Given a counter and a stream, format the counter's counts to that stream"))

(defmethod extract-count ((counter counter) item)
  (gethash (funcall (counter-key counter)
                    item)
           (item-counts counter)
           0))

(defmethod count-sequence ((sequence string) &key (test 'eql) (key 'identity))
  (let ((result (make-instance 'counter :test test :key key)))
    (loop for c across sequence
          do (incf (gethash (funcall key c)
                            (item-counts result)
                            0))
          finally (return result))))

(defmethod format-counts ((counter counter) (stream stream))
  (let ((result '()))
    (maphash (lambda (key val)
               (push (list key val)
                     result))
             (item-counts counter))
    (format t "~:{~s: ~2d~%~}"
            (stable-sort result #'< :key #'cadr))))

(defmethod update-counts ((counter counter) sequence)
  (with-accessors ((item-counts item-counts) (test counter-test) (key counter-key)) counter
    (maphash (lambda (key value)
               (incf (gethash key item-counts 0)
                     value))
             (item-counts (count-sequence sequence :test test :key key)))))
