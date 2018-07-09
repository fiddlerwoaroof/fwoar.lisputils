(uiop:define-package :fwoar.bin-parser
    (:use :cl)
  (:mix :fw.lu :alexandria :serapeum)
  (:export :extract
           :le->int
           :read-bytes))

(in-package :fwoar.bin-parser)

(defun read-bytes (n s)
  (with (seq (make-array n :element-type 'octet))
    (values seq
            (read-sequence seq s))))

(defun calculate-sizes (desc)
  (reduce #'+ desc
          :key #'cadr
          :initial-value 0))

(defun le->int (bytes)
  (cadr
   (reduce (op (destructuring-bind (count val) _1
                 (list (1+ count)
                       (+ val
                          (ash _2
                               (* count 8))))))
           bytes
           :initial-value (list 0 0))))

(defun get-extractable-bytes (desc &optional (bindings ()))
  (loop for ((name size . other) . rest) on (resolve-sizes desc bindings)
     until (symbolp size)
     collect (list* name size other) into extractable
     finally (return (values extractable
                             (append (unsplice
                                      (when (symbolp size)
                                        (list* name size other)))
                                     rest))))) 

(defun resolve-sizes (desc extant-bindings)
  (declare (optimize (debug 3)))
  (loop with bindings = (copy-seq extant-bindings)
     for (name size . rest) in desc
     for resolved = (when (symbolp size)
                      (cdr (assoc size bindings)))
     when resolved do (push (cons name resolved)
                            bindings)
     if resolved collect (list* name resolved rest) into new-desc
     else collect (list* name size rest) into new-desc
     finally (return (values new-desc
                             (remove-duplicates (append (mapcar (op (apply #'cons (subseq _ 0 2)))
                                                                new-desc)
                                                        bindings)
                                                :key 'car
                                                :from-end t)))))

(defun extract-bytes (desc bytes)
  (loop
     with cur-idx = 0
     for (name size . rest) in desc
     for next-seq = (subseq bytes cur-idx
                            (+ cur-idx size))
     collect (cons name (if rest
                            (funcall (car rest) next-seq)
                            next-seq))
     do (incf cur-idx size)))

(defun parse-struct (desc s)
  (let* ((struct-size (calculate-sizes desc))
         (bytes (read-bytes struct-size s)))
    (extract-bytes desc bytes)))

(defun make-zipfile-stream (fn)
  (open fn :element-type '(unsigned-byte 8)))


(defun extract (raw-desc s &optional bindings)
  (multiple-value-bind (desc remainder) (get-extractable-bytes raw-desc bindings)
    (let ((next-segment (parse-struct desc s)))
      (if remainder
          (append next-segment
                  (extract remainder s (append next-segment bindings)))
          next-segment))))
