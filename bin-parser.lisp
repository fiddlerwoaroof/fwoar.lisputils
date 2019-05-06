(uiop:define-package :fwoar.bin-parser
    (:use :cl)
  (:mix :fw.lu :alexandria :serapeum)
  (:export :le->int :be->int
           :read-bytes :extract
           :extract-let :byte-array-to-hex-string
           :extract-high))

(in-package :fwoar.bin-parser)

;; stolen from ironclad
(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector)
           (type fixnum start)
           (type (or null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (check-type vector (vector (unsigned-byte 8)))
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
          for i from start below end
          for j from 0 below (* length 2) by 2
          do (let ((byte (aref vector i)))
               (declare (optimize (safety 0)))
               (setf (aref string j)
                     (aref hexdigits (ldb (byte 4 4) byte))
                     (aref string (1+ j))
                     (aref hexdigits (ldb (byte 4 0) byte))))
          finally (return string))))


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

(defun be->int (bytes)
  (cadr
   (reduce (op (destructuring-bind (count val) _1
                 (list (1+ count)
                       (+ val
                           (ash _2
                                (* count 8))))))
           (reverse bytes)
           :initial-value (list 0 0))))

(defun get-extractable-bytes (desc &optional (bindings ()))
  (loop for ((name size . other) . rest) on (resolve-sizes desc bindings)
        until (symbolp size)
        collect (list* name size other) into extractable
        finally (return (values extractable
                                (append (when (and size (symbolp size))
                                          (unsplice
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

(defun extract-high (s)
  (coerce (loop for next = (read-byte s)
                collect next
                until (< next 128))
          'vector))

(defun parse-struct (desc s)
  (let* ((struct-size (calculate-sizes desc))
         (bytes (read-bytes struct-size s)))
    (extract-bytes desc bytes)))

(defun make-zipfile-stream (fn)
  (open fn :element-type '(unsigned-byte 8)))

(defun extract (raw-desc s &optional bindings)
  (multiple-value-bind (desc remainder) (get-extractable-bytes raw-desc bindings)
    (let ((next-segment (parse-struct desc s)))
      (if (car remainder)
          (append next-segment
                  (extract remainder s (append next-segment bindings)))
          next-segment))))

(defmacro extract-let ((&rest bindings) parser s &body body)
  (labels ((collect-binding (binding-spec)
             (destructuring-bind (name target) binding-spec
               `(,name (cdr (assoc ,target it)))))
           (collect-bindings ()
             (mapcar #'collect-binding bindings)))
    `(let* ((it (extract ,parser ,s))
            ,@(collect-bindings))
       ,@body)))
