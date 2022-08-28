(in-package :fwoar.string-utils)

(deftype array-length ()
  `(integer 0 ,array-dimension-limit))
(deftype array-index ()
  `(integer 0 ,(1- array-dimension-limit)))

(defun get-part-modifier (char string &optional count)
  (declare (optimize #+dev (debug 3) #-dev (speed 3))
           (type character char)
           (type string string))

  (flet ((count-splits (string)
           (declare (optimize (speed 3))
                    (type simple-string string))
           (if (= 0 (length string))
               1
           (do* ((x (the array-length 0) (1+ x))
                 (cur-char #1=(aref string x) #1#)
                 (result (the array-length 0) (if (char= cur-char char)
                                                  (1+ result)
                                                  result)))
                ((= x (1- (length string))) (1+ result))
                 (declare (type array-length result))))))
    (typecase string
      ((and string (not simple-string))
       (setf string (copy-seq string))))
    (unless count
      (setf count (count-splits string))))

  (let ((parts (make-array count :initial-element nil :element-type '(or string null)))
        (start-idx 0)
        (target-spot 0))
    (prog1 parts
      (dotimes (end-idx (length string))
        (when (typecase string
                (simple-string (char= (aref string end-idx) char))
                (string (char= (aref string end-idx) char)))
          (setf (aref parts target-spot)
                (make-array (- end-idx start-idx)
                            :displaced-to string :displaced-index-offset start-idx
                            :element-type 'character))
          (incf target-spot)
          (setf start-idx (1+ end-idx))
          (when (= target-spot (1- count))
            (return))))
      (when (<= start-idx (length string))
        (setf (aref parts target-spot)
              (make-array (- (length string) start-idx)
                          :displaced-to string :displaced-index-offset start-idx
                          :element-type 'character))))))

;; TODO: implement test
(defun %split-on-char (divider string &key count (test nil))
  (declare (optimize #+dev (debug 3) #+dev (speed 0) #+dev (space 0)
                     #-dev (speed 3) #-dev(space 2))
           (type (or null array-length) count)
           (type (or null function symbol) test)
           (type character divider)
           (type string string))
  (typecase string
    ((and string (not simple-string))
     (setf string (copy-seq string))))
  (check-type string simple-string)

  (flet ((count-splits (string)
           (declare (optimize #+dev (debug 3) #+dev (speed 0) #+dev (space 0)
                              #-dev (speed 3) #-dev(space 2))
                    (type simple-string string))
           (if (/= 0 (length string))
               (do* ((x (the array-length 0) (1+ x))
                     (cur-char #1=(aref string x) #1#)
                     (result (the array-length
                                  (if (char= cur-char divider)
                                      1
                                      0))
                             (if (char= cur-char divider)
                                 (1+ result)
                                 result)))
                    ((= x (1- (length string))) (1+ result))
                 (declare (type array-length result)))
               0))
         (find-pos (start-pos)
           (declare (optimize #+dev (debug 3) #+dev (speed 0) #+dev (space 0)
                              #-dev (speed 3) #-dev(space 2))
                    (type array-index start-pos))
           (etypecase test
             (function (position divider string :start start-pos :test test))
             (null (position divider string :start start-pos))
             (symbol (position divider string :start start-pos :test (symbol-function test))))))

    (unless count
      (setf count (count-splits string)))

    (check-type count array-length)
    (let ((parts (make-array (max (1+ count) 1) :fill-pointer 0))
          (start-pos (the fixnum 0)))
      (declare (dynamic-extent start-pos))
      (prog1 parts
        (loop 
          for end-pos = (find-pos start-pos)
          while end-pos 
          do
             (vector-push (subseq string start-pos end-pos) parts)
             (setf start-pos (1+ end-pos))
          while (< (length parts) count)
          finally
             (cond ((or (and end-pos count)
                        (< start-pos (length string)))
                    (vector-push (subseq string start-pos)
                                 parts))
                   ((not end-pos)
                    (vector-push "" parts))))))))

(defun %split-on-string (divider string &key count (test nil))
  (declare (optimize #+dev (debug 3) (speed 3))
           (type string divider string)
           (type (or null function symbol) test)
           (type (or null array-index) count))
  (flet ((%search (start-pos)
           (declare (optimize (speed 3))
                    (type array-index start-pos)
                    (inline))
           (typecase divider
             (simple-string (typecase string
                              (simple-string (search divider string :start2 start-pos))
                              (string (search divider string :start2 start-pos))))
             (string (search divider string :start2 start-pos))))
         (%search-with-test (start-pos test)
           (declare (optimize (speed 3))
                    (type array-index start-pos)
                    (type function test)
                    (inline))
           (typecase divider
             (simple-string (typecase string
                              (simple-string (search divider string :start2 start-pos :test test))
                              (string (search divider string :start2 start-pos :test test))))
             (string (search divider string :start2 start-pos :test test)))))
    (declare (dynamic-extent (function %search)
                             (function %search-with-test)))
    (let ((num-parts (the array-length 0))
          (pattern-length (the array-length (length divider)))
          (search-test (typecase test
                         (function test)
                         (null)
                         (symbol (symbol-function test))))
          (parts (make-array (if count count 100)
                             :adjustable t
                             :fill-pointer 0))
          (start-pos 0))
      (loop 
        for end-pos = (typecase search-test
                        (function (%search-with-test start-pos search-test))
                        (null (%search start-pos)))
        do
           (vector-push-extend (subseq string start-pos end-pos) parts) 
           (incf (the array-length num-parts))
        while end-pos
        do (setf start-pos (the array-length (+ pattern-length end-pos)))
        until (and count (>= num-parts count))
        finally
           (when (and count end-pos)
             (vector-push-extend (subseq string (+ pattern-length end-pos)) parts))
           (return parts)))))

(defun split (divider string &key count (test nil) (type nil type-p))
  (declare (optimize #+dev (debug 3) (speed 3) (space 3)))
  (unless test
    (setf test
          (typecase divider
            (string 'equal)
            (t 'eql))))
  (let ((result (etypecase divider
                  (character (%split-on-char divider string :count count :test test))
                  (string (if (= 1 (length divider))
                              (%split-on-char (aref divider 0) string :count count :test test)
                              (%split-on-string divider string :count count :test test))))))
    (if type-p
        (coerce result type)
        result)))
