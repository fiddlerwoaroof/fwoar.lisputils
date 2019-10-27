(in-package #:fwoar.lisputils)

(defmacro vector-destructuring-bind ((&rest symbols) vector &body body)
  (let ((mappings (loop for symbol in symbols
                        for num from 0
                        collect (list num symbol))))
    (once-only (vector)
      `(symbol-macrolet ,(mapcar (destructuring-lambda ((num symbol))
                                   `(,symbol (aref ,vector ,num)))
                          mappings)
         ,@body))))


(defun v-assoc (item vector &key test test-not key)
  (loop for cur across vector
        for assoc-key = (car cur)
        for keyed = (if key (funcall key assoc-key) assoc-key)
        if (and test (funcall test item keyed)) do (return cur)
          else if (and test-not (not (funcall test item keyed))) do (return cur)
                 else when (eql item keyed) do (return cur)))

(defun v-first (vector)
  (elt vector 0))

(defun index-table (table &key (test 'eql) (key 'v-first))
  (declare (optimize (speed 0) (debug 3)))
  (check-type table (array * (* *)))
  (let ((table-index (cl-containers:make-container 'cl-containers:simple-associative-container :test test)))
    (loop for row-num from 0 to (1- (array-dimension table 0))
          for current-row = (make-array (array-dimension table 1)
                                        :displaced-to table
                                        :displaced-index-offset (apply #'array-row-major-index
                                                                       table (list row-num 0)))
          do (setf (cl-containers:item-at table-index (funcall key current-row)) row-num))
    table-index))

(defun join-tables (table1 table2 &key (test 'eql) (key1 'v-first) (key2 'v-first))
  (declare (optimize (speed 0) (debug 3)))
  (check-type table1 (array * (* *)))
  (check-type table2 (array * (* *)))
  (let ((table2-index (index-table table2 :test test :key key2)))
    (loop for row-num from 0 to (1- (array-dimension table1 0))
          for current-row = (make-array (array-dimension table1 1)
                                        :displaced-to table1
                                        :displaced-index-offset (apply #'array-row-major-index
                                                                       table1 (list row-num 0)))
          for dest-index = (cl-containers:item-at table2-index (funcall key1 current-row))
          when dest-index
            collect (let* ((to-row (make-array (array-dimension table2 1)
                                               :displaced-to table2
                                               :displaced-index-offset (apply #'array-row-major-index
                                                                              table2 (list dest-index 0)))))
                      (concatenate 'vector current-row to-row)))))

