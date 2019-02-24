(in-package #:fwoar.lisputils)

;; (pick '("a" "b" "c")
;;         { "a": { "b" : { "c" : 3 })
;; =>> 3 t 3 "c"

;; (pick '("a" "b" "c")
;;         { "a": { "b" : 3 } })
;; =>> nil nil 3 "b"

;; (pick '("a" "b" "c")
;;         { "a": 3 })
;; =>> nil nil 3 "a"

;; (pick '("a" "b" "c")
;;         {})
;; =>> nil nil {} nil
(define-condition deprecation-warning (warning)
  ((%symbol :initarg :symbol :reader dw-symbol :initform (error "symbol required"))
   (%type :initarg :type :reader dw-type :initform (error "type required"))
   (%replacement :initarg :replacement :reader dw-replacement :initform (error "replacement required"))))

(defmethod print-object ((x deprecation-warning) stream)
  (if *print-escape*
      (call-next-method)
      (let ((symbol (dw-symbol x))              
            (replacement (dw-replacement x)))
        (format stream "~(~a~) named ~a:~a is deprecated, use ~a:~a instead"
                (dw-type x)
                (package-name (symbol-package symbol))
                symbol
                (package-name (symbol-package replacement))
                replacement))))

(defun pick (keys h-t &optional default)
  (warn 'deprecation-warning :symbol 'pick :type :function :replacement 'dive)
  (dive keys h-t default))

(defun dive (keys h-t &optional default)
  "(PICK KEYS H-T) => (values result found last-value last-key)
   result if all keys found, otherwise (or default nil)
   last-value the value associated with last-key or H-T if no key matches
   last-key the last key to match, or nil
   found-p nil if all keys didn't match otherwise truthy"
  (let ((result default)
        (found nil)
        (last-value h-t)
        (last-key nil)
	      (matched-keys 0)
        (key-count 0))
    (dolist (key keys)
      (incf key-count)
      (if (hash-table-p last-value)
	        (multiple-value-bind (next-value next-found)
              (gethash key last-value)
	          (setf found next-found)
	          (when next-found
	            (incf matched-keys)
	            (setf last-key key
		                last-value next-value)))
	        (setf found nil)))
    (when (= matched-keys key-count)
      (setf result last-value))
    (values result found last-value last-key)))

(define-condition pick-error (error)
  ((%key :initarg :key :reader key)
   (%value :initarg :value :reader value :initform nil)))

;; Stolen from sbcl . . . 
(defun read-evaluated-form (&optional (prompt-control nil promptp)
                            &rest prompt-args)
  (apply #'format *query-io*
         (if promptp prompt-control "~&Type a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun pick/r (keys h-t &optional default)
  "Pick keys from h-t until fully traversed or no longer found.
   provide a restart for filling missing values. Raises pick-error
   if it tries to traverse a non-existent key or a non-hash-table
   value"

  (flet ((ensure-hash-table (v)
	   (unless (hash-table-p v)
	     (error 'type-error :expected-type 'hash-table :datum v))))
    (let ((cur-ht h-t))
      (dolist (key (butlast keys) (gethash (car (last keys)) cur-ht default))
	start
	(restart-case
	    (multiple-value-bind (value found) (gethash key cur-ht)
	      (cond ((null found) (error 'pick-error :key key))
		    ((hash-table-p value) (setf cur-ht value))
		    (t (error 'pick-error :key key :value value))))
	  (use-value (v)
	    :interactive read-evaluated-form
	    :report (lambda (stream) (format stream "Act as if the key was found"))
	    (ensure-hash-table v)
	    (setf cur-ht v))
	  (store-value (v)
	    :test (lambda (c) c (hash-table-p cur-ht))
	    :interactive read-evaluated-form
	    :report (lambda (stream) (format stream "Store a new value for ~S." key))
	    (ensure-hash-table v)
	    (setf (gethash key cur-ht) v)
	    (go start)))))))
