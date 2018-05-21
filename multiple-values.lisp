(in-package :fwoar.lisputils)

;; (multiple-value-call #'local-time:encode-timestamp
;;   (limit-values 7
;;                 (substitute-values 0 0 0 1
;;                                    (local-time:decode-timestamp
;;                                      (local-time:now)))))

(defmacro skip-values (n form)
  (let* ((gensyms (loop repeat n collect (gensym "V")))
	 (ignore-sym (gensym))
	 (value-limiter `(lambda (,@gensyms &rest ,ignore-sym)
			   (declare (ignore ,@gensyms))
			   (values-list ,ignore-sym))))
    `(multiple-value-call ,value-limiter ,form)))

(defmacro limit-values (n form)
  (let* ((gensyms (loop repeat n collect (gensym "V")))
	 (ignore-sym (gensym))
	 (value-limiter `(lambda (,@gensyms &rest ,ignore-sym)
			   (declare (ignore ,ignore-sym))
			   (values ,@gensyms))))
    `(multiple-value-call ,value-limiter ,form)))

(defmacro substitute-values (&rest forms)
  (let* ((call (car (last forms)))
	 (values (butlast forms))
	 (num-values (length values)))
    `(multiple-value-call #'values
       ,@values
       (skip-values ,num-values ,call))))

(defmacro transform-first-value ((transform form))
  `(let ((values (multiple-value-list ,form)))
     (values-list
      (list* (,transform (car values))
             (cdr values)))))

(in-package :fwoar.lisputils.shortcuts)

(defmacro ~> ((transform form))
  `(transform-first-value (,transform ,form)))
