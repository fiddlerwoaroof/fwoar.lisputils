(in-package #:fwoar.lisputils)

(defmacro with-accessors* ((&rest accessors) object &body body)
  `(with-accessors ,(ensure-mapping accessors) ,object
     ,@body))

(defmacro new (class &rest initializer-syms)
  (multiple-value-bind (required optional rest) (parse-ordinary-lambda-list initializer-syms)
    (when optional
      (error "new doesn't handle optional arguments"))
    (if rest
        `(make-instance ,class
                        ,@(mapcan (lambda (_1)
                                    (list (alexandria:make-keyword _1)
                                          _1))
                                  required)
                        ,(make-keyword rest) ,rest)
        `(make-instance ,class
                        ,@(mapcan (lambda (_1)
                                    (list (alexandria:make-keyword _1)
                                          _1))
                                  initializer-syms)))))

(defun-ct %constructor-name (class)
  (format nil "~a-~a" '#:make class))

(defmacro make-constructor (class &rest args)
  (destructuring-bind (class &optional (constructor-name (intern (%constructor-name class))))
      (ensure-list class)
    `(defgeneric ,constructor-name (,@args)
       (:method (,@args)
         (new ',class ,@args)))))
