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

(defmacro defclass+ (name (&rest super) &body (direct-slots &rest options))
  (let* ((initargs (append (mapcan (lambda (class)
                                     (typecase class
                                       (cons (mapcar (lambda (it)
                                                       (list it nil))
                                                     (cadr class)))
                                       (t nil)))
                                   super)
                           (mapcan (lambda (slot)
                                     (alexandria:ensure-list
                                      (alexandria:when-let ((initarg (getf (cdr slot)
                                                                           :initarg)))
                                        (fw.lu:prog1-bind
                                            (it (list
                                                 (list (make-symbol (symbol-name initarg))
                                                       (eq :missing
                                                           (getf (cdr slot)
                                                                 :initform
                                                                 :missing)))))))))
                                   direct-slots))))
    (destructuring-bind (required optional)
        (loop for it in initargs
              if (second it) collect (first it) into required
                else collect (first it) into optional
              finally (return (list required
                                    optional)))
      (let ((passed-args (mapcar (lambda (it)
                                   (make-symbol (concatenate 'string
                                                             (symbol-name it)
                                                             "-P")))
                                 optional)))
        `(progn (defclass ,name
                    ,(mapcar (lambda (it)
                               (typecase it
                                 (cons (car it))
                                 (t it)))
                      super)
                  ,direct-slots
                  ,@options)
                (defun ,name (,@required ,@(when optional
                                             (list* '&optional
                                                    (mapcar (lambda (it it-p)
                                                              `(,it nil ,it-p))
                                                            optional
                                                            passed-args))))
                  ,(if optional
                       (let ((heads (reverse (inits optional))))
                         `(cond ,@(mapcar (lambda (it it-p)
                                            `(,it-p (fw.lu:new ',name ,@required ,@it)))
                                          heads
                                          passed-args)
                                (t (fw.lu:new ',name ,@required))))
                       `(fw.lu:new ',name ,@required ,@optional))))))))

(defun-ct %constructor-name (class)
  (format nil "~a-~a" '#:make class))

(defmacro make-constructor (class &rest args)
  (destructuring-bind (class &optional (constructor-name (intern (%constructor-name class))))
      (ensure-list class)
    `(defgeneric ,constructor-name (,@args)
       (:method (,@args)
         (new ',class ,@args)))))
