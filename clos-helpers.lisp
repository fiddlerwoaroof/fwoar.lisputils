(in-package #:fwoar.lisputils)

(defmacro with-accessors* ((&rest accessors) object &body body)
  `(with-accessors ,(ensure-mapping accessors) ,object
     ,@body))

(defmacro keys ((op &rest args))
  (multiple-value-bind (positional keywords) (split-at  '&key args)
    `(,op
       ,@positional
       ,@(mapcan (lambda (_1)
                   (list (alexandria:make-keyword _1)
                         _1))
                 (cdr keywords)))))

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
  (let (constructor-type defclass-options)
    (mapc (lambda (option)
            (case (car option)
              ((:constructor-type) (setf constructor-type (cadr option)))
              (t (push option defclass-options))))
          options)
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
                                                   (list (intern (symbol-name initarg))
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
                                     (intern (concatenate 'string
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
                    ,@(nreverse defclass-options))
                  (defun ,name (,@required ,@(when optional
                                               (list* '&optional
                                                      (mapcar (lambda (it it-p)
                                                                `(,it nil ,it-p))
                                                              optional
                                                              passed-args))))
                    (declare (optimize (speed 3) (debug 1)))
                    ,(if optional
                         (let ((heads (reverse (inits optional))))
                           `(cond ,@(mapcar (lambda (it it-p)
                                              `(,it-p (fw.lu:new ',name ,@required ,@it)))
                                            heads
                                            passed-args)
                                  (t (fw.lu:new ',name ,@required))))
                         `(fw.lu:new ',name ,@required ,@optional)))))))))

(defun-ct %constructor-name (class)
  (let ((*print-case* (readtable-case *readtable*)))
    (format nil "~a-~a" '#:make class)))

(defmacro make-constructor (class &rest args)
  (destructuring-bind (class &optional (constructor-name (intern (%constructor-name class))))
      (ensure-list class)
    `(defgeneric ,constructor-name (,@args)
       (:method (,@args)
         (new ',class ,@args)))))

(defclass hashtable-slot-mixin ()
  ((%doc :reader hsm-doc :initarg :doc)))

(defmethod c2mop:slot-value-using-class :before (class (object hashtable-slot-mixin) slotd)
  (let ((slot-name (c2mop:slot-definition-name slotd)))
    (unless (or (eql slot-name '%doc)
                (c2mop:slot-boundp-using-class class object slotd))
      (let* ((doc (hsm-doc object))
             (doc-value (gethash (substitute #\_ #\-
                                             (string-downcase
                                              (symbol-name slot-name)))
                                 doc)))
        (setf (slot-value object slot-name) doc-value)))))


(defmacro define-printer (class &body options)
  (alexandria:with-gensyms (s)
    `(defmethod print-object ((,class ,class) ,s)
       (print-unreadable-object (,class ,s :type t :identity t)
         ,(destructuring-bind (name value) (car options)
            `(format ,s "~a: ~s" ,name (,value ,class)))
         ,@(loop for (name value) in (cdr options)
                 collect `(format ,s ", ~a: ~s" ,name (,value ,class)))))))
