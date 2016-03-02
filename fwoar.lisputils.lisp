;;;; fwoar.lisputils.lisp

(in-package #:fwoar.lisputils)

(defmacro let-each ((&key (be '*)) &body forms)
  "Bind each element successively to the symbol specified via :be"
  `(let* ,(loop for form in forms
           collect (list be form))
     ,be))

(defmacro let-first ((&key (be '*)) &body forms)
  "Bind the result of the first form to the symbol specified via :be"
  `(let* ((,be ,(car forms)))
     ,@(loop for form in (cdr forms)
             collect (list be form))
     ,be))

(defmacro let-second ((&key (be '*)) &body forms)
  "Bind the result of the second form to the symbol specified via :be"
  `(progn
     ,(car forms)
     (let* ((,be ,(cadr forms)))
       ,@(loop for form in (cddr forms)
               collect (list be form))
       ,be)))

(defmacro lambda-if ((test &rest args) &body body)
  "Make a lambda that wraps an call to if"
  `(lambda ,args
     (if (,test ,@args)
       ,@body)))

(defmacro lambda-cond ((&rest args) &body body)
  "Make a lambda that wraps an call to cond"
  `(lambda ,args
     (cond
       ,@body)))

(defmacro alambda (&body body)
  `(lambda (anaphora:it)
     (declare (ignorable anaphora:it))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-mapping (list)
    "Take a list and make sure that it's parseable as a let-style binding.
     Very handy for certain sorts of macros."
    (let ((symbols->mappings (lambda-cond (x)
                               ((symbolp x) `(,x ,x))
                               ((null (cdr x)) `(,#1=(car x) ,#1#))
                               (t x))))
      (mapcar symbols->mappings list)))


    (defun rollup-list (list &optional body)
      (labels ((helper (list &optional accum start)
                 (tagbody
                   start
                   (cond
                     ((endp list) (return-from rollup-list accum))
                     (t (psetf accum  (cond
                                        ((null accum) (car list))
                                        (start `(,@(car list) ,@accum)) 
                                        (t `(,@(car list) ,accum)))
                               list (cdr list)
                               start nil)
                        ;; NOTE: REMEMBER! This call to #'GO is the "tail call"
                        (go start))))))
        (helper (reverse list) body t))))

(defmacro destructuring-lambda ((&rest args) &body body)
  "A lambda whose arguments can be lambda-lists to be destructured"
  (let* ((args-syms (mapcar (alambda (gensym "arg"))
                            args))
         (args (mapcar #'list args args-syms))
         (destructuring-expressions
           (rollup-list
               (loop for (arg arg-sym) in args
                     collect (if (consp arg)
                               `(destructuring-bind ,arg ,arg-sym)
                               `(let ((,arg ,arg-sym)))))
               body)))
    `(lambda ,args-syms
       ,destructuring-expressions)))


;;; CASES:::
#|
(fw.lu::destructuring-lambda ((slot slot-keyword . r))
                        (make-slot-spec slot slot-keyword))

(fw.lu::destructuring-lambda ((slot slot-keyword . r))
                        (declare (ignore r))
                        (make-slot-spec slot slot-keyword))

(fw.lu::destructuring-lambda ((slot slot-keyword . r) b c)
                        (make-slot-spec slot slot-keyword))

(fw.lu::destructuring-lambda ((slot slot-keyword . r) b)
                        (make-slot-spec slot slot-keyword))

(fw.lu::destructuring-lambda ((slot slot-keyword . r) b)
                        (declare (ignore r))
                        (make-slot-spec slot slot-keyword))
|#

(defun alist-string-hash-table (alist)
  "Make a hash table suitable for strings and other non-eql types
   from an association list"
  (alexandria:alist-hash-table alist :test #'equal))

(defmacro copy-slots (slots from to)
  "Given a list of slots specified as let-style bindings, copy them
   from one object to another."
  (once-only (from to)
    `(progn
       (setf ,@(apply #'append
                      (iterate (for (fro-slot to-slot) in (ensure-mapping slots))
                               (collect `((slot-value ,to ',to-slot) (slot-value ,from ',fro-slot))))))
       ,to)))

(defun transform-alist (function alist)
  "Run down an alist, applying a given function to each element"
  (mapcar (destructuring-lambda ((k . v)) (funcall function k v))
          alist))

(defun %json-pair-transform (k v)
  "Ugly hack to make jonathan work correctly with string values.
   TODO: move this elsewhere"
  (cons (make-keyword (string-downcase k))
        (typecase v
          (string (coerce v 'simple-string))
          (t v))))

(defun %default-pair-transform (k v)
  (cons (make-keyword (string-upcase k)) v))

(defmacro default-when (default test &body body)
  "return the default unless the test is true"
  (once-only (default)
    `(or (when ,test
           ,@body)
         ,default)))

(defmacro transform-result ((list-transform &optional (pair-transform #'identity)) &rest alist)
  "Transform an alist that results from some operation as a whole and, optionally, apply a
   transformation to each key-value pair."
  `(funcall ,list-transform (transform-alist ,pair-transform ,@alist)))

(defun make-pairs (symbols)
  ;TODO: does this duplicate ensure-mapping?
  (cons 'list
        (iterate (for (key value) in symbols)
                 (collect `(list* ,(symbol-name key) ,value)))))

(defmacro slots-to-pairs (obj (&rest slots))
  "Produce a alist from a set of object slots and their values"
  (once-only (obj)
    (let* ((slots (ensure-mapping slots))
           (bindings (iterate (for (slot v &key bind-from) in slots)
                              (collect (or bind-from slot)))))
      `(with-slots ,bindings ,obj
         ,(make-pairs slots)))))

(defun normalize-html (html)
  "Convert possibly bad HTML to sane HTML as best as possible."
  (let ((plump:*tag-dispatchers* plump:*html-tags*))
    (with-output-to-string (ss)
      (prog1 ss
        (map 'vector
           (lambda (x) (plump:serialize (plump:parse (plump:text x)) ss))
           html)))))

