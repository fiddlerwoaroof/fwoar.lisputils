(in-package #:fwoar.lisputils)

(defmacro neither (&rest forms)
  `(not (or ,@forms)))

(defmacro neither-null (&rest forms)
  `(neither ,@(loop for form
                      in forms
                    collecting `(null ,form))))


(defmacro let-each ((&key (be '*)) &body forms)
  "Bind each element successively to the symbol specified via :be"
  `(let* ,(loop for form in forms
                collect (list be form))
     ,be))

(defmacro let-first ((&key (be '*)) bound &body forms)
  "Bind the result of the first form to the symbol specified via :be"
  `(let* ((,be ,bound))
     ,@forms
     ,be))

(defmacro let-second ((&key (be '*)) &body forms)
  "Bind the result of the second form to the symbol specified via :be"
  `(progn
     ,(car forms)
     (let* ((,be ,(cadr forms)))
       ,@(cddr forms)
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
  `(lambda (it)
     (declare (ignorable it))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-cons (cb cons)
    (cond
      ((null cons) '())
      ((consp (cdr cons)) (cons (funcall cb (car cons))
                                (map-cons cb (cdr cons))))
      (t (list (funcall cb (car cons))
               (funcall cb (cdr cons))))))

  (defun generate-declarations-for (sym ignored ignorable)
    (let ((ignores (list))
          (ignorables (list)))
      (map-cons (lambda (_1)
                  (cond ((member _1 ignorable)
                         (push _1 ignorables))
                        ((member _1 ignored)
                         (push _1 ignores))))
                (alexandria:ensure-cons sym))
      (if (or ignores ignorables)
          `((declare
             ,@(when ignores
                 `((ignore ,@ignores)))
             ,@(when ignorables
                 `((ignorable ,@ignorables)))))
          '())))

  (defun find-ignored-vars (body)
    (let ((possible-declarations (car body))
          (ignored-vars nil)
          (ignorable-vars nil))
      (if (and (consp possible-declarations)
               (eq (car possible-declarations) 'declare)
               (consp (cadr possible-declarations)))
          (let* ((declarations (cdr possible-declarations)))
            (setf ignored-vars (cdr (assoc 'ignore declarations))
                  ignorable-vars (cdr (assoc 'ignorable declarations))
                  body (cdr body))))
      (values ignored-vars
              ignorable-vars
              body)))

  (defun ensure-mapping (list &optional (key-fn 'identity))
    "Take a list and make sure that it's parseable as a let-style binding.
     Very handy for certain sorts of macros."
    (let ((symbols->mappings (lambda-cond (x)
                               ((symbolp x) `(,(funcall key-fn x) ,x))
                               ((null (cdr x)) `(,(funcall key-fn #1=(car x)) ,#1#))
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

(defmacro m-lambda (sym &rest args)
  (let ((arglist (loop for x in args
                       unless (member x (list '&optional '&key '&rest))
                         collect (ctypecase x
                                   (cons                  (car x))
                                   ((or symbol keyword string) x)))))
    `(lambda (,@args)
       (,sym ,@arglist))))

(defun get-ignored-vars (body)
  (let ((declarations (cdr (assoc 'declare body))))
    (values (cdr (assoc 'ignore declarations))
            (cdr (assoc 'ignorable declarations)))))

(defmacro destructuring-lambda ((&rest args) &body body)
  "A lambda whose arguments can be lambda-lists to be destructured"
  (multiple-value-bind (ignored ignorable body) (find-ignored-vars body)
    (let* ((args-syms (mapcar (lambda (_) (declare (ignore _)) (gensym "arg"))
                              args))
           (args (mapcar #'list args args-syms))
           (destructuring-expressions
             (rollup-list
              (loop for (arg arg-sym) in args
                    collect (if (consp arg)
                                `(destructuring-bind ,arg ,arg-sym
                                   (declare (ignore ,@ignored) (ignorable ,@ignorable)))
                                `(let ((,arg ,arg-sym))
                                   ,@(generate-declarations-for arg ignored ignorable))))
              body)))
      `(lambda ,args-syms
         ,destructuring-expressions))))


;;; CASES:::
#|
;; (fw.lu::destructuring-lambda ((slot slot-keyword . r))
;;   (make-slot-spec slot slot-keyword))
;;
;; (fw.lu::destructuring-lambda ((slot slot-keyword . r))
;;   (declare (ignore r))
;;   (make-slot-spec slot slot-keyword))
;;
;; (fw.lu::destructuring-lambda ((slot slot-keyword . r) b c)
;;   (make-slot-spec slot slot-keyword))
;;
;; (fw.lu::destructuring-lambda ((slot slot-keyword . r) b)
;;   (make-slot-spec slot slot-keyword))
;;
;; (fw.lu::destructuring-lambda ((slot slot-keyword . r) b)
;;   (declare (ignore r))
;;   (make-slot-spec slot slot-keyword))
|#

(defun alist-string-hash-table (alist)
  "Make a hash table suitable for strings and other non-eql types
   from an association list"
  (alexandria:alist-hash-table alist :test #'equal))

(defmacro copy-slots (slots from to)
  "Given a list of slots specified as let-style bindings, copy them
   from one object to another."
  (alexandria:once-only (from to)
    `(progn
       (setf ,@(apply #'append
                      (iterate:iterate
                        (iterate:for (fro-slot to-slot) iterate:in (ensure-mapping slots))
                        (iterate:collect `((slot-value ,to ',to-slot) (slot-value ,from ',fro-slot))))))
       ,to)))

(defun transform-alist (function alist)
  "Run down an alist, applying a given function to each element"
  (mapcar (destructuring-lambda ((k . v)) (funcall function k v))
          alist))

(defun %json-pair-transform (k v)
  "Ugly hack to make jonathan work correctly with string values.
   TODO: move this elsewhere"
  (cons (alexandria:make-keyword (string-downcase k))
        (typecase v
          (string (coerce v 'simple-string))
          (t v))))

(defun %default-pair-transform (k v)
  (cons (alexandria:make-keyword (string-upcase k)) v))

(defun find-nonoperator-symbols (form)
  (alexandria:flatten
   (remove-duplicates
    (typecase form
      (symbol (list form))
      (cons (append
             (when (consp (car form))
               (find-nonoperator-symbols (car form)))
             (typecase (cdr form)
               (symbol (list (cdr form)))
               (cons (loop for thing in (cdr form)
                           append (find-nonoperator-symbols thing))))))))))

(defmacro may ((op arg &rest r))
  (let ((cond (case op
                (cl:funcall (car r))
                (t arg))))
    (alexandria:once-only (arg)
      `(when ,cond
         (,op ,arg ,@r)))))

(defmacro str->stream ((op arg &rest r))
  (let ((string (case op
                  (cl:funcall (car r))
                  (t arg))))
    (alexandria:once-only (arg)
      (alexandria:with-gensyms (s)
        `(with-input-from-string (,s ,arg)
           (,op ,s ,@r))))))

(defmacro default-when (default test &body body)
  "return the default unless the test is true"
  (warn "default-when is deprecated, renamed to default-unless")
  (alexandria:once-only (default)
    `(or (when ,test
           ,@body)
         ,default)))

(defmacro default-unless (default test &body body)
  "return the default unless the test is true"
  (alexandria:once-only (default)
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
        (iterate:iterate
          (iterate:for (key value) in symbols)
          (iterate:collect `(list* ,(symbol-name key) ,value)))))

(defun inits (l)
  (serapeum:with-collector (c)
    (let ((its ()))
      (mapc (lambda (it)
              (push it its)
              (c (reverse its)))
            l))))

(defmacro closing ((op &rest args))
  (let ((stream-sym (gensym "STRING")))
    `(with-open-stream (,stream-sym ,(first args))
       (,op ,stream-sym ,@(cdr args)))))
#+fw.ignore
(progn
  (closing (read-line (open "/foo/bar"
                            :element-type '(unsigned-byte 8))
                      nil
                      :the-end))
  )

(defmacro slots-to-pairs (obj (&rest slots))
  (declare (optimize (debug 3)))
  "Produce a alist from a set of object slots and their values"
  (alexandria:once-only (obj)
    (let* ((slots (ensure-mapping slots))
           (bindings (iterate:iterate
                       (iterate:for (slot v &key bind-from) in slots)
                       (iterate:collect (or bind-from slot)))))
      `(with-slots ,bindings ,obj
         ,(make-pairs slots)))))

(defmacro setfs (&body body)
  "Make setf a bit nicer to use with paredit"
  (list* 'setf (apply #'append body)))

(defmacro prog2-let (first-form (&rest result-binding) &body body)
  "Execute a form, make a bunch of bindings and retern the bound values via prog1 after executing body"
  `(progn ,first-form
          (let (,@result-binding)
            (prog1 (list ,@(mapcar #'car result-binding))
              ,@body))))

;; TODO: use multiple values . . .
(defmacro prog1-let ((&rest result-binding) &body body)
  "Bind a bunch of symbols to values and return them via prog1"
  `(let (,@result-binding)
     (multiple-value-prog1 (values ,@(mapcar #'car result-binding))
       ,@body)))

(defmacro prog1-bind ((var val) &body body)
  `(let ((,var ,val))
     (prog1 ,var
       ,@body)))

(defmacro if-let* ((&rest bindings) &body (then-form &optional else-form))
  "Like if-let, but sets bindings sequentially.  Doesn't short-circuit."
  `(let* ,bindings
     (if (and ,@(mapcar #'car bindings))
         ,then-form
         ,else-form)))

(defmacro with ((var val) &body body)
  "A stripped down let for binding a single name"
  `(let ((,var ,val))
     ,@body))

(defun do-acons (alist key datum)
  (acons key datum alist))
(define-modify-macro aconsf (key datum) do-acons)


(defun do-adjoin (list item &rest r &key key test test-not)
  (declare (ignore key test test-not))
  (apply #'adjoin item list r))
(define-compiler-macro do-adjoin (list item &rest r)
  (alexandria:once-only (list item)
    `(adjoin ,item ,list ,@r)))
(define-modify-macro adjoinf (item &rest r)
  do-adjoin)

;;(defun ensure-list (val)
;;  (typecase val
;;    (list val)
;;    (t (list val))))


(defmacro defun-ct (name (&rest args) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defun ,name ,args
       ,@body)))

(defmacro retry-once ((flag-sym) &body body)
  `(let ((,flag-sym t))
     (tagbody
      start
        ,@body
        (when ,flag-sym
          (setf ,flag-sym nil)
          (go start)))))

(defun split-at (el list &key (test #'eql) (key nil))
  (if key
      (loop for it on list
            until (funcall test (funcall key (car it)) el)
            collect (car it) into head
            finally (return (values head it)))
      (loop for it on list
            until (funcall test (car it) el)
            collect (car it) into head
            finally (return (values head it)))))
