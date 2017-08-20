(in-package :patmatch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric handle-pattern (pattern form &rest args)
    (:method-combination append)
    (:method append ((pattern cons) form &rest args)
             (let ((val-sym (gensym "VAL")))
               (destructuring-bind (car cdr) args
                 `((,val-sym ,form)
                   (,car (car ,val-sym))
                   (,cdr (cdr ,val-sym))))))

    (:method append ((pattern vector) form &rest args)
             (let ((val-sym (gensym "VAL")))
               `((,val-sym ,form)
                 ,@ (loop for arg in args
                       for idx from 0
                       collect `(,arg (aref ,val-sym ,idx))))))

    (:method append ((pattern hash-table) form &rest args)
             (let* ((val-sym (gensym "VAL"))
                    (binding-forms (loop for (key sym) in args
                                      append `((,sym (gethash ',key ,val-sym))))))
               `((,val-sym ,form)
                 ,@binding-forms)))

    (:method append ((pattern symbol) form &rest args)
             (apply #'handle-pattern
                    (closer-mop:class-prototype
                     (find-class pattern))
                    form
                    args)))) 

(defmacro let-pat* ((&rest clauses) &body body)
  `(let* (,@ (loop for ((discriminator . args) val-form) in clauses
                append (apply 'handle-pattern discriminator val-form args)))
     ,@body))


