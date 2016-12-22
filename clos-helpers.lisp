(in-package #:fwoar.lisputils)

(defmacro with-accessors* ((&rest accessors) object &body body)
  `(with-accessors ,(ensure-mapping accessors) ,object
     ,@body))
       
