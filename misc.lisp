(in-package :fwoar.lisputils)

(defmacro twice (&body body)
  `(progn ,@body
          ,@body))
