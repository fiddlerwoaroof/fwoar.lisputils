(in-package :fwoar.lisputils)

(defmacro twice (&body body)
  `(progn ,@body
          ,@body))

(defmacro define-cluser-entrypoint ((&rest args) &body body)
  "Use the current package name to generate a <PACKAGE-NAME>.MAIN function in CL-USER.

This will throw an error if :FW.DEV is not in features as it's not
intended to be usd in distributed code."
  (unless (featurep :fw.dev)
    (error "will not define cluser entrypoint without :FW.DEV in *FEATURES*"))
  (let ((entrypoint-symbol (intern (format nil "~a.~a" (package-name *package*) '#:main)
                                   :cl-user)))
    `(progn (defun ,entrypoint-symbol ,args
              ,@body)
            (export ',entrypoint-symbol :cl-user))))
