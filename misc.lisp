(in-package :fwoar.lisputils)

(defmacro twice (&body body)
  `(progn ,@body
          ,@body))

(defmacro define-cluser-entrypoint ((&rest args) &body body)
  "Use the current package name to generate a <PACKAGE-NAME>.MAIN function in CL-USER.

This will not do anything if :FW.DEV is not in features as it's only intended as a shortcut"
  (if (featurep :fw.dev)
      (let ((entrypoint-symbol (intern (format nil "~a.~a" (package-name *package*) '#:main)
                                       :cl-user)))
        `(progn (defun ,entrypoint-symbol ,args
                  ,@body)
                (export ',entrypoint-symbol :cl-user)))
      ()))
