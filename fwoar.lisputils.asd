;;;; fwoar.lisputils.asd
(in-package :asdf-user)
(asdf:defsystem #:fwoar.lisputils
  :description "Some utilities common to other libraries I'm writing"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :serial t
  :perform (test-op (o s)
                    (funcall (intern "TEST" :should-test)
                             :package :fwoar.string-utils))
  :depends-on (#:anaphora
               #:alexandria
               #:serapeum
               #:cl-containers
               #:iterate
               #-lispworks #:plump
               #:positional-lambda
               #-lispworks #:should-test)
  :components ((:file "package")
               (:file "fwoar.lisputils")
               (:file "hash-functions")
               (:file "multiple-values")
               (:file "clos-helpers")
               (:file "counter")
               (:file "vector-utils")
               (:file "string-utils/package")
               (:file "string-utils/string-utils")
               #-lispworks
               (:file "non-lispworks")
               #-lispworks
               (:file "string-utils/test")
               #-lispworks
               (:file "patmatch")
               (:file "glambda")))

