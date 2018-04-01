;;;; fwoar.lisputils.asd

(asdf:defsystem #:fwoar.lisputils
  :description "Some utilities common to other libraries I'm writing"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :serial t
  :depends-on (#:anaphora
               #:alexandria
               #-lispworks #:serapeum
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
               (:file "string-utils/test")
               #-lispworks
               (:file "patmatch")
               (:file "glambda")))

