;;;; fwoar.lisputils.asd

(asdf:defsystem #:fwoar.lisputils
  :description "Some utilities common to other libraries I'm writing"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :serial t
  :depends-on (#:anaphora
               #:alexandria
               #:serapeum
               #:cl-containers
               #:iterate
               #:plump
               #:positional-lambda
               #:should-test)
  :components ((:file "package")
               (:file "fwoar.lisputils")
               (:file "hash-functions")
               (:file "multiple-values")
               (:file "clos-helpers")
               (:file "counter")
               (:file "vector-utils")
               (:file "string-utils/package")
               (:file "string-utils/string-utils")
               (:file "string-utils/test")
               (:file "glambda")))

