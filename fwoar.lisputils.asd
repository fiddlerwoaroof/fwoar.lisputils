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
               #:positional-lambda)
  :components ((:file "package")
               (:file "fwoar.lisputils")
               (:file "hash-functions")
               (:file "multiple-values")
               (:file "clos-helpers")
               (:file "counter")
               (:file "vector-utils")))

