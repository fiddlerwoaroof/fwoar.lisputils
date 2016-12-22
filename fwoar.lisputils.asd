;;;; fwoar.lisputils.asd

(asdf:defsystem #:fwoar.lisputils
  :description "Some utilities common to other libraries I'm writing"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "fwoar.lisputils")
	       (:file "hash-functions")
	       (:file "multiple-values")
	       (:file "clos-helpers")
	       (:file "vector-utils"))
  :depends-on (#:anaphora
	       #:alexandria
	       #:serapeum
	       #:cl-containers
	       #:iterate
	       #:plump
	       #:positional-lambda))

