;;;; fwoar-lisputils.asd
(in-package :asdf-user)

(defsystem #:fwoar-lisputils/string-utils
  :description "A binary parser"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :depends-on (#:should-test)
  :components ((:file "string-utils/package")
               (:file "string-utils/string-utils" :depends-on ("string-utils/package"))
               #-lispworks
               (:file "string-utils/test" :depends-on ("string-utils/string-utils"))))

(asdf:defsystem #:fwoar-lisputils
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
               #:fwoar-lisputils/string-utils
               #-lispworks #:plump
               #:positional-lambda
               #-lispworks #:should-test)
  :components ((:file "package")
               (:file "fwoar-lisputils")
               (:file "lexical-compare")
               (:file "hash-functions")
               (:file "multiple-values")
               (:file "clos-helpers")
               (:file "counter")
               (:file "vector-utils")
               #-lispworks
               (:file "non-lispworks")
               #-lispworks
               (:file "patmatch")
               (:file "glambda")))

(defsystem #:fwoar-lisputils/bin-parser
  :description "A binary parser"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :depends-on (:fwoar-lisputils
               :alexandria
               :serapeum)
  :components ((:file "bin-parser")))
