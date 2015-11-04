;;;; fwoar.lisputils.asd

(asdf:defsystem #:fwoar.lisputils
  :description "Some utilities common to other libraries I'm writing"
  :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "fwoar.lisputils"))
  :depends-on (#:alexandria #:iterate #:plump))

