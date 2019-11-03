(in-package :asdf-user)

(defsystem :fwoar-lisputils/patmatch/test 
    :description ""
    :author "Ed L <edward@elangley.org>"
    :license "MIT"
    :depends-on (#:fwoar-lisputils/patmatch
                 #:parachute
                 #:serapeum)
    :components ((:file "test-fixtures")
                 (:file "test" :depends-on ("test-fixtures"))))
