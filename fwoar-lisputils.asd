;;;; fwoar-lisputils.asd
(in-package :asdf-user)

(asdf:defsystem :fwoar-lisputils
    :description "Some utilities common to other libraries I'm writing"
    :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
    :license "MIT"
    :serial t
    :perform (test-op (o s)
                      (funcall (intern "TEST" :should-test)
                               :package :fwoar.string-utils))
    :depends-on (#:anaphora
                 #:alexandria
                 #:cl-containers
                 #:closer-mop
                 #:iterate
                 #:fwoar-lisputils/patmatch
                 #:fwoar-lisputils/string-utils
                 #:plump
                 #:positional-lambda
                 (:feature (:not (:or :ecl :abcl))
                           #:fwoar-lisputils/implementation-dependent))
    :components ((:file "package")
                 (:file "fwoar-lisputils")
                 (:file "clos-helpers")
                 (:file "walk")
                 (:file "hash-functions")
                 (:file "multiple-values")
                 (:file "restarts")
                 (:file "counter")
                 (:file "vector-utils")
                 (:file "html")
                 (:file "glambda")
                 (:file "misc")))

(defsystem :fwoar-lisputils/implementation-dependent
    :description "Utilities that don't work on every system"
    :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
    :license "MIT"
    :serial t
    :depends-on (#:serapeum)
    :components ((:file "lexical-compare" :if-feature (:not (:or :ecl :abcl)))))

(defsystem :fwoar-lisputils/patmatch 
    :description ""
    :author "Ed L <edward@elangley.org>"
    :license "MIT"
    :depends-on (#:alexandria
                 #:uiop)
    :serial t
    :components ((:module "patmatch"
                  :components ((:file "package")
                               (:file "patmatch" :depends-on ("package"))))))

(defsystem :fwoar-lisputils/patmatch/test 
    :description ""
    :author "Ed L <edward@elangley.org>"
    :license "MIT"
    :depends-on (:fwoar-lisputils/patmatch
                 :parachute)
    :serial t
    :components ((:module "patmatch"
                  :components ((:file "test")))))

(defsystem #:fwoar-lisputils/string-utils
    :description "A string splitter"
    :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
    :license "MIT"
    :depends-on ()
    :components ((:file "string-utils/package")
                 (:file "string-utils/split"
                  :depends-on ("string-utils/package"))
                 (:file "string-utils/string-utils"
                  :depends-on ("string-utils/package"))
                 #+(or)
                 (:file "string-utils/test" :depends-on ("string-utils/string-utils"))))

(asdf:defsystem #:fwoar-lisputils/swank-utils
    :description "Utilities for use with swank"
    :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
    :license "MIT"
    :serial t
    :perform (test-op (o s)
                      (funcall (intern "TEST" :should-test)
                               :package :fwoar.string-utils))
    :depends-on (#:fwoar-lisputils
                 #:yason
                 #:swank)
    :components ((:file "swank-utils")))

(defsystem #:fwoar-lisputils/bin-parser
    :description "A binary parser"
    :author "fiddlerwoaroof <fiddlerwoaroof@gmail.com"
    :license "MIT"
    :depends-on (:fwoar-lisputils
                 :alexandria
                 :serapeum)
    :components ((:file "bin-parser")))
