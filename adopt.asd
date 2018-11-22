(asdf:defsystem :adopt
  :description "Simple, flexible UNIX-style option parsing."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/adopt/"

  :license "MIT"
  :version "0.0.1"

  :depends-on (:bobbin :split-sequence)

  :in-order-to ((asdf:test-op (asdf:test-op :adopt/test)))

  :serial t
  :components ((:file "package")
               (:module "src" :serial t
                :components ((:file "main")))))


(asdf:defsystem :adopt/test
  :description "Test suite for adopt."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:adopt :1am :losh)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "adopt.test:run-tests"))))
