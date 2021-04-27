(asdf:defsystem :adopt
  :description "Simple, flexible, UNIX-style option parsing."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/adopt/"

  :license "MIT"
  :version "1.1.1"

  :depends-on (:bobbin :split-sequence)

  :in-order-to ((asdf:test-op (asdf:test-op :adopt/test)))

  :serial t
  :components ((:module "src" :serial t
                :components ((:file "package")
                             (:file "main")))))


(asdf:defsystem :adopt/test
  :description "Test suite for adopt."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:adopt :1am)

  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "adopt.test:run-tests"))))
