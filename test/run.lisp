#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :adopt :silent t)
(asdf:test-system :adopt)
(quit)
