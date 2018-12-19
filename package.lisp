(defpackage :adopt
  (:use :cl)
  (:export
    :parse-options
    :print-usage
    :define-interface

    :argv
    :exit

    :flip
    :oldest
    :newest
    :collect

    )
  (:shadow :collect :documentation :reduce))
