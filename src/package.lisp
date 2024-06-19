(defpackage :adopt
  (:use :cl)
  (:export
    :define-string
    :defparameters

    :make-option
    :make-boolean-options
    :make-group
    :make-interface

    :parse-options
    :parse-options-or-exit

    :print-help
    :print-help-and-exit
    :print-error-and-exit
    :print-manual

    :argv
    :exit

    :unrecognized-option
    :problematic-option
    :discard-option
    :treat-as-argument
    :supply-new-value

    :flip
    :collect
    :first
    :last

    :*error-string*

    )
  (:shadow :collect :reduce :first :last))
