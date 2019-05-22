(defpackage :adopt
  (:use :cl)
  (:export
    :define-string

    :make-option
    :make-group
    :make-interface

    :parse-options

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

    )
  (:shadow :collect :reduce :first :last))
