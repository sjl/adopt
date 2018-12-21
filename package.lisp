(defpackage :adopt
  (:use :cl)
  (:export
    :define-interface
    :define-string

    :make-interface
    :make-option

    :parse-options

    :print-usage
    :print-usage-and-exit
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
    :oldest
    :newest
    :collect


    )
  (:shadow :collect :documentation :reduce))
