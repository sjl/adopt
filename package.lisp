(defpackage :adopt
  (:use :cl)
  (:export
    :define-interface
    :parse-options

    :print-usage
    :print-usage-and-exit
    :print-error-and-exit

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
