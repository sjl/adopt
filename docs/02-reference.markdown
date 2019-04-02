# API Reference

The following is a list of all user-facing parts of adopt.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `ADOPT`

### `ARGV` (function)

    (ARGV)

Return a list of the program name and command line arguments.

  This is not implemented for every Common Lisp implementation.  You can always
  pass your own values to `parse-options` and `print-help` if it's not
  implemented for your particular Lisp.

  

### `COLLECT` (function)

    (COLLECT LIST EL)

Append element `el` to the end of `list`.

  It is useful as a `:reduce` function when you want to collect all values given
  for an option.

  This is implemented as `(append list (list el))`.  It is not particularly
  fast.  If you can live with reversed output consider `(flip #'cons)`  instead.

  

### `DEFINE-STRING` (macro)

    (DEFINE-STRING VAR STRING &REST ARGS)

Convenience macro for `(defparameter ,var (format nil ,string ,@args))`.

### `DISCARD-OPTION` (function)

    (DISCARD-OPTION CONDITION)

Invoke the `discard-option` restart properly.

  Example:

    (handler-bind ((unrecognized-option 'discard-option))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  

### `EXIT` (function)

    (EXIT &OPTIONAL (CODE 0))

### `FIRST` (function)

    (FIRST OLD NEW)

Return `new` if `old` is `nil`, otherwise return `old`.

  It is useful as a `:reduce` function when you want to just keep the
  first-given value for an option.

  

### `FLIP` (function)

    (FLIP FUNCTION)

Return a function of two arguments X and Y that calls `function` with Y and X.

  Useful for wrapping existing functions that expect their arguments in the
  opposite order.

  Examples:

    (funcall #'cons 1 2)        ; => (1 . 2)
    (funcall (flip #'cons) 1 2) ; => (2 . 1)
    (reduce (flip #'cons) '(1 2 3) :initial-value nil)
    ; => (3 2 1)

  

### `LAST` (function)

    (LAST OLD NEW)

Return `new`.

  It is useful as a `:reduce` function when you want to just keep the last-given
  value for an option.

  

### `MAKE-GROUP` (function)

    (MAKE-GROUP NAME &KEY TITLE HELP MANUAL OPTIONS)

### `MAKE-INTERFACE` (function)

    (MAKE-INTERFACE &KEY NAME SUMMARY USAGE HELP MANUAL EXAMPLES CONTENTS)

### `MAKE-OPTION` (function)

    (MAKE-OPTION NAME &KEY LONG SHORT HELP MANUAL PARAMETER REDUCE (RESULT-KEY NAME)
                 (INITIAL-VALUE NIL INITIAL-VALUE?) (KEY #'IDENTITY) (FINALLY #'IDENTITY))

### `PARSE-OPTIONS` (function)

    (PARSE-OPTIONS INTERFACE &OPTIONAL (ARGUMENTS (REST (ARGV))))

Parse `arguments` according to `interface`.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash table of option keys to values.

  See the full documentation for more information.

  

### `PRINT-ERROR-AND-EXIT` (function)

    (PRINT-ERROR-AND-EXIT ERROR &KEY (STREAM *ERROR-OUTPUT*) (EXIT-CODE 1) (PREFIX error: ))

Print `prefix` and `error` to `stream` and exit.

  Example:

    (handler-case
        (multiple-value-bind (arguments options) (parse-options *ui*)
          (run arguments options))
      (unrecognized-option (c)
        (print-error-and-exit c)))

  

### `PRINT-HELP` (function)

    (PRINT-HELP INTERFACE &KEY (STREAM *STANDARD-OUTPUT*) (PROGRAM-NAME (CAR (ARGV))) (WIDTH 80)
                (OPTION-WIDTH 20) (INCLUDE-EXAMPLES T))

Print a pretty help document for `interface` to `stream`.

  `width` should be the total width (in characters) for line-wrapping purposes.
  Care will be taken to ensure lines are no longer than this, though some edge
  cases (extremely long short/long option names and parameters) may slip
  through.

  `option-width` should be the width of the column of short/long options (in
  characters).  If the short/long option help is shorter than this, the option's
  help string will start on the same line.  Otherwise the option's help string
  will start on the next line.

  The result will look something like:

    (print-help *program-interface* :width 60 :option-width 15)
    ; =>
    ; foo - do some things and meow
    ;
    ; USAGE: /bin/foo [options] FILES
    ;
    ; Foo is a program to frobulate some files, meowing as it
    ; happens.
    ;
    ; Options:
    ;   -v, --verbose    Output extra information.
    ;   -q, --quiet      Shut up.
    ;   --ignore FILE    Ignore FILE.  May be specified multiple
    ;                    times.
    ;   -n NAME, --name NAME
    ;                    Your name.  May be specified many times,
    ;                    last one wins.
    ;   -m, --meow       Meow.
    ;   0.........1.... option-width
    ; 0........1.........2.........3.........4.........5.........6

  

### `PRINT-HELP-AND-EXIT` (function)

    (PRINT-HELP-AND-EXIT INTERFACE &KEY (STREAM *STANDARD-OUTPUT*) (PROGRAM-NAME (CAR (ARGV)))
                         (WIDTH 80) (OPTION-WIDTH 20) (INCLUDE-EXAMPLES T) (EXIT-CODE 0))

Print a pretty help document for `interface` to `stream` and exit.

  Handy for easily providing --help:

    (multiple-value-bind (arguments options) (parse-options *ui*)
      (when (gethash 'help options)
        (print-help-and-exit *ui*))
      (run arguments options))
  

### `PRINT-MANUAL` (function)

    (PRINT-MANUAL INTERFACE &KEY (STREAM *STANDARD-OUTPUT*) (MANUAL-SECTION 1))

### `SUPPLY-NEW-VALUE` (function)

    (SUPPLY-NEW-VALUE CONDITION VALUE)

Invoke the `supply-new-value` restart properly.

  Example:

    (handler-bind
        ((unrecognized-option (alexandria:rcurry 'supply-new-value "--foo"))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  

### `TREAT-AS-ARGUMENT` (function)

    (TREAT-AS-ARGUMENT CONDITION)

Invoke the `treat-as-argument` restart properly.

  Example:

    (handler-bind ((unrecognized-option 'treat-as-argument))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  

### `UNRECOGNIZED-OPTION` (class)

#### Slot `PROBLEMATIC-OPTION`

* Allocation: `:INSTANCE`
* Initarg: `:PROBLEMATIC-OPTION`
* Accessor: `PROBLEMATIC-OPTION`

