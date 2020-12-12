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

  This is not implemented for every Common Lisp implementation.  You can pass
  your own values to `parse-options` and `print-help` if it's not implemented
  for your particular Lisp.

  

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

### `DEFPARAMETERS` (macro)

    (DEFPARAMETERS PARAMETERS VALUES-FORM)

Convenience macro for `defparameter`ing multiple variables at once.

  `parameters` must be a list of special variable names suitable for giving to
  `defparameter`.

  `values-form` must be an expression that returns as many values as parameters
  in the parameter list.  Each parameter will be set to the corresponding value.

  This can be handy when using `make-boolean-options` to create two `option`s at
  once and assign them to special variables.

  Examples:

    (defparameters (*a* *b*) (truncate 100 3))
    (list *a* *b*)
    ; =>
    ; (33 1)

    (defparameters (*option-foo* *option-no-foo*)
      (make-boolean-options 'foo
        :help "Foo the widgets during the run."
        :help-no "Do not foo the widgets during the run (the default)."
        :long "foo"
        :short #f))

  

### `DISCARD-OPTION` (function)

    (DISCARD-OPTION CONDITION)

Invoke the `discard-option` restart properly.

  Example:

    (handler-bind ((unrecognized-option 'discard-option))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  

### `EXIT` (function)

    (EXIT &OPTIONAL (CODE 0))

Exit the program with status `code`.

  This is not implemented for every Common Lisp implementation.  You can write
  your own version of it and pass it to `print-help-and-exit` and
  `print-error-and-exit` if it's not implemented for your particular Lisp.

  

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

  

### `MAKE-BOOLEAN-OPTIONS` (function)

    (MAKE-BOOLEAN-OPTIONS NAME &KEY
                          (NAME-NO (INTERN (CONCATENATE 'STRING (STRING 'NO-) (STRING NAME)))) LONG
                          (LONG-NO (WHEN LONG (FORMAT NIL no-~A LONG))) SHORT
                          (SHORT-NO (WHEN SHORT (CHAR-UPCASE SHORT))) (RESULT-KEY NAME) HELP
                          HELP-NO MANUAL MANUAL-NO INITIAL-VALUE)

Create and return a pair of boolean options, suitable for use in an interface.

  This function reduces some of the boilerplate when creating two `option`s for
  boolean values, e.g. `--foo` and `--no-foo`.  It will try to guess at an
  appropriate name, long option, short option, and result key, but you can
  override them with the `…-no` keyword options as needed.

  The two options will be returned as two separate values — you can use
  `defparameters` to conveniently bind them to two separate variables if
  desired.

  Example:

    (defparameters (*option-debug* *option-no-debug*)
      (make-boolean-options 'debug
        :long "debug"
        :short #d
        :help "Enable the Lisp debugger."
        :help-no "Disable the Lisp debugger (the default)."))

    ;; is roughly equivalent to:

    (defparameter *option-debug*
      (make-option 'debug
        :long "debug"
        :short #d
        :help "Enable the Lisp debugger."
        :initial-value nil
        :reduce (constantly t))

    (defparameter *option-no-debug*
      (make-option 'no-debug
        :long "no-debug"
        :short #D
        :help "Disable the Lisp debugger (the default)."
        :reduce (constantly nil))

  

### `MAKE-GROUP` (function)

    (MAKE-GROUP NAME &KEY TITLE HELP MANUAL OPTIONS)

Create and return an option group, suitable for use in an interface.

  This function takes a number of arguments that define how the group is
  presented to the user:

  * `name` (**required**): a symbol naming the group.
  * `title` (optional): a title for the group for use in the help text.
  * `help` (optional): a short summary of this group of options for use in the help text.
  * `manual` (optional): used in place of `help` when rendering a man page.
  * `options` (**required**): the options to include in the group.

  See the full documentation for more information.

  

### `MAKE-INTERFACE` (function)

    (MAKE-INTERFACE &KEY NAME SUMMARY USAGE HELP MANUAL EXAMPLES CONTENTS)

Create and return a command line interface.

  This function takes a number of arguments that define how the interface is
  presented to the user:

  * `name` (**required**): a symbol naming the interface.
  * `summary` (**required**): a string of a concise, one-line summary of what the program does.
  * `usage` (**required**): a string of a UNIX-style usage summary, e.g. `"[OPTIONS] PATTERN [FILE...]"`.
  * `help` (**required**): a string of a longer description of the program.
  * `manual` (optional): a string to use in place of `help` when rendering a man page.
  * `examples` (optional): an alist of `(prose . command)` conses to render as a list of examples.
  * `contents` (optional): a list of options and groups.  Ungrouped options will be collected into a single top-level group.

  See the full documentation for more information.

  

### `MAKE-OPTION` (function)

    (MAKE-OPTION NAME &KEY LONG SHORT HELP MANUAL PARAMETER REDUCE
                 (INITIAL-VALUE NIL INITIAL-VALUE?) (RESULT-KEY NAME) (KEY #'IDENTITY)
                 (FINALLY #'IDENTITY))

Create and return an option, suitable for use in an interface.

  This function takes a number of arguments, some required, that define how the
  option interacts with the user.

  * `name` (**required**): a symbol naming the option.
  * `help` (**required**): a short string describing what the option does.
  * `result-key` (optional): a symbol to use as the key for this option in the hash table of results.
  * `long` (optional): a string for the long form of the option (e.g. `--foo`).
  * `short` (optional): a character for the short form of the option (e.g. `-f`).  At least one of `short` and `long` must be given.
  * `manual` (optional): a string to use in place of `help` when rendering a man page.
  * `parameter` (optional): a string.  If given, it will turn this option into a parameter-taking option (e.g. `--foo=bar`) and will be used as a placeholder
  in the help text.
  * `reduce` (**required**): a function designator that will be called every time the option is specified by the user.
  * `initial-value` (optional): a value to use as the initial value of the option.
  * `key` (optional): a function designator, only allowed for parameter-taking options, to be called on the values given by the user before they are passed along to the reducing function.  It will not be called on the initial value.
  * `finally` (optional): a function designator to be called on the final result after all parsing is complete. 

  The manner in which the reducer is called depends on whether the option takes a parameter:

  * For options that don't take parameters, it will be called with the old value.
  * For options that take parameters, it will be called with the old value and the value given by the user.

  See the full documentation for more information.

  

### `PARSE-OPTIONS` (function)

    (PARSE-OPTIONS INTERFACE &OPTIONAL (ARGUMENTS (REST (ARGV))))

Parse `arguments` according to `interface`.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash table of option keys to values.

  See the full documentation for more information.

  

### `PARSE-OPTIONS-OR-EXIT` (function)

    (PARSE-OPTIONS-OR-EXIT INTERFACE &OPTIONAL (ARGUMENTS (REST (ARGV))))

Parse `arguments` according to `interface`, exiting if any error occurs.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash table of option keys to values.

  If an error occurs while parsing the arguments, exits immediately as if with
  `adopt:print-error-and-exit`.

  See the full documentation for more information.

  

### `PRINT-ERROR-AND-EXIT` (function)

    (PRINT-ERROR-AND-EXIT ERROR &KEY (STREAM *ERROR-OUTPUT*) (EXIT-FUNCTION #'EXIT) (EXIT-CODE 1)
                          (PREFIX error: ))

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
                         (WIDTH 80) (OPTION-WIDTH 20) (INCLUDE-EXAMPLES T) (EXIT-FUNCTION #'EXIT)
                         (EXIT-CODE 0))

Print a pretty help document for `interface` to `stream` and exit.

  Handy for easily providing --help:

    (multiple-value-bind (arguments options) (parse-options *ui*)
      (when (gethash 'help options)
        (print-help-and-exit *ui*))
      (run arguments options))

  

### `PRINT-MANUAL` (function)

    (PRINT-MANUAL INTERFACE &KEY (STREAM *STANDARD-OUTPUT*) (MANUAL-SECTION 1))

Print a troff-formatted man page for `interface` to `stream`.

  Example:

    (with-open-file (manual "man/man1/foo.1"
                            :direction :output
                            :if-exists :supersede)
      (print-manual *ui* manual))

  

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

