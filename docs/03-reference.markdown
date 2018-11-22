# API Reference

The following is a list of all user-facing parts of adopt.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `ADOPT`

### `APPEND1` (function)

    (APPEND1 LIST EL)

Append element `el` to the end of `list`.

  This is implemented as `(append list (list el))`.  It is not particularly
  fast.

  It is useful as a `:reduce` function when you want to collect all values given
  for an option.

  

### `ARGV` (function)

    (ARGV)

Return a list of the program name and command line arguments.

  This is not implemented for every Common Lisp implementation.  You can always
  pass your own values to `parse-options` and `print-usage` if it's not
  implemented for your particular Lisp.

  

### `DEFINE-INTERFACE` (macro)

    (DEFINE-INTERFACE NAME USAGE &REST OPTIONS)

### `LATEST` (function)

    (LATEST OLD NEW)

Return `new`.

  It is useful as a `:reduce` function when you want to just keep the last-given
  value for an option.

  

### `PARSE-OPTIONS` (function)

    (PARSE-OPTIONS INTERFACE &OPTIONAL (ARGUMENTS (REST (ARGV))))

Parse `arguments` according to `interface`.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash map of option `name`s to values.

  See the full usage documentation for more information.

  

### `PRINT-USAGE` (function)

    (PRINT-USAGE INTERFACE &KEY (STREAM *STANDARD-OUTPUT*) (PROGRAM-NAME (FIRST (ARGV))) (WIDTH 80) (OPTION-WIDTH 20))

Print a pretty usage document for `interface` to `stream`.

  `width` should be the total width (in characters) for line-wrapping purposes.
  Care will be taken to ensure lines are no longer than this, though some edge
  cases (extremely long short/long option names and parameters) may slip
  through.

  `option-width` should be the width of the column of short/long options (in
  characters).  If the short/long option documentation is shorter than this, the
  option's documentation string will start on the same line.  Otherwise the
  option's documentation string will start on the next line.

  The result will look something like (assuming a usage string of
  `"[options] FILES"`):

    (print-usage *program-interface* :width 60 :option-width 15)
    ; =>
    ; USAGE: /bin/foo [options] FILES
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

  

