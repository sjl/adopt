Usage
=====

Adopt is a simple library for parsing UNIX-style command line arguments in
Common Lisp.  It was made because none of the other libraries did what I needed.

[TOC]

Package
-------

All core Adopt functions and macros are in the `adopt` package.  You can `:use`
that if you really want to, but it's probably clearer to use namespaced
`adopt:…` symbols.

Interfaces
----------

define-interface and usage.  nothing else.

Parsing
-------

`parse-options` with a bare-bones interface.

Options
-------

talk about the option arguments.

Short and Long Options
----------------------

short versus long arguments.  1+ is required.

Initial Value
-------------

at this point it's just a default

Reduce
------

talk about reducers. mention argparse's:

`store_const` is `(constantly x)`

`store` is `#'latest`

`store_true` is `(constantly t)`

`store_false` is `(constantly nil)`

`append` is `append1`

`append_const` is `(rcurry #'append1 x)`

`count` is `1+` with an `initial-value` of `0`.

need to figure out `help` and `version`.

Usage Printing
--------------

It's `(print-usage interface)`.
