Usage
=====

Adopt is a simple library for parsing UNIX-style command line arguments in
Common Lisp.  It was made because none of the other libraries did what I needed.

[TOC]

Package
-------

All core Adopt functions are in the `adopt` package.  Several of the symbols in
adopt shadow those in the `common-lisp` package, so you should probably use
namespaced `adopt:…` symbols instead of `USE`ing the package.

Interfaces
----------

To get started with Adopt, you should create an interface with the
`adopt:make-interface` function.  This returns an object representing the
command line interface presented to your users.

### Creating an Interface

Let's say you're developing a program to search the contents of files (because
the world certainly needs *another* `grep` replacement).  You might start with
something like:

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help "Search the contents of each FILE for the regular expression PATTERN.  If no files are specified, searches standard input instead."))

You can now print some help text for your CLI with `adopt:print-help`:

    (adopt:print-help *ui*)
    ; =>
    search - search files for a regular expression

    USAGE: … [OPTIONS] PATTERN [FILE]...

    Search the contents of each FILE for the regular expression PATTERN.  If no
    files are specified, searches standard input instead.

### Line Wrapping

Adopt will handle line-wrapping your help text, so you don't need to (and
shouldn't) add extra line breaks when creating your interface.  If you want to
line break the text in your source code to fit nicely in your editor, remember
that `adopt:make-interface` is just a function — you can use `format` (possibly
with its `~Newline` directive) to preprocess the help text argument:

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help (format nil "Search the contents of each FILE for ~
                           the regular expression PATTERN.  If ~
                           no files are specified, searches ~
                           standard input instead.")))

Adopt's line-wrapping library [Bobbin][] will only ever *add* line breaks, never
remove them, which means you can include breaks in the output if you want to
have multiple paragraphs in your help text:

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help (format nil
                "Search the contents of each FILE for the regular expression PATTERN.~@
                 ~@
                 If no files are specified (or if - is given as a file name) standard input will be searched instead.")))

If you want to control the width of the help text lines when they are printed,
`adopt:print-help` takes a `:width` argument:

    (adopt:print-help *ui* :width 50)
    ; =>
    search - search files for a regular expression

    USAGE: … [OPTIONS] PATTERN [FILE]...

    Search the contents of each FILE for the regular
    expression PATTERN.

    If no files are specified (or if - is given as a
    file name) standard input will be searched
    instead.

`adopt:print-help` takes a number of other options — see the API Reference for
more information.

### Examples

Describing the CLI in detail is helpful, but users can often learn a lot more by
seeing a few examples of its usage.  `adopt:make-interface` can take an
`:examples` argument, which should be an alist of `(description . example)`
conses:

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help …
        :examples
        '(("Search foo.txt for the string 'hello':"
           . "search hello foo.txt")
          ("Search standard input for lines starting with x:"
           . "search '^x' -")
          ("Watch the file log.txt for lines containing the username steve.losh:"
           . "tail foo/bar/baz/log.txt | search --literal steve.losh -"))))

    (adopt:print-help *ui* :width 50)
    ; =>
    search - search files for a regular expression

    USAGE: … [OPTIONS] PATTERN [FILE]...

    Search the contents of each FILE for the regular
    expression PATTERN.

    If no files are specified (or if - is given as a
    file name) standard input will be searched
    instead.

    Examples:

      Search foo.txt for the string 'hello':

          search hello foo.txt

      Search standard input for lines starting with x:

          search '^x' -

      Watch the file log.txt for lines containing the
      username steve.losh:

          tail foo/bar/baz/log.txt | search --literal steve.losh -

Notice how Adopt line wraps the prose explaining each example, but leaves the
example itself untouched for easier copying and pasting.  In general Adopt tries
to do the right thing for your users (even when that means making a little more
work for *you* in certain places).

Exiting
-------

Adopt provides some helpful utility functions to exit out of your program with
a UNIX exit code.  These do what you think they do:

    (adopt:exit)

    (adopt:exit 1) 

    (adopt:print-help-and-exit *ui*)

    (adopt:print-help-and-exit *ui*
      :stream *error-output*
      :exit-code 1)

    (handler-case (assert (= 1 0))
      (error (err)
        (adopt:print-error-and-exit err)))

These functions are not implemented for every Lisp implementation.  PRs are
welcome, or you can just write the implementation-specific calls in your program
yourself.

Options
-------

Now that you know how to create an interface, you can create some options to use
inside it with `adopt:make-option`:

    (defparameter *option-version*
      (adopt:make-option 'version
        :long "version"
        :help "display version information and exit"
        :reduce (constantly t)))

    (defparameter *option-help*
      (adopt:make-option 'help
        :long "help"
        :short #\h
        :help "display help information and exit"
        :reduce (constantly t)))

    (defparameter *option-literal*
      (adopt:make-option 'literal
        :long "literal"
        :short #\l
        :help "treat PATTERN as a literal string instead of a regular expression"
        :reduce (constantly t)))

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help "Search the contents of …"
        :contents (list
                    *option-version*
                    *option-help*
                    *option-literal*)))

Adopt will automatically add the options to the help text:

    (adopt:print-help *ui*)
    ; =>
    search - search files for a regular expression

    USAGE: /usr/local/bin/sbcl [OPTIONS] PATTERN [FILE]...

    Search the contents of …

    Options:
      --version             display version information and exit
      -h, --help            display help information and exit
      -l, --literal         treat PATTERN as a literal string instead of a regular
                            expression

The first argument to `adopt:make-option` is the name of the option, which we'll
see put to use shortly.  At least one of `:short` and `:long` is required, and
`:help` text must be specified.  We'll talk more about `:reduce` in a bit, but
it too is required.

I prefer to define each option as its own global variable to keep the call to
`adopt:make-interface` from getting too large and unwieldy, but feel free to do
something like this if you prefer to avoid cluttering your package:

    (defparameter *ui*
      (adopt:make-interface
        …
        :contents
        (list (adopt:make-option 'foo …)
              (adopt:make-option 'bar …)
              …)))

Parsing
-------

At this point we've got an interface with some options, so we can use it to
parse a list of strings we've received as command line arguments:

    (adopt:parse-options *ui* '("foo.*" "--literal" "a.txt" "b.txt"))
    ; =>
    ("foo.*" "a.txt" "b.txt")
    #<HASH-TABLE :TEST EQL :COUNT 3 {10103142A3}>

`adopt:parse-options` returns two values: a list of non-option arguments, and
a hash table of the option values.

The keys of the hash table are (by default) the option names given as the first
argument to `adopt:make-option`.  We'll see how the option values are determined
soon.

From now on I'll use a special pretty printer for hash tables to make it easier
to see what's inside them:

    (adopt:parse-options *ui* '("foo.*" "--literal" "a.txt" "b.txt"))
    ; =>
    ("foo.*" "a.txt" "b.txt")
    {LITERAL: T, VERSION: NIL, HELP: NIL}

Top-Level Structure
-------------------

We'll look at how the option values are computed shortly, but first let's see
the overall structure of the programs you create with Adopt:

    (defun toplevel ()
      (handler-case
          (multiple-value-bind (arguments options)
              (adopt:parse-options *ui*)
            (when (gethash 'help options)
              (adopt:print-help-and-exit *ui*))
            (when (gethash 'version options)
              (format t "1.0.0~%")
              (adopt:exit))
            (destructuring-bind (pattern . files) arguments
              (run pattern
                   files
                   :literal (gethash 'literal options))))
        (error (c)
          (adopt:print-error-and-exit c))))

    (sb-ext:save-lisp-and-die "search" :toplevel #'toplevel)

The `toplevel` function first uses a `handler-case` to trap all `error`s.  If
any error occurs it will print the error message and exit, to avoid confusing
users by dropping them into a Lisp debugger REPL (which they probably won't
understand).  When you're developing your program yourself you'll want to omit
this part and let yourself land in the debugger as usual.

Next we use `adopt:parse-options` to parse the command line arguments and
options.  We do some initial checks to see if the user wants `--help` or
`--version` information.  If not, we destructure the arguments into the items we
expect and call a `run` function with all the information it needs to do its
job.

If the `destructuring-bind` fails an error will be signaled, and the
`handler-case` will print it and exit.  If you want to be a nice person you
could check that the `arguments` have the correct shape first, and return
a friendlier error message to your users if they don't.

Computing Values with Reduce
----------------------------

