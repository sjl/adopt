Usage
=====

Adopt is a library for parsing UNIX-style command line arguments in Common Lisp.
It was made because none of the other libraries did what I needed.

[TOC]

Package
-------

All core Adopt functions are in the `adopt` package.  Several of the symbols in
adopt shadow those in the `common-lisp` package, so you should probably use
namespaced `adopt:…` symbols instead of `USE`ing the package.

Interfaces
----------

To get started with Adopt you can create an interface object with
`adopt:make-interface`.  This returns an object representing the command line
interface presented to your users.

### Creating an Interface

Let's say you're developing a program to search the contents of files (because
the world certainly needs *another* `grep` replacement).  You might start with
something like:

    :::lisp
    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help "Search the contents of each FILE for the regular expression PATTERN.  If no files are specified, searches standard input instead."))

`make-interface` takes several required arguments:

* `:name` is the name of the program.
* `:summary` is a concise one-line summary of what it does.
* `:usage` is a UNIX-style the command line usage string.
* `:help` is a longer description of the program.

You can now print some pretty help text for the CLI with `adopt:print-help`:

    :::lisp
    (adopt:print-help *ui*)
    ; =>
    ; search - search files for a regular expression
    ;
    ; USAGE: /path/to/binary [OPTIONS] PATTERN [FILE]...
    ;
    ; Search the contents of each FILE for the regular expression PATTERN.  If no
    ; files are specified, searches standard input instead.

### Line Wrapping

Adopt will handle line-wrapping your help text, so you don't need to (and
shouldn't) add extra line breaks when creating your interface.

If you want to line break the text in your source code to fit nicely in your
text editor, remember that `adopt:make-interface` is just a function — you can
use `format` (possibly with its [`~Newline` directive][tilde-newline]) to
preprocess the help text argument:

[tilde-newline]: http://www.lispworks.com/documentation/lw71/CLHS/Body/22_cic.htm

    :::lisp
    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help (format nil "Search the contents of each FILE for ~
                           the regular expression PATTERN.  If ~
                           no files are specified, searches ~
                           standard input instead.")))

If you want to pull out the documentation string into its own variable to keep
that `make-interface` call from getting too unwieldy, you can certainly do that:

    :::lisp
    (defparameter *help-text*
      (format nil "Search the contents of each FILE for the ~
                   regular expression PATTERN.  If no files ~
                   are specified, searches standard input ~
                   instead."))

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help *help-text*))

The `(defparameter … (format nil …))` pattern can be tedious to write, so Adopt
provides a helper macro `define-string` that does exactly that:

    :::lisp
    (adopt:define-string *help-text*
      "Search the contents of each FILE for the regular ~
       expression PATTERN.  If no files are specified, ~
       searches standard input instead.")

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help *help-text*))

Adopt's line-wrapping library [Bobbin][] will only ever *add* line breaks, never
remove them, which means you can include breaks in the output if you want to
have multiple paragraphs in your help text.  Once again, `format` is your
friend:

[Bobbin]: https://sjl.bitbucket.io/bobbin/

    :::lisp
    (adopt:define-string *help-text*
      "Search the contents of each FILE for the regular ~
       expression PATTERN.~@
       ~@
       If no files are specified (or if - is given as a ~
       file name), standard input will be searched instead.")

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help *help-text*))

If you want to control the width of the help text lines when they are printed,
`adopt:print-help` takes a `:width` argument:

    :::lisp
    (adopt:print-help *ui* :width 50)
    ; =>
    ; search - search files for a regular expression
    ;
    ; USAGE: … [OPTIONS] PATTERN [FILE]...
    ;
    ; Search the contents of each FILE for the regular
    ; expression PATTERN.
    ;
    ; If no files are specified (or if - is given as a
    ; file name), standard input will be searched
    ; instead.

`adopt:print-help` takes a number of other options — see the API Reference for
more information.

### Adding Examples

Describing the CLI in detail is helpful, but users can often learn a lot more by
seeing a few examples of its usage.  `make-interface` can take an `:examples`
argument, which should be an alist of `(description . example)` conses:

    :::lisp
    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :summary "search files for a regular expression"
        :usage "[OPTIONS] PATTERN [FILE]..."
        :help *help-text*
        :examples
        '(("Search foo.txt for the string 'hello':"
           . "search hello foo.txt")
          ("Search standard input for lines starting with x:"
           . "search '^x' -")
          ("Watch the file log.txt for lines containing the username steve.losh:"
           . "tail foo/bar/baz/log.txt | search --literal steve.losh -"))))

    (adopt:print-help *ui* :width 50)
    ; =>
    ; search - search files for a regular expression
    ;
    ; USAGE: … [OPTIONS] PATTERN [FILE]...
    ;
    ; Search the contents of each FILE for the regular
    ; expression PATTERN.
    ;
    ; If no files are specified (or if - is given as a
    ; file name) standard input will be searched
    ; instead.
    ;
    ; Examples:
    ;
    ;   Search foo.txt for the string 'hello':
    ;
    ;       search hello foo.txt
    ;
    ;   Search standard input for lines starting with x:
    ;
    ;       search '^x' -
    ;
    ;   Watch the file log.txt for lines containing the
    ;   username steve.losh:
    ;
    ;       tail foo/bar/baz/log.txt | search --literal steve.losh -

Notice how Adopt line wraps the prose explaining each example, but leaves the
example itself untouched for easier copying and pasting.  In general Adopt tries
to do the right thing for your users (even when that means making a little more
work for *you* in certain places).

Exiting
-------

Adopt provides some helpful utility functions to exit out of your program with
a UNIX exit code.  These do what you think they do:

    :::lisp
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
yourself if you prefer.

Options
-------

Now that you know how to create an interface, you can create some options to use
inside it with `adopt:make-option`:

    :::lisp
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

    :::lisp
    (adopt:print-help *ui*)
    ; =>
    ; search - search files for a regular expression
    ;
    ; USAGE: /usr/local/bin/sbcl [OPTIONS] PATTERN [FILE]...
    ;
    ; Search the contents of …
    ;
    ; Options:
    ;   --version             display version information and exit
    ;   -h, --help            display help information and exit
    ;   -l, --literal         treat PATTERN as a literal string instead of a regular
    ;                         expression

The first argument to `make-option` is the name of the option, which we'll see
put to use shortly.  At least one of `:short` and `:long` is required, and
`:help` text must be specified.  We'll talk more about `:reduce` in a little
while, but it too is required.

I prefer to define each option as its own global variable to keep the call to
`make-interface` from getting too large and unwieldy, but feel free to do
something like this if you prefer to avoid cluttering your package:

    :::lisp
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
parse a list of strings we've received as command line arguments with
`adopt:parse-options`:

    :::lisp
    (adopt:parse-options *ui* '("foo.*" "--literal" "a.txt" "b.txt"))
    ; =>
    ; ("foo.*" "a.txt" "b.txt")
    ; #<HASH-TABLE :TEST EQL :COUNT 3 {10103142A3}>

From now on I'll use a special pretty printer for hash tables to make it easier
to see what's inside them:

    :::lisp
    (adopt:parse-options *ui* '("foo.*" "--literal" "a.txt" "b.txt"))
    ; =>
    ; ("foo.*" "a.txt" "b.txt")
    ; {LITERAL: T, VERSION: NIL, HELP: NIL}

`parse-options` returns two values:

1. A list of non-option arguments.
2. An `eql` hash table of the option keys and values.

We'll talk about how the option values are determined soon.  The keys of the
hash table are (by default) the option names given as the first argument to
`make-option`.  You can specify a different key for a particular option with the
`:result-key` argument to `make-option`:

    :::lisp
    (defparameter *option-literal*
      (adopt:make-option 'literal
        :result-key 'pattern-is-literal
        :long "literal"
        :short #\l
        :help "treat PATTERN as a literal string instead of a regular expression"
        :reduce (constantly t)))

    ;; …

    (adopt:parse-options *ui* '("foo.*" "--literal" "a.txt" "b.txt"))
    ; =>
    ; ("foo.*" "a.txt" "b.txt")
    ; {PATTERN-IS-LITERAL: T, VERSION: NIL, HELP: NIL}

This can come in useful if you want multiple options that affect the same result
(e.g. `--verbose` and `--silent` flags that toggle extra log output on and off).

Option Formats
--------------

Adopt tries to support the most common styles of long and short UNIX options.

Long options must be given with two dashes (`--foo`).  If a long option takes
a parameter it can be given as the next argument (`--foo meow`) or mashed
together into the same argument using an equals sign (`--foo=meow`).

Short options must be given with a single dash (`-f`).  If several short options
do not take any parameters, they can be mashed together and given all at once
(`-xzvf`).  If a short option takes a parameter it can be given as the next
argument (`-n foo`) or mashed together with the option `-nfoo`.

The special string `--` signals that all remaining arguments are normal text
arguments, and should not be parsed as options.

Top-Level Structure
-------------------

We'll look at how the option values are computed shortly, but first let's see
the overall structure of the programs you'll typically create with Adopt:

    :::lisp
    (defun run (pattern files &key literal)
      ;; Actually do something here.
      )

    (defun toplevel ()
      (handler-case
          (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
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

    (defun build ()
      (sb-ext:save-lisp-and-die "search" :executable t :toplevel #'toplevel))

This is a typical way to use Adopt.  There are three functions important
functions here:

* The `toplevel` function takes care of parsing arguments and exiting with an
  appropriate status code if necessary.
* The `run` function takes parsed, Lispy arguments and actually *does*
  something.  When developing (in SLIME, VLIME, etc) you'll call `run`, because
  you don't want the program to exit when you're developing interactively.
* The `build` function dumps an executable binary.  For more complicated
  programs you might use something fancier, like ASDF or Shinmera's Deploy
  library instead.

In this example the `toplevel` function first uses a `handler-case` to trap all
errors.  If any error occurs it will print the error message and exit, to avoid
confusing users by dropping them into a Lisp debugger REPL (which they probably
won't understand).  If you're developing a program just for yourself, you might
want to omit this part and let yourself land in the debugger as usual.

Next it uses `adopt:parse-options` to parse the command line arguments and
options.  It them does some initial checks to see if the user wants `--help` or
`--version` information.  If so, it prints the requested information and exits.

Otherwise it destructures the arguments into the expected items and calls `run`
with all the information it needs to do its job.  If the `destructuring-bind`
fails an error will be signaled, and the `handler-case` will print it and exit.
If you want to be a nice person you could check that the `arguments` have the
correct shape first, and return a friendlier error message to your users if they
don't.

Computing Values with Reduce
----------------------------

So far we've talked about how to define an interface, print help text, parse
a list of options, and the overall structure of the program you'll create with
Adopt.  Now we need to talk about how the options the user specifies are parsed
and turned into the resulting hash table.

Not all command-line options are the same.  There are several common types of
options in the UNIX world:

* Simple options that are either given or not, like `--help` or `--version`.
* Boolean options, like git's `-p/--paginate` and `--no-pager`, where both options affect a single boolean flag.
* Counted options, where the number of times they are given has an effect, like SSH's `-v` option (more `-v`'s means more verbosity).
* Options that take a single parameter, like Mercurial's `--repository /path/to/repo` option, which specifies the path to a repository to work on.
* Options that collect all parameters they are given, like rsync's `--exclude PATTERN`, which you can pass multiple times to add several exclusions.

An option-parsing library needs to give you the tools to handle all of these
cases (and more).  Python's [argparse][ap-actions] library, for example, has
a number of different "actions" to account to handle these various use cases.
Adopt works differently: it uses an interface similar to [reduce][] to let you
do whatever you need.

[ap-actions]: https://docs.python.org/3/library/argparse.html#action
[reduce]: http://www.lispworks.com/documentation/HyperSpec/Body/f_reduce.htm

First: before any options are parsed, all entries in the options hash table have
their values set to the `:initial-value` given to `make-option` (or `nil` if
none was specified).

Next: When you create an option you must specify a `:reduce` function that takes
the current value (and, for options that take a parameter, the given parameter)
and produces a new value each time the option is given.

You may also specify a `:finally` function that will be called on the final
value after all parsing is done.

For convenience, if an option takes a parameter you may also specify a `:key`
function, which will be called on the given string before it is passed to the
`:reduce` function.  For example: you might use this for an option that takes
integers as arguments with something like `:key #'parse-integer`.

The combination of these four pieces will let you do just about anything you
might want.  Let's look at how to do some common option parsing tasks using
these as our building blocks.

### Simple Options

To define an option that just tracks whether it's ever been given, you can do
something like:

    :::lisp
    (defparameter *option-help*
      (adopt:make-option 'help
        :long "help"
        :short #\h
        :help "display help information and exit"
        :initial-value nil
        :reduce (lambda (current-value)
                  (declare (ignore current-value))
                  t)))

But since `nil` is the default initial value and Common Lisp provides the handy
[`constantly`](http://www.lispworks.com/documentation/HyperSpec/Body/f_cons_1.htm)
function, you can do this more concisely:

    :::lisp
    (defparameter *option-help*
      (adopt:make-option 'help
        :long "help"
        :short #\h
        :help "display help information and exit"
        :reduce (constantly t)))

### Boolean Options

If you want to have multiple options that both affect the same key in the
results, you can use `:result-key` to do this:

    :::lisp
    (defparameter *option-paginate*
      (adopt:make-option 'paginate
        :long "paginate"
        :short #\p
        :help "turn pagination on"
        :reduce (constantly t)))

    (defparameter *option-no-paginate*
      (adopt:make-option 'no-paginate
        :result-key 'paginate
        :long "no-paginate"
        :short #\P
        :help "turn pagination off (the default)"
        :reduce (constantly nil)))

The way we've written this, if the user gives multiple options the last-given
one will take precedence.  This is generally what you want, because it allows
someone to add a shell alias with these options like this:

    :::bash
    alias g='git --paginate --color=always'

but still lets them override an option at runtime for a single invocation:

    :::bash
    g --no-paginate log
    # expands to: git --paginate --color=always --no-paginate log

If the last-given option didn't take precedence, they'd have to fall back to the
non-alias version of the command, and type out all the options they *do* want by
hand.  This is annoying, so it's usually better to let the last one win.

### Counting Options

To define an option that counts how many times it's been given, like SSH's `-v`,
you can use something like this:

    :::lisp
    (defparameter *option-verbosity*
      (adopt:make-option 'verbosity
        :short #\v
        :help "output more verbose logs"
        :initial-value 0
        :reduce #'1+))

### Single-Parameter Options

To define an option that takes a parameter and only keeps the last one given,
you can do something like:

    :::lisp
    (defparameter *option-repository*
      (adopt:make-option 'repository
        :parameter "PATTERN"
        :long "repository"
        :short #\R
        :help "path to the repository (default .)"
        :initial-value "."
        :reduce (lambda (prev new)
                  (declare (ignore prev))
                  new)))

Specifying the `:parameter` argument makes this option a parameter-taking
option, which means the `:reduce` function will be called with the current value
and the given parameter each time.

Writing that `lambda` out by hand every time would be tedious.  Adopt provides
a function called `last` (as in "keep the *last* parameter given") that does
exactly that:

    :::lisp
    (defparameter *option-repository*
      (adopt:make-option 'repository
        :long "repository"
        :short #\R
        :help "path to the repository (default .)"
        :initial-value "."
        :reduce #'adopt:last))

### Multiple-Parameter Options

Collecting every parameter given can be done in a number of different ways.  One
strategy could be:

    :::lisp
    (defparameter *option-exclude*
      (adopt:make-option 'exclude
        :long "exclude"
        :parameter "PATTERN"
        :help "exclude PATTERN (may be given multiple times)"
        :initial-value nil
        :reduce (lambda (patterns new)
                  (cons new patterns))))

You might notice that the `:reduce` function here is just `cons` with its
arguments flipped.  Common Lisp doesn't have a function like Haskell's
[flip](https://en.wikibooks.org/wiki/Haskell/Higher-order_functions#Flipping_arguments),
so Adopt provides it:

    :::lisp
    (defparameter *option-exclude*
      (adopt:make-option 'exclude
        :long "exclude"
        :parameter "PATTERN"
        :help "exclude PATTERN (may be given multiple times)"
        :initial-value nil
        :reduce (adopt:flip #'cons)))

Note that the result of this will be a fresh list of all the given parameters,
but their order will be reversed because `cons` adds each new parameter to the
front of the list.  If the order doesn't matter for what you're going to do with
it, you're all set.  Otherwise, there are several ways to get around this
problem.  The first is to add the parameter to the end of the list in the
`:reduce` function:

    :::lisp
    (defparameter *option-exclude*
      (adopt:make-option 'exclude
        :long "exclude"
        :parameter "PATTERN"
        :help "exclude PATTERN (may be given multiple times)"
        :initial-value nil
        :reduce (lambda (patterns new)
                  (append patterns (list new)))))

This is tedious and inefficient if you have a lot of arguments.  If you don't
care much about argument parsing speed, Adopt provides a function called
`collect` that does exactly this, so you don't have to type out that `lambda`
yourself (and `nil` is the default initial value, so you don't need that
either):

    :::lisp
    (defparameter *option-exclude*
      (adopt:make-option 'exclude
        :long "exclude"
        :parameter "PATTERN"
        :help "exclude PATTERN (may be given multiple times)"
        :reduce #'adopt:collect))

A more efficient (though slightly uglier) solution would be to use `nreverse` at
the end:

    :::lisp
    (defparameter *option-exclude*
      (adopt:make-option 'exclude
        :long "exclude"
        :parameter "PATTERN"
        :help "exclude PATTERN (may be given multiple times)"
        :reduce (adopt:flip #'cons)
        :finally #'nreverse))

If you really need maximum efficiency when parsing command line options (you
probably don't) you could use a queue library, or use a vector and
`vector-push-extend`, or anything else you might dream up.  The combination of
`:reduce`, `:initial-value`, and `:finally` will let you do just about anything.

Required Options
----------------

Adopt doesn't have a concept of a required option.  Not only is "required
option" an oxymoron, but it's almost never what you want — if a user types
`foo --help` they shouldn't get an error about a missing required option.

In cases where you really do need to require an option (perhaps only if some
other one is also given) you can check it yourself:

    :::lisp
    (defun toplevel ()
      (handler-case
          (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
            (when (gethash 'help options)
              (adopt:print-help-and-exit *ui*))
            (unless (gethash 'some-required-option options)
              (error "Required option foo is missing."))
            (run …))
        (error (c)
          (adopt:print-error-and-exit c))))

Option Groups
-------------

Related options can be grouped together in the help text to make them easier for
users to understand.  Groups can have their own name, title, and help text.

Here's a example of how this works.  It's fairly long, but shows how Adopt can
help you make a command line interface with all the fixins:

    :::lisp
    (defparameter *option-help*
      (adopt:make-option 'help
        :help "display help and exit"
        :long "help"
        :short #\h
        :reduce (constantly t)))

    (defparameter *option-literal*
      (adopt:make-option 'literal
        :help "treat PATTERN as a literal string instead of a regex"
        :long "literal"
        :short #\l
        :reduce (constantly t)))

    (defparameter *option-no-literal*
      (adopt:make-option 'no-literal
        :help "treat PATTERN as a regex (the default)"
        :long "no-literal"
        :short #\L
        :result-key 'literal
        :reduce (constantly nil)))

    (defparameter *option-case-sensitive*
      (adopt:make-option 'case-sensitive
        :help "match case-sensitively (the default)"
        :long "case-sensitive"
        :short #\c
        :initial-value t
        :reduce (constantly t)))

    (defparameter *option-case-insensitive*
      (adopt:make-option 'case-insensitive
        :help "ignore case when matching"
        :long "case-insensitive"
        :short #\C
        :result-key 'case-sensitive
        :reduce (constantly nil)))

    (defparameter *option-color*
      (adopt:make-option 'color
        :help "highlight matches with color"
        :long "color"
        :reduce (constantly t)))

    (defparameter *option-no-color*
      (adopt:make-option 'no-color
        :help "don't highlight matches (the default)"
        :long "no-color"
        :result-key 'color
        :reduce (constantly nil)))

    (defparameter *option-context*
      (adopt:make-option 'context
        :parameter "N"
        :help "show N lines of context (default 0)"
        :long "context"
        :short #\U
        :initial-value 0
        :reduce #'adopt:last
        :key #'parse-integer))

    (defparameter *group-matching*
      (adopt:make-group 'matching-options
        :title "Matching Options"
        :help "These options affect how lines are matched."
        :options (list *option-literal*
                       *option-no-literal*
                       *option-case-sensitive*
                       *option-case-insensitive*)))

    (defparameter *group-output*
      (adopt:make-group 'output-options
        :title "Output Options"
        :help "These options affect how matching lines are printed."
        :options (list *option-color*
                       *option-no-color*
                       *option-context*)))

    (adopt:define-string *help-text*
      "Search FILEs for lines that match the regular expression ~
       PATTERN and print them to standard out.  Several options ~
       are available to control how the matching lines are printed.~@
       ~@
       If no files are given (or if - is given as a filename) ~
       standard input will be searched.")

    (defparameter *ui*
      (adopt:make-interface
        :name "search"
        :usage "PATTERN [FILE...]"
        :summary "print lines that match a regular expression"
        :help *help-text*
        :contents (list *option-help*
                        *group-matching*
                        *group-output*)))

And with all that out of the way, you've got some nicely-organized help text
for your users:

    :::lisp
    (adopt:print-help *ui* :width 60 :option-width 16)
    ; =>
    ; search - print lines that match a regular expression
    ;
    ; USAGE: /usr/local/bin/sbcl PATTERN [FILE...]
    ;
    ; Search FILEs for lines that match the regular expression
    ; PATTERN and print them to standard out.  Several options are
    ; available to control how the matching lines are printed.
    ;
    ; If no files are given (or if - is given as a filename)
    ; standard input will be searched.
    ;
    ; Options:
    ;   -h, --help        display help and exit
    ;
    ; Matching Options:
    ;   -l, --literal     treat PATTERN as a literal string
    ;                     instead of a regex
    ;   -L, --no-literal  treat PATTERN as a regex (the default)
    ;   -c, --case-sensitive
    ;                     match case-sensitively (the default)
    ;   -C, --case-insensitive
    ;                     ignore case when matching
    ;
    ; Output Options:
    ;   --color           highlight matches with color
    ;   --no-color        don't highlight matches (the default)
    ;   -U N, --context N show N lines of context (default 0)

Error Handling
--------------

For the most part Adopt doesn't try to be too smart about error handling and
leaves it up to you.

However, when Adopt is parsing the command line options it *will* signal an
error of type `adopt:unrecognized-option` if the user passes a command line
option that wasn't defined in the interface:

    :::lisp
    (defparameter *ui*
      (adopt:make-interface
        :name "meow"
        :summary "say meow"
        :usage "[OPTIONS]"
        :help "Say meow.  Like a cat."
        :contents (list (make-option 'times
                          :parameter "N"
                          :long "times"
                          :initial-value 1
                          :help "say meow N times (default 1)"
                          :reduce #'adopt:last
                          :key #'parse-integer))))

    (adopt:parse-options *ui* '("--times" "5"))
    ; =>
    ; NIL
    ; {TIMES: 5}

    (adopt:parse-options *ui* '("--bark"))
    ; =>
    ; No such option "--bark".
    ;    [Condition of type UNRECOGNIZED-OPTION]
    ;
    ; Restarts:
    ;   R 0.  DISCARD-OPTION    - Discard the unrecognized option.
    ;   R 1.  TREAT-AS-ARGUMENT - Treat the unrecognized option as a plain argument.
    ;   R 2.  SUPPLY-NEW-VALUE  - Supply a new value to parse.
    ;   R 3.  RETRY             - Retry SLIME REPL evaluation request.
    ;   R 4. *ABORT             - Return to SLIME's top level.
    ;   R 5.  ABORT             - abort thread (#<THREAD "repl-thread" RUNNING {100AF48413}>)

Adopt provides three possible restarts for this condition as seen above.  Adopt
also provides functions with the same names that invoke the restarts properly,
to make it easier to use them programatically with `handler-bind`.  For example:

    :::lisp
    (handler-bind
        ((adopt:unrecognized-option 'adopt:discard-option))
      (adopt:parse-options *ui* '("--bark")))
    ; =>
    ; NIL
    ; {TIMES: 1}

    (handler-bind
        ((adopt:unrecognized-option 'adopt:treat-as-argument))
      (adopt:parse-options *ui* '("--bark")))
    ; =>
    ; ("--bark")
    ; {TIMES: 1}

    (handler-bind
        ((adopt:unrecognized-option
           (alexandria:rcurry 'adopt:supply-new-value "--times")))
      (adopt:parse-options *ui* '("--bark" "5")))
    ; =>
    ; NIL
    ; {TIMES: 5}

Generating Man Pages
--------------------

We've already seen that Adopt can print a pretty help document, but it can also
render `man` pages for you:

    :::lisp
    (with-open-file (out "man/man1/search.1"
                      :direction :output
                      :if-exists :supersede)
      (adopt:print-manual *ui* :stream out))

The generated `man` page will contain the same information as the help text by
default.  If you want to override this (e.g. to provide a short summary of an
option in the help text, but elaborate more in the manual), you can use the
`:manual` argument to `make-interface` and `make-option`:

    :::lisp
    (defparameter *option-exclude*
      (adopt:make-option 'exclude
        :long "exclude"
        :parameter "PATTERN"
        :help "exclude PATTERN"
        :manual "Exclude lines that match PATTERN (a PERL-compatible regular expression) from the search results.  Multiple PATTERNs can be specified by giving this option multiple times."
        :reduce (adopt:flip #'cons)))

In order for `man` to find the pages, they need to be in the correct place.  By
default `man` is usually smart enough to look next to every directory in your
`$PATH` to find a directory called `man`.  So if you put your binaries in
`/home/me/bin/` you can put your `man` pages in `/home/me/man/` under the
appropriate subdirectories and it should all Just Work™.  Consult the `man`
documentation for more information.

Implementation Specifics
------------------------

TODO: talk about Lisp runtime options vs program options.

### SBCL

You'll want to use `:save-runtime-options t` in the call to `sb-ext:save-lisp-and-die`.

### ClozureCL

See <https://github.com/Clozure/ccl/issues/177>.
