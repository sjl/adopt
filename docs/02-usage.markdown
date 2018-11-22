Usage
=====

Adopt is a simple library for parsing UNIX-style command line arguments in
Common Lisp.  It was made because none of the other libraries did what I needed.

[TOC]

Package
-------

All core Adopt functions are in the `adopt` package.  You can `:use` that if you
really want to, but it's probably clearer to use namespaced `adopt:…` symbols.

Example
-------

    (define-interface *my-program* "[options] FILES"
      (verbosity
        "Output extra information."
        :short #\v
        :long "verbose"
        :initial-value 0
        :reduce (constantly 1))
      (verbosity
        "Shut up."
        :short #\q
        :long "quiet"
        :reduce (constantly -1))
      (ignore
        "Ignore FILE.  May be specified multiple times."
        :long "ignore"
        :parameter "FILE"
        :reduce #'append1)
      (name
        "Your name.  May be specified many times, last one wins."
        :short #\n
        :long "name"
        :parameter "NAME"
        :reduce #'latest)
      (meows
        "Meow."
        :short #\m
        :long "meow"
        :initial-value 0
        :reduce #'1+))
    
    (pprint
      (multiple-value-list
        (parse-options *my-program*
                       '("-vqn" "steve"
                         "--meow"
                         "-m" "--meow" "foo"
                         "--name=sjl" "more"
                         "--" "--ignore" "bar"
                         ))))
