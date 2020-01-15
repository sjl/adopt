I needed **a** **d**amn **opt**ion parsing library.

Adopt is a simple UNIX-style option parser in Common Lisp, heavily influenced by
Python's optparse and argparse.

* **License:** MIT
* **Documentation:** <https://docs.stevelosh.com/adopt/>
* **Mercurial:** <https://hg.stevelosh.com/adopt/>
* **Git:** <https://github.com/sjl/adopt/>

Adopt aims to be a simple, robust option parser.  It can automatically print
help information and even generate `man` pages for you.

Adopt can be installed with Quicklisp: `(ql:quickload :adopt)`.

The `adopt` system contains the core API and depends on [Bobbin][] and
[split-sequence][].

The `adopt/test` system contains the test suite, which depends on some other
systems.  You don't need to load this unless you want to run the unit tests.
The tests pass on SBCL, CCL, ECL, and ABCL on Ubuntu 18.04.  Further testing is
welcome.

[Bobbin]: https://github.com/sjl/bobbin
[split-sequence]: https://www.cliki.net/split-sequence
