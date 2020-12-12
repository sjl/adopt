(in-package :adopt)

;;;; Utils --------------------------------------------------------------------
(defun flip (function)
  "Return a function of two arguments X and Y that calls `function` with Y and X.

  Useful for wrapping existing functions that expect their arguments in the
  opposite order.

  Examples:

    (funcall #'cons 1 2)        ; => (1 . 2)
    (funcall (flip #'cons) 1 2) ; => (2 . 1)
    (reduce (flip #'cons) '(1 2 3) :initial-value nil)
    ; => (3 2 1)

  "
  (lambda (x y)
    (funcall function y x)))

(defun collect (list el)
  "Append element `el` to the end of `list`.

  It is useful as a `:reduce` function when you want to collect all values given
  for an option.

  This is implemented as `(append list (list el))`.  It is not particularly
  fast.  If you can live with reversed output consider `(flip #'cons)`  instead.

  "
  (append list (list el)))

(defun last (old new)
  "Return `new`.

  It is useful as a `:reduce` function when you want to just keep the last-given
  value for an option.

  "
  (declare (ignore old))
  new)

(defun first (old new)
  "Return `new` if `old` is `nil`, otherwise return `old`.

  It is useful as a `:reduce` function when you want to just keep the
  first-given value for an option.

  "
  (if (null old)
    new
    old))


(defun argv ()
  "Return a list of the program name and command line arguments.

  This is not implemented for every Common Lisp implementation.  You can pass
  your own values to `parse-options` and `print-help` if it's not implemented
  for your particular Lisp.

  "
  #+sbcl sb-ext:*posix-argv*
  #+ccl (destructuring-bind (program-name &rest arguments)
            ccl:*command-line-argument-list*
            (cons program-name (rest (member "--" arguments :test #'string=))))
  ;; #+ecl (ext:command-args)
  #-(or sbcl ccl ecl) (error "ARGV is not supported on this implementation."))

(defun exit (&optional (code 0))
  "Exit the program with status `code`.

  This is not implemented for every Common Lisp implementation.  You can write
  your own version of it and pass it to `print-help-and-exit` and
  `print-error-and-exit` if it's not implemented for your particular Lisp.

  "
  #+sbcl (sb-ext:exit :code code)
  #+ccl (ccl:quit code)
  #-(or sbcl ccl) (error "EXIT is not supported on this implementation."))


(defun funcall% (value function)
  (funcall function value))

(define-modify-macro funcallf (function) funcall%)


(defmacro define-string (var string &rest args)
  "Convenience macro for `(defparameter ,var (format nil ,string ,@args))`."
  `(defparameter ,var (format nil ,string ,@args)))


(defmacro defclass* (name &rest slots)
  `(defclass ,name ()
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :accessor ,slot))))


(defmacro check-types (&rest place-type-pairs)
  `(progn
     ,@(loop :for (place type) :on place-type-pairs :by #'cddr
             :collect `(check-type ,place ,type))))


(defmacro quit-on-ctrl-c ((&key (code 130)) &body body)
  `(handler-case
     (progn ,@body)
     #+sbcl (sb-sys:interactive-interrupt () (adopt:exit ,code))))


;;;; Definition ---------------------------------------------------------------
(defclass* option
  name
  result-key
  help
  manual
  short
  long
  parameter
  initial-value
  key
  finally
  reduce)

(defmethod print-object ((o option) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A ~A/~A" (name o) (short o) (long o))))

(defun make-option (name &key
                    long
                    short
                    help
                    manual
                    parameter
                    reduce
                    ;; can't just default to nil because multiple options might
                    ;; have the same result key, and only one can provide init
                    (initial-value nil initial-value?)
                    (result-key name)
                    (key #'identity)
                    (finally #'identity))
  "Create and return an option, suitable for use in an interface.

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

  "
  (when (and (null long) (null short))
    (error "Option ~A requires at least one of :long/:short." name))
  (when (null reduce)
    (error "Option ~A is missing required argument :reduce." name))
  (when (null help)
    (error "Option ~A is missing required argument :help" name))
  (when (and (member reduce (list 'collect #'collect
                                  'first #'first
                                  'last #'last))
             (null parameter))
    (error "Option ~A has reduce function ~A, which requires a :parameter."
           name reduce))
  (check-types short (or null character)
               long (or null string)
               help string
               manual (or null string)
               parameter (or null string))
  (apply #'make-instance 'option
    :name name
    :result-key result-key
    :help help
    :manual manual
    :long long
    :short short
    :parameter parameter
    :reduce reduce
    :key key
    :finally finally
    (when initial-value?
      (list :initial-value initial-value))))

(defun optionp (object)
  (typep object 'option))


(defmacro defparameters (parameters values-form)
  "Convenience macro for `defparameter`ing multiple variables at once.

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
        :help \"Foo the widgets during the run.\"
        :help-no \"Do not foo the widgets during the run (the default).\"
        :long \"foo\"
        :short #\f))

  "
  `(progn
     ,@(loop :for parameter :in parameters
             :collect `(defparameter ,parameter nil))
     (setf (values ,@parameters) ,values-form)
     ',parameters))

(defun make-boolean-options
    (name &key
     (name-no (intern (concatenate 'string (string 'no-) (string name))))
     long
     (long-no (when long (format nil "no-~A" long)))
     short
     (short-no (when short (char-upcase short)))
     (result-key name)
     help
     help-no
     manual
     manual-no
     initial-value)
  "Create and return a pair of boolean options, suitable for use in an interface.

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
        :long \"debug\"
        :short #\d
        :help \"Enable the Lisp debugger.\"
        :help-no \"Disable the Lisp debugger (the default).\"))

    ;; is roughly equivalent to:

    (defparameter *option-debug*
      (make-option 'debug
        :long \"debug\"
        :short #\d
        :help \"Enable the Lisp debugger.\"
        :initial-value nil
        :reduce (constantly t))

    (defparameter *option-no-debug*
      (make-option 'no-debug
        :long \"no-debug\"
        :short #\D
        :help \"Disable the Lisp debugger (the default).\"
        :reduce (constantly nil))

  "
  (values (adopt:make-option name
            :result-key result-key
            :long long
            :short short
            :help help
            :manual manual
            :initial-value initial-value
            :reduce (constantly t))
          (adopt:make-option name-no
            :result-key result-key
            :long long-no
            :short short-no
            :help help-no
            :manual manual-no
            :reduce (constantly nil))))


(defclass* group
  name title help manual options)

(defmethod print-object ((g group) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~A (~D options)" (name g) (length (options g)))))

(defun make-group (name &key title help manual options)
  "Create and return an option group, suitable for use in an interface.

  This function takes a number of arguments that define how the group is
  presented to the user:

  * `name` (**required**): a symbol naming the group.
  * `title` (optional): a title for the group for use in the help text.
  * `help` (optional): a short summary of this group of options for use in the help text.
  * `manual` (optional): used in place of `help` when rendering a man page.
  * `options` (**required**): the options to include in the group.

  See the full documentation for more information.

  "
  (check-types name symbol
               title (or null string)
               help (or null string)
               manual (or null string)
               options list)
  (assert (every #'optionp options) (options)
    "The :options argument to ~S was not a list of options.  Got: ~S"
    'make-group options)
  (make-instance 'group
    :name name
    :title title
    :help help
    :manual manual
    :options options))

(defun make-default-group (options)
  (make-instance 'group
    :name nil
    :title nil
    :help nil
    :manual nil
    :options options))

(defun groupp (object)
  (typep object 'group))


(defclass* interface
  name
  summary
  examples
  options
  groups
  short-options
  long-options
  usage
  help
  manual)

(defmethod print-object ((i interface) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~A (~D options in ~D groups)"
            (name i)
            (length (options i))
            (length (groups i)))))

(defun make-interface (&key name summary usage help manual examples contents)
  "Create and return a command line interface.

  This function takes a number of arguments that define how the interface is
  presented to the user:

  * `name` (**required**): a symbol naming the interface.
  * `summary` (**required**): a string of a concise, one-line summary of what the program does.
  * `usage` (**required**): a string of a UNIX-style usage summary, e.g. `\"[OPTIONS] PATTERN [FILE...]\"`.
  * `help` (**required**): a string of a longer description of the program.
  * `manual` (optional): a string to use in place of `help` when rendering a man page.
  * `examples` (optional): an alist of `(prose . command)` conses to render as a list of examples.
  * `contents` (optional): a list of options and groups.  Ungrouped options will be collected into a single top-level group.

  See the full documentation for more information.

  "
  (check-types name string
               summary string
               usage string
               help string
               manual (or null string)
               examples list
               contents list)
  (let* ((ungrouped-options (remove-if-not #'optionp contents))
         (groups (cons (make-default-group ungrouped-options)
                       (remove-if-not #'groupp contents)))
         (options (loop :for g :in groups :append (options g)))
         (interface (make-instance 'interface
                      :name name
                      :usage usage
                      :summary summary
                      :help help
                      :manual manual
                      :examples examples
                      :groups groups
                      :options options
                      :short-options (make-hash-table)
                      :long-options (make-hash-table :test #'equal))))
    (flet ((add-option (option)
             (let ((short (short option))
                   (long (long option)))
               (when short
                 (when (gethash short (short-options interface))
                   (error "Duplicate short option ~S." short))
                 (setf (gethash short (short-options interface)) option))
               (when long
                 (when (gethash long (long-options interface))
                   (error "Duplicate long option ~S." long))
                 (setf (gethash long (long-options interface)) option)))))
      (dolist (g groups)
        (map nil #'add-option (options g))))
    ;; TODO: check for multiple conflicting initial-values
    interface))


;;;; Parsing ------------------------------------------------------------------
(defun shortp (arg)
  (and (> (length arg) 1)
       (char= #\- (aref arg 0))
       (char/= #\- (aref arg 1))))

(defun longp (arg)
  (and (> (length arg) 2)
       (char= #\- (aref arg 0))
       (char= #\- (aref arg 1))))

(defun terminatorp (arg)
  (string= "--" arg))


(define-condition unrecognized-option (error)
  ((problematic-option :accessor problematic-option :initarg :problematic-option))
  (:report (lambda (condition stream)
             (format stream "No such option ~S." (problematic-option condition)))))

(defun unrecognized-option-p (value)
  (typep value 'unrecognized-option))


(defun parse-long (interface results arg remaining)
  (let* ((= (position #\= arg))
         (long-name (subseq arg 2 =))
         (option (gethash long-name (long-options interface))))
    (when (null option)
      (error 'unrecognized-option :problematic-option (format nil "--~A" long-name)))
    (let* ((k (result-key option))
           (current (gethash k results)))
      (setf (gethash k results)
            (if (parameter option)
              (let ((param (funcall (key option)
                                    (if =
                                      (subseq arg (1+ =))
                                      (pop remaining)))))
                (funcall (reduce option) current param))
              (funcall (reduce option) current)))))
  remaining)

(defun parse-short (interface results arg remaining)
  (let* ((short-name (aref arg 1))
         (option (gethash short-name (short-options interface))))
    (when (null option)
      (error 'unrecognized-option :problematic-option (format nil "-~A" short-name)))
    (let* ((k (result-key option))
           (current (gethash k results)))
      (setf (gethash k results)
            (if (parameter option)
              (let ((param (funcall (key option)
                                    (if (> (length arg) 2)
                                      (subseq arg 2) ; -xfoo
                                      (pop remaining))))); -x foo
                (funcall (reduce option) current param))
              (prog1 (funcall (reduce option) current)
                (if (> (length arg) 2)
                  (push (format nil "-~A" (subseq arg 2)) remaining)))))))
  remaining)


(defun initialize-results (interface results)
  (dolist (option (options interface))
    (when (slot-boundp option 'initial-value)
      (setf (gethash (result-key option) results)
            (initial-value option))))
  results)

(defun finalize-results (interface results)
  (dolist (option (options interface))
    (funcallf (gethash (result-key option) results)
              (finally option)))
  results)


(defun discard-option (condition)
  "Invoke the `discard-option` restart properly.

  Example:

    (handler-bind ((unrecognized-option 'discard-option))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  "
  (invoke-restart (find-restart 'discard-option condition)))

(defun treat-as-argument (condition)
  "Invoke the `treat-as-argument` restart properly.

  Example:

    (handler-bind ((unrecognized-option 'treat-as-argument))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  "
  (invoke-restart (find-restart 'treat-as-argument condition)))

(defun supply-new-value (condition value)
  "Invoke the `supply-new-value` restart properly.

  Example:

    (handler-bind
        ((unrecognized-option (alexandria:rcurry 'supply-new-value \"--foo\"))
      (multiple-value-bind (arguments options) (parse-options *ui*)
        (run arguments options)))

  "
  (invoke-restart (find-restart 'supply-new-value condition) value))


(defun parse-options (interface &optional (arguments (rest (argv))))
  "Parse `arguments` according to `interface`.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash table of option keys to values.

  See the full documentation for more information.

  "
  (let ((toplevel nil)
        (results (make-hash-table)))
    (initialize-results interface results)
    (labels
        ((recur (arguments)
           (if (null arguments)
             (values (nreverse toplevel)
                     (finalize-results interface results))
             (destructuring-bind (arg . remaining) arguments
               (recur
                 (restart-case
                     (cond
                       ((terminatorp arg) (dolist (r remaining) (push r toplevel)))
                       ((shortp arg) (parse-short interface results arg remaining))
                       ((longp arg) (parse-long interface results arg remaining))
                       (t (push arg toplevel) remaining))
                   (discard-option ()
                     :test unrecognized-option-p
                     :report "Discard the unrecognized option."
                     remaining)
                   (treat-as-argument ()
                     :test unrecognized-option-p
                     :report "Treat the unrecognized option as a plain argument."
                     (push arg toplevel)
                     remaining)
                   (supply-new-value (v)
                     :test unrecognized-option-p
                     :report "Supply a new value to parse."
                     :interactive (lambda () (list (read-line)))
                     (cons v remaining))))))))
      (recur arguments))))

(defun parse-options-or-exit (interface &optional (arguments (rest (argv))))
  "Parse `arguments` according to `interface`, exiting if any error occurs.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash table of option keys to values.

  If an error occurs while parsing the arguments, exits immediately as if with
  `adopt:print-error-and-exit`.

  See the full documentation for more information.

  "
  (handler-case (adopt:parse-options interface)
    (error (c) (adopt:print-error-and-exit c))))


;;;; Help ---------------------------------------------------------------------
(defun option-string (option)
  (let* ((long (long option))
         (short (short option))
         (parameter (parameter option))
         (parameter-string (if parameter
                             (format nil " ~A" parameter)
                             "")))
    (format nil "~{~A~^, ~}"
            (remove nil
                    (list (when short
                            (format nil "-~A~A" short parameter-string))
                          (when long
                            (format nil "--~A~A" long parameter-string)))))))

(defun print-option-help (stream option option-column doc-column doc-width)
  "Print `option`'s help to `stream`, indented/wrapped properly.

  Assumes the last thing printed to `stream` was a newline.

  The option string will start at `option-column`.  The help will start at
  `doc-column` and be line-wrapped to `doc-width`.

  "
  (let ((option-string (option-string option))
        (lines (bobbin:wrap (split-sequence:split-sequence
                              #\newline (help option))
                            doc-width))
        (col 0))
    (flet ((print-at (c string &optional newline)
             "Print `string` starting at column `c`, adding padding/newline if needed."
             (when (>= col c)
               (terpri stream)
               (setf col 0))
             (format stream "~vA~A" (- c col) #\space string)
             (if newline
               (progn (terpri stream)
                      (setf col 0))
               (setf col (+ c (length string))))))
      (print-at option-column option-string)
      (dolist (line lines)
        (print-at doc-column line t)))))

(defun print-help (interface &key
                   (stream *standard-output*)
                   (program-name (car (argv)))
                   (width 80)
                   (option-width 20)
                   (include-examples t))
  "Print a pretty help document for `interface` to `stream`.

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

  "
  (assert (> width (+ 2 option-width 2)) (width option-width)
    "WIDTH (~D) must be at least 4 greater than OPTION-WIDTH (~D)"
    width option-width)
  (format stream "~A - ~A~2%" (name interface) (summary interface))
  (format stream "USAGE: ~A ~A~2%" program-name (usage interface))
  (format stream (bobbin:wrap (help interface) width))
  (format stream "~%")
  (dolist (group (groups interface))
    (when (or (options group) (help group))
      (format stream "~%~A:~%" (or (title group) (name group) "Options"))
      (let* ((help (help group))
             (help-column 2)
             (help-width (- width help-column))
             (option-column 2)
             (option-padding 2)
             (doc-column (+ option-column option-width option-padding))
             (doc-width (- width doc-column)))
        (when help
          (format stream "~{  ~A~^~%~}~2%"
                  (bobbin:wrap (list help) help-width)))
        (dolist (option (options group))
          (print-option-help stream option option-column doc-column doc-width)))))
  (let* ((examples (examples interface))
         (example-column 2)
         (example-width (- width example-column)))
    (when (and examples include-examples)
      (format stream "~%Examples:~%")
      (loop :for (prose . command) :in examples :do
            (format stream "~%~{  ~A~^~%~}~2%      ~A~%"
                    (bobbin:wrap (list prose) example-width)
                    command)))))

(defun print-help-and-exit
    (interface &key
     (stream *standard-output*)
     (program-name (car (argv)))
     (width 80)
     (option-width 20)
     (include-examples t)
     (exit-function #'exit)
     (exit-code 0))
  "Print a pretty help document for `interface` to `stream` and exit.

  Handy for easily providing --help:

    (multiple-value-bind (arguments options) (parse-options *ui*)
      (when (gethash 'help options)
        (print-help-and-exit *ui*))
      (run arguments options))

  "
  (print-help interface
              :stream stream
              :program-name program-name
              :width width
              :option-width option-width
              :include-examples include-examples)
  (funcall exit-function exit-code))

(defun print-error-and-exit (error &key
                             (stream *error-output*)
                             (exit-function #'exit)
                             (exit-code 1)
                             (prefix "error: "))
  "Print `prefix` and `error` to `stream` and exit.

  Example:

    (handler-case
        (multiple-value-bind (arguments options) (parse-options *ui*)
          (run arguments options))
      (unrecognized-option (c)
        (print-error-and-exit c)))

  "
  (format stream "~A~A~%" (or prefix "") error)
  (funcall exit-function exit-code))


;;;; Man ----------------------------------------------------------------------
(defun escape (string)
  (if (zerop (length string))
    ""
    (with-output-to-string (s)
      (when (char= #\. (aref string 0)) ;; is this some kind of joke, troff?
        (write-string "\\[char46]" s)
        (setf string (subseq string 1)))
      (loop :for char :across string :do
            (when (find char "\\-")
              (write-char #\\ s))
            (write-char char s)))))

(defun split-paragraphs (string &key (delimiter ".PP") (escape t))
  (let ((lines (split-sequence:split-sequence #\newline string)))
    (when escape
      (setf lines (mapcar #'escape lines)))
    (substitute delimiter "" lines :test #'string=)))

(defun option-troff (option)
  (let ((short (short option))
        (long (long option))
        (parameter (parameter option)))
    (labels
        ((short-option ()
           (when short
             (if parameter
               (format nil "\\-~A \" \" \\fI~A\\fR" short parameter)
               (format nil "\\-~A" short))))
         (long-option ()
           (when long
             (if parameter
               (format nil "\\-\\-~A=\\fI~A\\fR" long parameter)
               (format nil "\\-\\-~A" long)))))
      (format nil ".BR ~{~A~^ \", \"~}"
              (remove nil (list (short-option) (long-option)))))))


(defun print-manual (interface &key
                     (stream *standard-output*)
                     (manual-section 1))
  "Print a troff-formatted man page for `interface` to `stream`.

  Example:

    (with-open-file (manual \"man/man1/foo.1\"
                            :direction :output
                            :if-exists :supersede)
      (print-manual *ui* manual))

  "
  (check-type manual-section (integer 1))
  (labels
      ((f (&rest args)
         (apply #'format stream args)
         (terpri stream))
       (fa (string-or-list)
         (if (listp string-or-list)
           (map nil #'fa string-or-list)
           (f "~A" string-or-list)))
       (print-header ()
         (f ".TH ~:@(~A~) ~D" (escape (name interface)) manual-section))
       (print-name ()
         (f ".SH NAME")
         (f "~A \\- ~A" (escape (name interface)) (escape (summary interface))))
       (print-synopsis ()
         (f ".SH SYNOPSIS")
         (f ".B ~A" (escape (name interface)))
         (unless (string= "" (usage interface))
           (f ".R ~A" (escape (usage interface)))))
       (print-description ()
         (f ".SH DESCRIPTION")
         (fa (split-paragraphs (or (manual interface)
                                   (help interface)))))
       (print-option (option)
         (f ".TP")
         (fa (option-troff option))
         (fa (split-paragraphs
               (or (manual option) (help option))
               :delimiter ".IP")))
       (print-group (group)
         (if (title group)
           (f ".SS ~A" (escape (title group)))
           (f ".SH OPTIONS"))
         (let ((desc (or (manual group)
                         (help group))))
           (when desc
             (fa (split-paragraphs desc))))
         (map nil #'print-option (options group)))
       (print-groups ()
         (map nil #'print-group (groups interface)))
       (print-example (prose command prefix)
         (f prefix)
         (fa (escape prose))
         (f ".PP")
         (f ".nf")
         (f ".RS")
         (fa command)
         (f ".RE")
         (f ".fi"))
       (print-examples ()
         (let ((examples (examples interface)))
           (loop :for prefix = ".SH EXAMPLES" :then ".PP"
                 :for (prose . command) :in examples
                 :do (print-example prose command prefix)))))
    (print-header)
    (print-name)
    (print-synopsis)
    (print-description)
    (print-groups)
    (print-examples)))
