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

(defun newest (old new)
  "Return `new`.

  It is useful as a `:reduce` function when you want to just keep the last-given
  value for an option.

  "
  (declare (ignore old))
  new)

(defun oldest (old new)
  "Return `new` if `old` is `nil`, otherwise return `old`.

  It is useful as a `:reduce` function when you want to just keep the
  first-given value for an option.

  "
  (if (null old)
    new
    old))


(defun argv ()
  "Return a list of the program name and command line arguments.

  This is not implemented for every Common Lisp implementation.  You can always
  pass your own values to `parse-options` and `print-usage` if it's not
  implemented for your particular Lisp.

  "
  #+sbcl sb-ext:*posix-argv*
  #+ccl ccl:*unprocessed-command-line-arguments*
  #-(or sbcl ccl) (error "ARGV is not supported on this implementation."))

(defun exit (&optional (code 0))
  #+sbcl (sb-ext:exit :code code)
  #-(or sbcl) (error "EXIT is not supported on this implementation."))


(defun funcall% (value function)
  (funcall function value))

(define-modify-macro funcallf (function) funcall%)


(defmacro define-string (var string &rest args)
  "Convenience macro for `(defparameter ,var (format nil ,string ,@args))`."
  `(defparameter ,var (format nil ,string ,@args)))


;;;; Definition ---------------------------------------------------------------
(defclass option ()
  ((name :initarg :name :accessor name)
   (documentation :initarg :documentation :accessor documentation)
   (manual :initarg :manual :accessor manual)
   (short :initarg :short :accessor short)
   (long :initarg :long :accessor long)
   (parameter :initarg :parameter :accessor parameter)
   (initial-value :initarg :initial-value :accessor initial-value)
   (key :initarg :key :accessor key)
   (result-key :initarg :result-key :accessor result-key)
   (finally :initarg :finally :accessor finally)
   (reducer :initarg :reduce :accessor reducer)))

(defmethod print-object ((o option) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A ~A/~A" (name o) (short o) (long o))))

(defun make-option (name result-key &key
                    long
                    short
                    documentation
                    manual
                    parameter
                    reduce
                    (initial-value nil initial-value?)
                    (key #'identity)
                    (finally #'identity))
  (when (and (null long) (null short))
    (error "Option ~A requires at least one of :long/:short." name))
  (when (null reduce)
    (error "Option ~A is missing required argument :reduce." name))
  (when (null documentation)
    (error "Option ~A is missing required argument :documentation" name))
  (when (and (member reduce (list 'collect #'collect
                                  'newest #'newest
                                  'oldest #'oldest))
             (null parameter))
    (error "Option ~A has reduce function ~A, which requires a :parameter."
           name reduce))
  (apply #'make-instance 'option
         :name name
         :result-key result-key
         :documentation documentation
         :manual manual
         :long long
         :short short
         :parameter parameter
         :reduce reduce
         :key key
         :finally finally
         (when initial-value?
           (list :initial-value initial-value))))


(defclass interface ()
  ((name :initarg :name :accessor name)
   (summary :initarg :summary :accessor summary)
   (examples :initarg :examples :accessor examples)
   (options :initarg :options :accessor options)
   (short-options :initarg :short-options :accessor short-options)
   (long-options :initarg :long-options :accessor long-options)
   (usage :initarg :usage :accessor usage)
   (documentation :initarg :documentation :accessor documentation)))

(defmethod print-object ((i interface) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~{~A~^ ~}"
            (mapcar (lambda (o)
                      (format nil "(~A ~A/~A)"
                              (name o)
                              (short o)
                              (long o)))
                    (options i)))))

(defun make-interface (&key name summary usage documentation examples options)
  (let ((interface (make-instance 'interface
                     :options nil
                     :name name
                     :usage usage
                     :summary summary
                     :documentation documentation
                     :examples examples
                     :short-options (make-hash-table)
                     :long-options (make-hash-table :test #'equal))))
    (dolist (option options)
      (push option (options interface))
      (let ((short (short option))
            (long (long option)))
        (when short
          (setf (gethash short (short-options interface)) option))
        (when long
          (setf (gethash long (long-options interface)) option))))
    (funcallf (options interface) #'nreverse)
    interface))


(defun resolve-names (name)
  (etypecase name
    (symbol (list name name))
    ((cons symbol null)
     (list (first name) (first name)))
    ((cons symbol (cons symbol null))
     name)))

(defmacro define-interface
    (symbol (&key name summary usage documentation examples) &body options)
  `(defparameter ,symbol
     (make-interface
       :name ,name
       :summary ,summary
       :usage ,usage
       :documentation ,documentation
       :examples ,examples
       :options
       (list
         ,@(loop
             :for (name-and-result-key . args) :in options
             :for (option-name result-key) = (resolve-names name-and-result-key)
             :collect `(make-option ',option-name ',result-key ,@args))))))


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
                (funcall (reducer option) current param))
              (funcall (reducer option) current)))))
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
                (funcall (reducer option) current param))
              (prog1 (funcall (reducer option) current)
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
  2. An `EQL` hash table of option `name`s to values.

  See the full usage documentation for more information.

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


;;;; Usage --------------------------------------------------------------------
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

(defun print-option-usage (stream option option-column doc-column doc-width)
  "Print `option`'s usage to `stream`, indented/wrapped properly.

  Assumes the last thing printed to `stream` was a newline.

  The option string will start at `option-column`.  The documentation will start
  at `doc-column` and be line-wrapped to `doc-width`.

  "
  (let ((option-string (option-string option))
        (lines (bobbin:wrap (split-sequence:split-sequence
                              #\newline (documentation option))
                            doc-width))
        (col 0))
    (flet ((print-at (c string &optional newline)
             "Print `string` starting at column `c`, adding padding/newline if needed."
             (when (> col c)
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

(defun print-usage (interface &key
                    (stream *standard-output*)
                    (program-name (first (argv)))
                    (width 80)
                    (option-width 20)
                    (include-examples t))
  "Print a pretty usage document for `interface` to `stream`.

  `width` should be the total width (in characters) for line-wrapping purposes.
  Care will be taken to ensure lines are no longer than this, though some edge
  cases (extremely long short/long option names and parameters) may slip
  through.

  `option-width` should be the width of the column of short/long options (in
  characters).  If the short/long option documentation is shorter than this, the
  option's documentation string will start on the same line.  Otherwise the
  option's documentation string will start on the next line.

  The result will look something like:

    (print-usage *program-interface* :width 60 :option-width 15)
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
  (format stream (bobbin:wrap (documentation interface) width))
  (format stream "~2%Options:~%")
  (let* ((option-column 2)
         (option-padding 2)
         (doc-column (+ option-column option-width option-padding))
         (doc-width (- width doc-column))
         (examples (examples interface))
         (example-column 2)
         (example-width (- width example-column)))
    (dolist (option (options interface))
      (print-option-usage stream option option-column doc-column doc-width))
    (when (and examples include-examples)
      (format stream "~%Examples:~%")
      (loop :for (prose . command) :in examples :do
            (format stream "~%~{  ~A~^~%~}~2%      ~A~%"
                    (bobbin:wrap (list prose) example-width)
                    command)))))

(defun print-usage-and-exit
    (interface &key
     (stream *standard-output*)
     (program-name (first (argv)))
     (width 80)
     (option-width 20)
     (include-examples t)
     (exit-code 0))
  "Print a pretty usage document for `interface` to `stream` and exit.

  Handy for easily providing --help:

    (multiple-value-bind (arguments options) (parse-options *ui*)
      (when (gethash 'help options)
        (print-usage-and-exit *ui*))
      (run arguments options))
  "
  (print-usage interface
               :stream stream
               :program-name program-name
               :width width
               :option-width option-width
               :include-examples include-examples)
  (exit exit-code))

(defun print-error-and-exit (error &key
                             (stream *error-output*)
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
  (adopt:exit exit-code))


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
         (fa (split-paragraphs (documentation interface))))
       (print-option (option)
         (f ".TP")
         (fa (option-troff option))
         (fa (split-paragraphs
               (or (manual option) (documentation option))
               :delimiter ".IP")))
       (print-options ()
         (f ".SH OPTIONS")
         (map nil #'print-option (options interface)))
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
    (print-options)
    (print-examples)))
