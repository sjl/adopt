(in-package :adopt)

;;;; Utils --------------------------------------------------------------------
(defun append1 (list el)
  "Append element `el` to the end of `list`.

  This is implemented as `(append list (list el))`.  It is not particularly
  fast.

  It is useful as a `:reduce` function when you want to collect all values given
  for an option.

  "
  (append list (list el)))

(defun latest (old new)
  "Return `new`.

  It is useful as a `:reduce` function when you want to just keep the last-given
  value for an option.

  "
  (declare (ignore old))
  new)


(defun argv ()
  "Return a list of the program name and command line arguments.

  This is not implemented for every Common Lisp implementation.  You can always
  pass your own values to `parse-options` and `print-usage` if it's not
  implemented for your particular Lisp.

  "
  #+sbcl sb-ext:*posix-argv*
  #+ccl ccl:*unprocessed-command-line-arguments*
  #-(or sbcl ccl) (error "ARGV is not supported on this implementation."))


;;;; Definition ---------------------------------------------------------------
(defclass option ()
  ((name :initarg :name :accessor name%)
   (documentation :initarg :documentation :accessor documentation%)
   (short :initarg :short :accessor short%)
   (long :initarg :long :accessor long%)
   (parameter :initarg :parameter :accessor parameter%)
   (initial-value :initarg :initial-value :accessor initial-value%)
   (reduce :initarg :reduce :accessor reduce%)))

(defmethod print-object ((o option) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A ~A/~A" (name% o) (short% o) (long% o))))

(defun make-option (name documentation &key long short parameter initial-value reduce)
  (make-instance 'option
    :name name
    :documentation documentation
    :long long
    :short short
    :parameter parameter
    :initial-value initial-value
    :reduce reduce))


(defclass interface ()
  ((options :initarg :options :accessor options)
   (short-options :initarg :short-options :accessor short-options)
   (long-options :initarg :long-options :accessor long-options)
   (usage :initarg :usage :accessor usage)))

(defmethod print-object ((i interface) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~{~A~^ ~}"
            (mapcar (lambda (o)
                      (format nil "(~A ~A/~A)"
                              (name% o)
                              (short% o)
                              (long% o)))
                    (options i)))))

(defun make-interface (usage &rest options)
  (let ((interface (make-instance 'interface
                     :options nil
                     :usage usage
                     :short-options (make-hash-table)
                     :long-options (make-hash-table :test #'equal))))
    (dolist (option options)
      (push option (options interface))
      (let ((short (short% option))
            (long (long% option)))
        (when short
          (setf (gethash short (short-options interface)) option))
        (when long
          (setf (gethash long (long-options interface)) option))))
    (setf (options interface) (reverse (options interface)))
    interface))

(defmacro define-interface (name usage &rest options)
  `(defparameter ,name
     (make-interface ,usage
                     ,@(loop :for (name . args) :in options :collect
                             `(make-option ',name ,@args)))))


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


(defun parse-long (interface results arg remaining)
  (let* ((= (position #\= arg))
         (long-name (subseq arg 2 =))
         (option (gethash long-name (long-options interface)))
         (name (name% option))
         (current (gethash name results)))
    (setf (gethash name results)
          (if (parameter% option)
            (funcall (reduce% option) current
                     (if =
                       (subseq arg (1+ =))
                       (pop remaining)))
            (funcall (reduce% option) current))))
  remaining)

(defun parse-short (interface results arg remaining)
  (let* ((short-name (aref arg 1))
         (option (gethash short-name (short-options interface)))
         (name (name% option))
         (current (gethash name results)))
    (setf (gethash name results)
          (if (parameter% option)
            (funcall (reduce% option) current (if (> (length arg) 2)
                                                (subseq arg 2) ; -xfoo
                                                (pop remaining))) ; -x foo
            (prog1 (funcall (reduce% option) current)
              (if (> (length arg) 2)
                (push (format nil "-~A" (subseq arg 2)) remaining))))))
  remaining)


(defun parse-options (interface &optional (arguments (rest (argv))))
  "Parse `arguments` according to `interface`.

  Two values are returned:

  1. A fresh list of top-level, unaccounted-for arguments that don't correspond
     to any options defined in `interface`.
  2. An `EQL` hash map of option `name`s to values.

  See the full usage documentation for more information.

  "
  (let ((toplevel nil)
        (results (make-hash-table)))
    (dolist (option (options interface))
      (setf (gethash (name% option) results) (initial-value% option)))
    (labels
        ((recur (arguments)
           (if (null arguments)
             (values (reverse toplevel) results)
             (destructuring-bind (arg . remaining) arguments
               (recur (cond
                        ((terminatorp arg)
                         (dolist (r remaining) (push r toplevel))
                         nil)
                        ((shortp arg)
                         (parse-short interface results arg remaining))
                        ((longp arg)
                         (parse-long interface results arg remaining))
                        (t (push arg toplevel) remaining)))))))
      (recur arguments))))


;;;; Printing Usage -----------------------------------------------------------
(defun option-string (option)
  (let* ((long (long% option))
         (short (short% option))
         (parameter (parameter% option))
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
  "Print `option` to `stream`, indented/wrapped properly.

  Assumes the last thing printed to `stream` was a newline.

  The option string will start at `option-column`.  The documentation will start
  at `doc-column` and be line-wrapped to `doc-width`.

  "
  (let ((option-string (option-string option))
        (lines (bobbin:wrap (split-sequence:split-sequence
                              #\newline (documentation% option))
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
                    (option-width 20))
  "Print a pretty usage document for `interface` to `stream`.

  `width` should be the total width (in characters) for line-wrapping purposes.
  Care will be taken to ensure lines are no longer than this, though some edge
  cases (extremely long short/long option names and parameters) may slip
  through.

  `option-width` should be the width of the column of short/long options (in
  characters).  If the short/long option documentation is shorter than this, the
  option's documentation string will start on the same line.  Otherwise the
  option's documentation string will start on the next line.

  The result will look something like (assuming a usage string of
  `\"[options] FILES\"`):

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

  "
  (assert (> width (+ 2 option-width 2)) (width option-width)
    "WIDTH (~D) must be at least 4 greater than OPTION-WIDTH (~D)"
    width option-width)
  (format stream "USAGE: ~A ~A~2%Options:~%" program-name (usage interface))
  (let* ((option-column 2)
         (option-padding 2)
         (doc-column (+ option-column option-width option-padding))
         (doc-width (- width doc-column)))
    (dolist (option (options interface))
      (print-option-usage stream option option-column doc-column doc-width))))
