(in-package :adopt.test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(intern (concatenate 'string (symbol-name 'test-) (symbol-name name)))
    (let ((*package* ,*package*))
      ,@body)))


(defun run-tests ()
  (1am:run))


(defun hash-table-equal (h1 h2)
  (and (= (hash-table-count h1)
          (hash-table-count h2))
       (progn (maphash (lambda (k v)
                         (unless (equal v (gethash k h2))
                           (return-from hash-table-equal nil)))
                       h1)
              t)))

(defun result (&rest key-value-pairs)
  (loop :with result = (make-hash-table)
        :for (k v) :on key-value-pairs :by #'cddr
        :do (setf (gethash k result) v)
        :finally (return result)))

(defmacro check (interface input expected-args expected-result)
  (let ((args (gensym "ARGS"))
        (result (gensym "RESULT")))
    `(multiple-value-bind (,args ,result)
       (adopt:parse-options ,interface
                            (split-sequence:split-sequence
                              #\space ,input
                              :remove-empty-subseqs t))
       (is (equal ,expected-args ,args))
       (is (hash-table-equal ,expected-result ,result)))))


;;;; Tests --------------------------------------------------------------------
(defparameter *noop*
  (adopt:make-interface))

(defparameter *option-types*
  (adopt:make-interface
    :contents
    (list
      (adopt:make-option 'long
        :help "long only"
        :long "long"
        :reduce (constantly t))
      (adopt:make-option 'short
        :help "short only"
        :short #\s
        :reduce (constantly t))
      (adopt:make-option 'both
        :help "both short and long"
        :short #\b
        :long "both"
        :reduce (constantly t)))))

(defparameter *reducers*
  (adopt:make-interface
    :contents
    (list
      (adopt:make-option 'c1
        :help "1"
        :short #\1
        :reduce (constantly 1))
      (adopt:make-option 'c2
        :help "2"
        :short #\2
        :reduce (constantly 2))
      (adopt:make-option 'collect
        :help "collect"
        :short #\c
        :long "collect"
        :parameter "DATA"
        :reduce #'adopt:collect)
      (adopt:make-option 'last
        :help "last"
        :short #\l
        :long "last"
        :parameter "DATA"
        :reduce #'adopt:last)
      (adopt:make-option 'first
        :help "first"
        :short #\f
        :long "first"
        :parameter "DATA"
        :reduce #'adopt:first))))

(defparameter *same-key*
  (adopt:make-interface
    :contents
    (list
      (adopt:make-option '1
        :result-key 'foo
        :help "1"
        :short #\1
        :reduce (constantly 1))
      (adopt:make-option '2
        :result-key 'foo
        :help "2"
        :short #\2
        :reduce (constantly 2)))))

(defparameter *initial-value*
  (adopt:make-interface
    :contents
    (list
      (adopt:make-option 'foo
        :help "foo"
        :short #\f
        :long "foo"
        :initial-value "hello"
        :reduce (constantly "goodbye")))))

(defparameter *finally*
  (adopt:make-interface
    :contents
    (list
      (adopt:make-option 'yell
        :help "yell"
        :short #\y
        :long "yell"
        :parameter "VAL"
        :initial-value "default"
        :reduce #'adopt:last
        :finally #'string-upcase)
      (adopt:make-option 'a
        :help "ensure a"
        :short #\a
        :initial-value "x"
        :parameter "A"
        :reduce #'adopt:last
        :finally (lambda (a)
                   (assert (string= "a" a))
                   :ok)))))

(defparameter *keys*
  (adopt:make-interface
    :contents
    (list
      (adopt:make-option 'int
        :help "int"
        :short #\i
        :long "int"
        :parameter "K"
        :reduce #'adopt:collect
        :key #'parse-integer)
      (adopt:make-option 'len
        :help "len"
        :short #\l
        :long "len"
        :parameter "K"
        :reduce #'adopt:collect
        :key #'length))))


(define-test noop
  (check *noop* ""
         '()
         (result))
  (check *noop* "foo"
         '("foo")
         (result))
  (check *noop* "a b c foo a"
         '("a" "b" "c" "foo" "a")
         (result)))

(define-test option-types
  (check *option-types* "foo -s bar"
         '("foo" "bar")
         (result 'short t
                 'long nil
                 'both nil))
  (check *option-types* "foo --long bar"
         '("foo" "bar")
         (result 'short nil
                 'long t
                 'both nil))
  (check *option-types* "foo --both bar"
         '("foo" "bar")
         (result 'short nil
                 'long nil
                 'both t))
  (check *option-types* "foo -b bar"
         '("foo" "bar")
         (result 'short nil
                 'long nil
                 'both t))
  (check *option-types* "foo -bs --long bar"
         '("foo" "bar")
         (result 'short t
                 'long t
                 'both t)))

(define-test reducers
  (check *reducers* ""
         '()
         (result 'c1 nil
                 'c2 nil
                 'first nil
                 'last nil
                 'collect nil))

  (check *reducers* "here we -2 -2 --collect a -c b go --collect c -1"
         '("here" "we" "go")
         (result 'c1 1
                 'c2 2
                 'first nil
                 'last nil
                 'collect '("a" "b" "c")))

  (check *reducers* "foo -f 1 -f 2 --last 1 --first 3 --last 2 -l 3 bar"
         '("foo" "bar")
         (result 'c1 nil
                 'c2 nil
                 'first "1"
                 'last "3"
                 'collect nil)))

(define-test same-key
  (check *same-key* ""
         '()
         (result 'x nil))
  (check *same-key* "-1"
         '()
         (result 'foo 1))
  (check *same-key* "-2"
         '()
         (result 'foo 2))
  (check *same-key* "-1121"
         '()
         (result 'foo 1)))

(define-test initial-value
  (check *initial-value* ""
         '()
         (result 'foo "hello"))
  (check *initial-value* "x"
         '("x")
         (result 'foo "hello"))
  (check *initial-value* "x --foo y"
         '("x" "y")
         (result 'foo "goodbye")))

(define-test keys
  (check *keys* ""
         '()
         (result 'len '()
                 'int '()))
  (check *keys* "--int 123 --int 0 --len abc --len 123456"
         '()
         (result 'int '(123 0)
                 'len '(3 6))))

(define-test finally
  (check *finally* "-a a"
         '()
         (result 'yell "DEFAULT" 'a :ok))
  (check *finally* "-y foo -y bar -a x -a a"
         '()
         (result 'yell "BAR" 'a :ok)))

(define-test helpers
  (is (equal :old (adopt:first :old :new)))
  (is (equal :new (adopt:last :old :new)))
  (is (equal '(:a) (adopt:collect '() :a)))
  (is (equal '(:a :b) (adopt:collect '(:a) :b)))
  (is (equal '(2 . 1) (funcall (adopt:flip 'cons) 1 2))))
