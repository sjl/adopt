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
(define-interface *noop* "usage")
(define-interface *option-types* "usage"
  (short
    "short-only"
    :short #\s
    :reduce (constantly t))
  (long
    "long-only"
    :long "long"
    :reduce (constantly t))
  (both
    "both short and long"
    :short #\b
    :long "both"
    :reduce (constantly t)))

(define-interface *reducers* "usage"
  (c1
    "constantly 1"
    :short #\1
    :reduce (constantly 1))
  (c2
    "constantly 2"
    :short #\2
    :reduce (constantly 2))
  (snoc
    "snoc"
    :long "snoc"
    :parameter "FOO"
    :reduce (lambda (l el)
              (cons el l))))

(define-interface *same-name* "usage"
  (x
    "constantly 1"
    :short #\1
    :reduce (constantly 1))
  (x
    "constantly 2"
    :short #\2
    :reduce (constantly 2)))

(define-interface *initial-value* "usage"
  (foo
    "foo"
    :long "foo"
    :initial-value "hello"
    :reduce (constantly "goodbye")))

(define-interface *keys* "keys"
  (num
    "num"
    :long "num"
    :parameter "NUMBER"
    :initial-value nil
    :key #'parse-integer
    :reduce (flip #'cons))
  (len
    "len"
    :long "len"
    :parameter "STRING"
    :initial-value nil
    :key #'length
    :reduce (flip #'cons)))

(define-interface *finally* "finally"
  (yell
    "yell"
    :short #\y
    :parameter "STRING"
    :initial-value "default"
    :reduce #'newest
    :finally #'string-upcase)
  (a
    "ensure a"
    :short #\a
    :parameter "STRING"
    :initial-value "x"
    :reduce #'newest
    :finally (lambda (a)
               (assert (string= "a" a))
               :ok)))

(define-interface *parameters* "usage"
  (no-param
    "no parameter"
    :long "no-param"
    :initial-value 0
    :reduce #'1+)
  (param
    "one parameter"
    :long "param"
    :parameter "P"
    :reduce #'(lambda (old new) old new)))

(define-interface *helpers* "usage"
  (oldest
    "oldest"
    :short #\o
    :parameter "X"
    :reduce #'oldest)
  (newest
    "newest"
    :short #\n
    :parameter "X"
    :reduce #'newest)
  (collect
    "collect"
    :short #\c
    :parameter "X"
    :reduce #'collect)
  (flip
    "flip"
    :short #\f
    :parameter "X"
    :reduce (flip #'cons)))


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
                 'snoc nil))
  (check *reducers* "here we --snoc 1 --snoc 2 go -2 --snoc 3 -1"
         '("here" "we" "go")
         (result 'c1 1
                 'c2 2
                 'snoc '("3" "2" "1"))))

(define-test same-name
  (check *same-name* ""
         '()
         (result 'x nil))
  (check *same-name* "-1"
         '()
         (result 'x 1))
  (check *same-name* "-2"
         '()
         (result 'x 2))
  (check *same-name* "-1121"
         '()
         (result 'x 1)))

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
                 'num '()))
  (check *keys* "--num 123 --num 0 --len abc --len 123456"
         '()
         (result 'num '(0 123)
                 'len '(6 3))))

(define-test finally
  (check *finally* "-a a"
         '()
         (result 'yell "DEFAULT" 'a :ok))
  (check *finally* "-y foo -y bar -a x -a a"
         '()
         (result 'yell "BAR" 'a :ok)))

(define-test parameters
  (check *parameters* ""
         '()
         (result 'no-param 0
                 'param nil))
  (check *parameters* "--no-param foo"
         '("foo")
         (result 'no-param 1
                 'param nil))
  (check *parameters* "--param foo"
         '()
         (result 'no-param 0
                 'param "foo"))
  (check *parameters* "--no-param --param foo --no-param --param bar baz"
         '("baz")
         (result 'no-param 2
                 'param "bar")))

(define-test helpers
  (check *helpers* ""
         '()
         (result 'oldest nil
                 'newest nil
                 'collect nil
                 'flip nil))
  (check *helpers* "-o1 -o2 -n1 -n2 -c1 -c2 -f1 -f2"
         '()
         (result 'oldest "1"
                 'newest "2"
                 'collect '("1" "2")
                 'flip '("2" "1"))))
