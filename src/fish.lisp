(in-package :adopt)

(defun escape/shell (string)
  (if (zerop (length string))
    ""
    (with-output-to-string (s)
      (loop :for char :across string :do
            (when (find char "'")
              (write-char #\\ s))
            (write-char char s)))))

(defun print-fish-completions (interface &key
                               (stream *standard-output*)
                               (program-name (name interface)))
  (labels ((f (&rest args)
             (write-string " " stream)
             (apply #'format stream args)))
    (dolist (o (options interface))
      (format stream "complete -c ~A" program-name)
      (let ((long (long o))
            (short (short o))
            (param (parameter o))
            (help (or (terse o) (help o))))
        (when long (f "-l ~A" long))
        (when short (f "-s ~A" short))
        (when param (f "-ra ~A" param))
        (when help (f "-d '~A'" (escape/shell help)))
        (terpri stream)))))
