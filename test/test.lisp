;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BDB-TEST; Base: 10 -*-

(in-package :bdb-test)

(defvar *chars*
    (let ((chars (loop for (from to) in '((#\0 #\9)
                                          (#\a #\z)
                                          (#\A #\Z))
                      append (loop for i from (char-code from) to (char-code to)
                                   collect (code-char i)))))
      (make-array (length chars) :initial-contents chars)))
(defun random-string (&key (min 1) (max 1024))
  (with-output-to-string (s)
    (dotimes (i (+ min (random (- max min))))
      (format s "~c" (aref *chars* (random (length *chars*)))))))

(defun test-get-put (&key (verbose t))
  (let ((db (db-create))
        (db-file "/tmp/bdb-get-put.db"))
    ;; (delete-file db-file)
    (db-open db :file db-file :type DB-BTREE :create t :mode 0)
    (when verbose
      (format t "db-put"))
    (loop with time = (get-universal-time)
          for i from 1 to 256
          for key = (random-string)
          for value = (random-string :min 1024 :max (* 1024 1024))
          do
       (when verbose
         (format t ".")
         (when (= 0 (mod i 20))
           (format t "~D(about ~D millseconds one loop)~%db-put" i (* (/ 1000 20) (- (get-universal-time) time)))
           (setf time (get-universal-time))))
       (db-put db key value)
       (db-put db key value)
       (assert (db-exists db key))
       (assert (string= value (db-get db key)))
       (when (= 0 (mod i 5))
         (db-delete db key)
         (assert (not (db-exists db key)))))
    (format t "done")
    (db-close db)
    t))


(defun run-all-tests (&key (verbose t))
  "Runs all tests for BDB and returns a true value iff all
tests succeeded.  VERBOSE is interpreted by the individual test suites
above."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      (run-test-suite (test-get-put :verbose verbose))
      (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
      successp)))
            
