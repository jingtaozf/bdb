;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: test.lisp
;; Description: test routines for bdb.
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 16:13:00(+0800)
;; Last-Updated: 2013.08.23 15:37:17(+0800)
;;     Update #: 21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :bdb-test)

(defvar *chars*
    (let ((chars (loop for (from to) in '((#\0 #\9)
                                          (#\a #\z)
                                          (#\A #\Z))
                      append (loop for i from (char-code from) to (char-code to)
                                   collect (code-char i)))))
      (make-array (length chars) :initial-contents chars)))
(defun random-string (&key (min 1) (max 256))
  (with-output-to-string (s)
    (dotimes (i (+ min (random (- max min))))
      (format s "~c" (aref *chars* (random (length *chars*)))))))

(register-store :test-hash #'(lambda (o) (loop for key being the hash-keys of o
                                          using (hash-value value)
                                          collect (cons key value)))
                #'(lambda (o) (loop with hash = (make-hash-table)
                               for (key . value) in o
                               do (setf (gethash key hash) value)
                               finally (return hash))))

(defun test-get-put (&key (times 16) (verbose t))
  (let ((db (db-create))
        (db-file "/tmp/bdb-get-put.db"))
    ;; (delete-file db-file)
    (db-open db :file db-file :type DB-BTREE :create t :mode 0)
    (when verbose
      (format t "db-put"))
    (loop with time = (get-universal-time)
          for i from 1 to times
          for key = (random-string)
          for value = (random-string :min 1024 :max (* 100 1024))
          do
       (when verbose
         (format t ".")
         (when (= 0 (mod i 8))
           (format t "~D(about ~D millseconds one loop)~%db-put" i (* (/ 1000 20) (- (get-universal-time) time)))
           (setf time (get-universal-time))))
       (db-put db key value)
       (db-put db key value)
       (assert (db-exists db key))
       (assert (string= value (db-get db key)))
       (when (= 0 (mod i 5))
         (db-delete db key)
         (assert (not (db-exists db key)))))
    (format t "...done~%")
    (db-close db)
    t))
(defun test-object-get-put (&key (verbose t))
  (let* ((env (denv-open "/tmp/"))
         (db (dopen "/tmp/bdb-object-get-put.db" env)))
    (when verbose
      (format t "test object store."))
    (dput db '(:a b) '(c d e))
    (assert (dexists db '(:a b)))
    (assert (equal '(c d e) (dget db '(:a b))))
    (when verbose
      (format t "~%"))

    (when verbose
      (format t "test store category."))
    (let ((hash (make-hash-table))
          (key '(:test hash)))
      (setf (gethash :测试 hash) #1="测试一下")
      (setf (gethash :ab hash) #2="测试ab")
      (dput db key hash :category :test-hash)
      (assert (dexists db key))
      (let ((stored-hash (dget db key :category :test-hash)))
        (assert (string= #1# (gethash :测试 stored-hash)))
        (assert (string= #2# (gethash :ab stored-hash))))
      (ddel db key)
      (assert (not (dexists db key))))
    (when verbose
      (format t "~%"))
    (dclose db)
    (db-env-close env)
    t))
(defun test-cursor (&key (verbose t))
  (let* ((env (denv-open "/tmp/"))
         (db (dopen "/tmp/bdb-cursor.db" env)))
    (when verbose
      (format t "test cursor."))
    (loop for i from 1 to 9
          do (dput db `(:a ,i) `(c "d" ,i e)))
    (loop with cursor = (db-cursor db)
          for i from 1
          do
       (multiple-value-bind (key value)
           (dcursor-get cursor)
         (unless key
           (loop-finish))
         (assert (equal key `(:a ,i)))
         (assert (equal value `(c "d" ,i e))))
       finally (db-cursor-close cursor)
               (assert (= i 10)))
    (when verbose
      (format t "done~%"))
    (dclose db)
    (db-env-close env)
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
      (run-test-suite (test-object-get-put :verbose verbose))
      (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
      successp)))
            
