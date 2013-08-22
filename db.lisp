;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: db.lisp
;; Description: fli db function wrappers.
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 16:13:00(+0800)
;; Last-Updated: 2013.08.22 16:17:23(+0800)
;;     Update #: 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :bdb)


;;;; declaim optimize
(declaim (optimize (speed 3) (safety 0)))

;;;; Environment

(defun db-env-create ()
  "Create an environment handle."
  (multiple-value-bind (env errno)
      (%db-env-create 0)
    (declare (type fixnum errno))
    (if (= errno 0)
	env
	(error 'bdb-db-error :errno errno))))

(defun db-env-close (dbenvp)
  "Close an environment handle."
  (with-cstrings NIL
    (with-db-errno nil
        (%db-env-close dbenvp 0))))

(defun db-env-open
    (dbenvp home &key auto-commit init-cdb init-lock init-log
            init-mpool init-rep init-txn recover recover-fatal create
            lockdown private system-mem thread (mode 416))
  "Open an environment handle."
  (with-cstring (home home)
    (with-db-errno nil
        (%db-env-open dbenvp
                      home
                      (flags :auto-commit auto-commit :init-cdb init-cdb :init-lock init-lock :init-log
                             init-log :init-mpool init-mpool :init-rep init-rep :init-txn init-txn
                             :recover recover :recover-fatal recover-fatal :create create
                             :lockdown lockdown :private private :system-mem system-mem :thread thread)
                      mode))))

(defun db-env-dbremove
    (env file &key auto-commit
         (transaction (txn-default *current-transaction*))
         (database +null-char+))
  "Remove a database."
  (with-cstrings ((file file) (database database))
    (with-db-errno transaction
        (%db-env-dbremove env
                          transaction
                          file
                          database
                          (flags :auto-commit
                                 auto-commit)))))

(defun db-env-dbrename
       (env file newname &key auto-commit
        (transaction (txn-default *current-transaction*))
        (database +null-char+))
  "Rename an environment."
  (with-cstrings ((file file) (database database) (newname newname))
    (with-db-errno transaction
        (%db-env-dbrename env
                          transaction
                          file
                          database
                          newname
                          (flags :auto-commit
                                 auto-commit)))))

(defun db-env-set-error-file (env filename)
 (with-cstrings ((fname filename))
   (%db-env-set-error-file env fname)))

(defun db-env-set-data-dir (env filename)
 (with-cstrings ((fname filename))
   (%db-env-set-data-dir env fname)))


;;;; Database

(defun db-create (&optional (dbenv +NULL-VOID+))
  "Create a DB handle."
  (multiple-value-bind (db errno)
      (%db-create dbenv 0)
    (declare (type fixnum errno))
    (if (= errno 0)
	db
	(error 'bdb-db-error :errno errno))))

(defun db-get-flags (db)
  "Get flags on a DB handle."
  (multiple-value-bind (errno flags)
      (%db-get-flags db)
    (if (= errno 0) flags
	(error 'bdb-db-error :errno errno))))

(defun db-set-error-file (db filename)
 (with-cstrings ((fname filename))
   (%db-set-error-file db fname)))


;;;; utf-8 support
(defun string-to-buffer-stream (string)
  (declare (type string string))
  (let* ((buffer (flexi-streams:string-to-octets string :external-format :utf-8))
         (buf-len (length buffer))
         (stream (make-buffer-stream :buffer (allocate-foreign-object :unsigned-char buf-len) :length buf-len)))
    (loop for c across buffer
          do (buffer-write-byte c stream))
    stream))
(defun buffer-stream-to-string (stream)
  (declare (type buffer-stream stream))
  (loop with buf = (make-array (buffer-stream-size stream) :element-type 'flexi-streams:octet)
        for i from 0 to (1- (buffer-stream-size stream))
        do (setf (aref buf i) (buffer-read-byte stream))
        finally (return (flexi-streams:octets-to-string buf :external-format :utf-8))))

;;;; Accessors

(defun db-get (db key &key (transaction (txn-default *current-transaction*))
                  get-both degree-2 read-committed
                  dirty-read read-uncommitted)
  "Get a key / value pair from a DB.  The key is passed as a
string, and the value is returned as a string.  If nothing
is found, NIL is returned."
  (declare (type pointer-void db transaction)
	   (type string key)
	   (type boolean get-both degree-2 read-committed
		 dirty-read read-uncommitted))
  (let* ((key-buffer-stream (string-to-buffer-stream key))
         (value-buffer-stream (make-buffer-stream)))
    (declare (type buffer-stream key-buffer-stream value-buffer-stream))
    (loop for value-length fixnum = (buffer-stream-length value-buffer-stream)
          do
       (multiple-value-bind (errno result-size)
	   (%db-get-key-buffered db transaction
                                 (buffer-stream-buffer key-buffer-stream)
                                 (buffer-stream-size key-buffer-stream)
                                 (buffer-stream-buffer value-buffer-stream)
                                 value-length
                                 (flags :get-both get-both
                                        :degree-2 (or degree-2 read-committed)
                                        :dirty-read (or dirty-read read-uncommitted)))
	 (declare (type fixnum result-size errno))
	 (cond
	   ((= errno 0)
            (setf (buffer-stream-size value-buffer-stream) result-size)
	    (return-from db-get
              (prog1
                (buffer-stream-to-string value-buffer-stream)
                (cleanup-buffer-stream key-buffer-stream)
                (cleanup-buffer-stream value-buffer-stream))))
	   ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
            (cleanup-buffer-stream key-buffer-stream)
            (cleanup-buffer-stream value-buffer-stream)
	    (return-from db-get nil))
	   ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
            (cleanup-buffer-stream key-buffer-stream)
            (cleanup-buffer-stream value-buffer-stream)
	    (throw 'transaction transaction))
	   ((> result-size value-length)
	    (resize-buffer-stream-no-copy value-buffer-stream result-size))
	   (t
            (cleanup-buffer-stream key-buffer-stream)
            (cleanup-buffer-stream value-buffer-stream)
            (error 'bdb-db-error :errno errno)))))))

(defun db-put (db key value &key (transaction (txn-default *current-transaction*)) exists-error-p)
  "Put a key / value pair into a DB.  The pair are strings."
  (declare (type pointer-void db transaction)
           (type boolean exists-error-p)
           (type string key value))
  (let* ((key-buffer-stream (string-to-buffer-stream key))
         (value-buffer-stream (string-to-buffer-stream value)))
    (declare (type buffer-stream key-buffer-stream value-buffer-stream))
    (let ((errno
           (%db-put-buffered db transaction
                             (buffer-stream-buffer key-buffer-stream)
                             (buffer-stream-size key-buffer-stream)
                             (buffer-stream-buffer value-buffer-stream)
                             (buffer-stream-size value-buffer-stream)
                             0)))
      (declare (type fixnum errno))
      (cleanup-buffer-stream key-buffer-stream)
      (cleanup-buffer-stream value-buffer-stream)
      (cond ((= errno 0) t)
            ((and (= errno DB_KEYEXIST) (not exists-error-p))
             nil)
            ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
             (throw 'transaction transaction))
            (t (error 'bdb-db-error :errno errno))))))

(defun db-exists (db key &key (transaction (txn-default *current-transaction*)))
  "check whether a key exist in a DB.  The key is a
string.  T on exist, NIL if the key wasn't found."
  (declare (type pointer-void db transaction) (type string key))
  (let* ((key-buffer-stream (string-to-buffer-stream key)))
    (declare (type buffer-stream key-buffer-stream))
    (let ((errno (%db-exists db transaction
                             (buffer-stream-buffer key-buffer-stream)
                             (buffer-stream-size key-buffer-stream)
                             0)))
      (declare (type fixnum errno))
      (cleanup-buffer-stream key-buffer-stream)
      (cond ((= errno 0) t)
            ((or (= errno DB_NOTFOUND)
                 (= errno DB_KEYEMPTY))
             nil)
            ((or (= errno DB_LOCK_DEADLOCK)
                 (= errno DB_LOCK_NOTGRANTED))
             (throw 'transaction transaction))
            (t (error 'bdb-db-error :errno errno))))))

(defun db-delete (db key &key (transaction (txn-default *current-transaction*)))
  "Delete a key / value pair from a DB.  The key is a
string.  T on success, NIL if the key wasn't found."
  (declare (type pointer-void db transaction) (type string key))
  (let* ((key-buffer-stream (string-to-buffer-stream key)))
    (declare (type buffer-stream key-buffer-stream))
    (let ((errno
           (%db-delete-buffered db transaction
                                (buffer-stream-buffer key-buffer-stream)
                                (buffer-stream-size key-buffer-stream)
                       0)))
      (declare (type fixnum errno))
      (cleanup-buffer-stream key-buffer-stream)
      (cond ((= errno 0) t)
            ((or (= errno DB_NOTFOUND)
                 (= errno DB_KEYEMPTY))
             nil)
            ((or (= errno DB_LOCK_DEADLOCK)
                 (= errno DB_LOCK_NOTGRANTED))
             (throw 'transaction transaction))
            (t (error 'bdb-db-error :errno errno))))))

;;;; Compaction for BDB 4.4

(defun db-compact (db start stop end &key (transaction (txn-default *current-transaction*))
		   freelist-only free-space)
  (declare (type pointer-void db transaction)
	   (type boolean freelist-only free-space))
  (loop
       for end-length fixnum = (buffer-stream-length end)
       do
	 (multiple-value-bind (errno end-size)
	     (%db-compact db transaction
			  (if start (buffer-stream-buffer start) 0)
			  (if start (buffer-stream-size start) 0)
			  (if stop (buffer-stream-buffer stop) 0)
			  (if stop (buffer-stream-size stop) 0)
			  (flags :freelist-only freelist-only :free-space free-space)
			  (buffer-stream-buffer end)
			  (buffer-stream-length end))
	   (declare (type fixnum errno end-size))
	   (cond ((= errno 0)
		  (setf (buffer-stream-size end) end-size)
		  (return-from db-compact (the buffer-stream end)))
		 ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
		  (return-from db-compact nil))
		 ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
		  (throw 'transaction transaction))
		 ((> end-size end-length)
		  (resize-buffer-stream-no-copy end end-size))
		 (t (error 'bdb-db-error :errno errno))))))
