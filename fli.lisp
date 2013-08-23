;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: fli.lisp
;; Description: foreign language interface
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.06.14 11:20:05(+0800)
;; Last-Updated:
;;     Update #: 71
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
(in-package :bdb)

(defvar *current-transaction* nil)

;;;; load library
(defvar *load-library-p* nil)
#+lispworks(defvar *reloaded-times* 0)
(defun load-library-if-necessary ()
  (unless *load-library-p*
    (let ((root (namestring (asdf:component-pathname (asdf:find-system "bdb")))))
      (asdf:run-shell-command
       (format nil "gcc -shared -fPIC -ldb-4.8 -o \"~alibbdb.so\" \"~alibbdb.c\"" root root))
      (uffi:load-foreign-library (format nil "~alibbdb.so" root) :module :libbdb)
      (setf *load-library-p* t))))
(load-library-if-necessary)

#+lispworks
(defun reload-library ()
  (%close-foreign-library :libbdb)
  (setf *load-library-p* nil)
  (load-library-if-necessary))

;;;; utilities.

(eval-when (:compile-toplevel :load-toplevel)
  (let (#+lispworks(lw:*handle-warn-on-redefinition* :quiet))
    (def-type pointer-int (* :int))
    (def-type pointer-void :pointer-void)
    (def-foreign-type array-or-pointer-char
        #+(or allegro) (:array :unsigned-char)
        #+(or cmu sbcl scl openmcl lispworks) (* :unsigned-char))
    (def-type array-or-pointer-char array-or-pointer-char)))

;; Constants and Flags
;; eventually write a macro which generates a custom flag function.
(defvar +NULL-VOID+ (make-null-pointer :void)
  "A null pointer to a void type.")
(defvar +NULL-CHAR+ (make-null-pointer :char)
  "A null pointer to a char type.")
;; Standard utility for copying two foreign buffers --
;;   also to test that lib is actually loaded!
(def-function ("copy_buf" copy-bufs)
        ((dest array-or-pointer-char)
         (dest-offset :int)
         (src array-or-pointer-char)
         (src-offset :int)
         (length :int))
      :returning :void)

;;;; db error
(eval-when (:compile-toplevel :load-toplevel)

  (def-function ("db_strerr" %db-strerror)
      ((error :int))
    :returning :cstring)

  (defun db-strerror (errno)
    "Get the string error associated with an error number."
    (convert-from-cstring (%db-strerror errno)))

  (define-condition bdb-db-error (error)
    ((errno :type fixnum :initarg :errno :reader db-error-errno))
    (:report
     (lambda (condition stream)
       (declare (type bdb-db-error condition) (type stream stream))
       (format stream "Berkeley DB error: ~A"
	       (db-strerror (db-error-errno condition)))))
    (:documentation "Berkeley DB errors."))

  )

(defmacro txn-default (dvar)
  `(progn
     (assert (null ,dvar))
     +NULL-VOID+))

;;;; Constants and Flags
;; eventually write a macro which generates a custom flag function.
;;

;; Current header file version required: Berkeley DB 4.8

;; I don't like the UFFI syntax for enumerations
(defconstant DB-BTREE                 1)
(defconstant DB-HASH                  2)
(defconstant DB-RECNO                 3)
(defconstant DB-QUEUE                 4)
(defconstant DB-UNKNOWN               5)

(defconstant DB_LOCK_NOWAIT   #x00000001)

(defconstant DB_CREATE        #x00000001)
(defconstant DB_FORCE         #x00000001)
(defconstant DB_MULTIVERSION  #x00000004)
(defconstant DB_NOMMAP        #x00000008)
(defconstant DB_RDONLY        #x00000400)
(defconstant DB_RECOVER       #x00000002)
(defconstant DB_THREAD        #x00000010)
(defconstant DB_TRUNCATE      #x00004000)
(defconstant DB_TXN_NOSYNC    #x00000001)
(defconstant DB_TXN_NOT_DURABLE #x00000002)
(defconstant DB_TXN_WRITE_NOSYNC #x00000020)

(defconstant DB_EXCL          #x00000040)

(defconstant DB_TXN_NOWAIT    #x00000010)
(defconstant DB_TXN_SYNC      #x00000004)

(defconstant DB_DUP           #x00000010)
(defconstant DB_DUPSORT       #x00000004)

(defconstant DB_JOINENV          #x00000000)
(defconstant DB_INIT_CDB         #x00000040)
(defconstant DB_INIT_LOCK        #x00000080)
(defconstant DB_INIT_LOG         #x00000100)
(defconstant DB_INIT_MPOOL       #x00000200)
(defconstant DB_INIT_REP         #x00000400)
(defconstant DB_INIT_TXN         #x00000800)
(defconstant DB_LOCKDOWN         #x00001000)
(defconstant DB_PRIVATE          #x00002000)
(defconstant DB_RECOVER_FATAL    #x00004000)
(defconstant DB_REGISTER         #x00008000)
(defconstant DB_SYSTEM_MEM       #x00010000)
(defconstant DB_AUTO_COMMIT      #x00000100)
(defconstant DB_READ_COMMITTED   #x00000400)
(defconstant DB_DEGREE_2         #x00000400) ;; DEPRECATED, now called DB_READ_COMMITTED
(defconstant DB_READ_UNCOMMITTED #x00000200)
(defconstant DB_DIRTY_READ       #x00000200) ;; DEPRECATED, now called DB_READ_UNCOMMITTED

(defconstant DB_AFTER		      1)
(defconstant DB_BEFORE		      3)
(defconstant DB_CURRENT		      6)
(defconstant DB_FIRST		      7)
(defconstant DB_GET_BOTH	      8)
(defconstant DB_GET_BOTH_RANGE	     10)
(defconstant DB_KEYFIRST	     13)
(defconstant DB_KEYLAST		     14)
(defconstant DB_LAST		     15)
(defconstant DB_NEXT		     16)
(defconstant DB_NEXT_DUP	     17)
(defconstant DB_NEXT_NODUP	     18)
(defconstant DB_PREV		     24)
(defconstant DB_PREV_NODUP	     26)
(defconstant DB_SET		     27)
(defconstant DB_SET_RANGE	     28)

(defconstant DB_NODUPDATA	     19)
(defconstant DB_NOOVERWRITE	     20)
(defconstant DB_NOSYNC		     21)

(defconstant DB_POSITION	     23)

(defconstant DB_SEQ_DEC	     #x00000001)
(defconstant DB_SEQ_INC	     #x00000002)
(defconstant DB_SEQ_WRAP     #x00000008)

(defconstant DB_SET_LOCK_TIMEOUT     #x00000001)
(defconstant DB_SET_TXN_TIMEOUT      #x00000002)

(defconstant DB_FREELIST_ONLY  #x00000001)
(defconstant DB_FREE_SPACE     #x00000002)

(defconstant DB_KEYEMPTY         -30996)
(defconstant DB_KEYEXIST	 -30995)
(defconstant DB_LOCK_DEADLOCK    -30994)
(defconstant DB_LOCK_NOTGRANTED  -30993)
(defconstant DB_NOTFOUND         -30988)

(defconstant DB_LOCK_DEFAULT	     1)
(defconstant DB_LOCK_EXPIRE	     2)
(defconstant DB_LOCK_MAXLOCKS        3)
(defconstant DB_LOCK_MAXWRITE        4)
(defconstant DB_LOCK_MINLOCKS        5)
(defconstant DB_LOCK_MINWRITE        6)
(defconstant DB_LOCK_OLDEST	     7)
(defconstant DB_LOCK_RANDOM	     8)
(defconstant DB_LOCK_YOUNGEST        9)


(def-enum DB-LOCKOP ((:DUMP 0) :GET :GET-TIMEOUT :INHERIT
		     :PUT :PUT-ALL :PUT-OBJ :PUT-READ
		     :TIMEOUT :TRADE :UPGRADE-WRITE))

(def-enum DB-LOCKMODE ((:NG 0) :READ :WRITE :WAIT
		       :IWRITE :IREAD :IWR :DIRTY :WWRITE))

(def-struct DB-LOCK
    (off :unsigned-int)
  (ndx :unsigned-int)
  (gen :unsigned-int)
  (mode DB-LOCKMODE))

#+openmcl
(ccl:def-foreign-type DB-LOCK (:struct DB-LOCK))

(def-struct DB-LOCKREQ
    (op DB-LOCKOP)
  (mode DB-LOCKMODE)
  (timeout :unsigned-int)
  (obj (:array :char))
  (lock (* DB-LOCK)))

#+openmcl
(ccl:def-foreign-type DB-LOCKREQ (:struct DB-LOCKREQ))

(defconstant +2^32+ 4294967296)
(defconstant +2^64+ 18446744073709551616)
(defconstant +2^32-1+ (1- +2^32+))

(defmacro make-64-bit-integer (high32 low32)
  `(+ ,low32 (ash ,high32 32)))

(defmacro high32 (int64)
  `(ash ,int64 -32))

(defmacro low32 (int64)
  `(logand ,int64 +2^32-1+))

(defmacro split-64-bit-integer (int64)
  `(values (ash ,int64 -32) (logand ,int64 +2^32-1+)))

;;;; Wrapper macro -- handles errno return values
;; makes flags into keywords
;; makes keyword args, cstring wrappers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-wrapper-args (args flags keys)
    (if (or flags keys)
	(append (remove-keys (remove 'flags args) keys)
		`(&key ,@flags ,@keys))
	(remove 'flags args)))

  (defun remove-keys (args keys)
    (if keys
	(loop for key in keys
	      for kw = (if (atom key) key (first key))
	      for wrapper-args = (remove kw args) then (remove kw wrapper-args)
	      finally (return wrapper-args))
	args))

  (defun make-fun-args (args flags)
    (if flags
	(substitute (cons 'flags (symbols-to-kw-pairs flags)) 'flags args)
	(substitute 0 'flags args)))

  (defun make-out-args (count)
    (loop for i from 1 to count
	  collect (gensym)))

  (defun symbols-to-kw-pairs (symbols)
    (loop for symbol in symbols
	  append (list (intern (symbol-name symbol) "KEYWORD")
		       symbol)))

  (defun symbols-to-pairs (symbols)
    (loop for symbol in symbols
	  collect (list symbol symbol)))
  )

(defmacro with-db-errno (transaction db-form &body succ-body)
  (let ((errno (gensym)))
    `(let ((,errno ,db-form))
       (declare (type fixnum ,errno))
       (cond ((= ,errno 0) (values ,(if succ-body
                                      `(progn ,@succ-body)
                                      t)))
             ,@(if transaction
                 (list `((or (= ,errno DB_LOCK_DEADLOCK)
                             (= ,errno DB_LOCK_NOTGRANTED))
                         (throw 'transaction ,transaction))))
             (t (error 'bdb-db-error :errno ,errno))))))

(defmacro wrap-errno (names args &key (keys nil) (flags nil)
		      (cstrings nil) (outs 1) (declarations nil)
		      (documentation nil)
		      (transaction nil))
  (let ((wname (if (listp names) (first names) names))
	(fname (if (listp names) (second names)
		   (intern (concatenate 'string "%" (symbol-name names)))))
	(wrapper-args (make-wrapper-args args flags keys))
	(fun-args (make-fun-args args flags))
	(errno (gensym)))
    (if (> outs 1)
	(let ((out-args (make-out-args outs)))
	  `(defun ,wname ,wrapper-args
	    ,@(if documentation (list documentation) (values))
	    ,@(if declarations (list declarations) (values))
	    (with-cstrings ,(symbols-to-pairs cstrings)
	      (multiple-value-bind ,out-args
		  (,fname ,@fun-args)
		(let ((,errno ,(first out-args)))
		  (declare (type fixnum ,errno))
		  (cond
		    ((= ,errno 0) (values ,@(rest out-args)))
		    ,@(if transaction
			  (list `((or (= ,errno DB_LOCK_DEADLOCK)
				      (= ,errno DB_LOCK_NOTGRANTED))
				  (throw 'transaction ,transaction)))
			  (values))
		    (t (error 'bdb-db-error :errno ,errno))))))))
	`(defun ,wname ,wrapper-args
	  ,@(if documentation (list documentation) (values))
	  ,@(if declarations (list declarations) (values))
	  (with-cstrings ,(symbols-to-pairs cstrings)
	    (let ((,errno (,fname ,@fun-args)))
	      (declare (type fixnum ,errno))
	      (cond
		((= ,errno 0) nil)
		,@(if transaction
		      (list `((or (= ,errno DB_LOCK_DEADLOCK)
			       (= ,errno DB_LOCK_NOTGRANTED))
			      (throw 'transaction ,transaction)))
		      (values))
		(t (error 'bdb-db-error :errno ,errno)))))))))

(defmacro flags (&key auto-commit joinenv init-cdb init-lock init-log
		 init-mpool init-rep init-txn recover recover-fatal lockdown
		 private system-mem thread force create excl nommap
		 degree-2 read-committed dirty-read read-uncommitted
		 rdonly truncate txn-nosync txn-nowait txn-sync lock-nowait
		 dup dup-sort current first get-both get-both-range last next
		 next-dup next-nodup prev prev-nodup set set-range
		 after before keyfirst keylast freelist-only free-space
		 no-dup-data no-overwrite nosync position
		 seq-dec seq-inc seq-wrap set-lock-timeout
		 set-transaction-timeout)
  (let ((flags (gensym)))
    `(let ((,flags 0))
      (declare (type fixnum ,flags))
      ,@(when auto-commit `((when ,auto-commit (setq ,flags (logior ,flags DB_AUTO_COMMIT)))))
      ,@(when joinenv `((when ,joinenv (setq ,flags (logior ,flags DB_JOINENV)))))
      ,@(when init-cdb `((when ,init-cdb (setq ,flags (logior ,flags DB_INIT_CDB)))))
      ,@(when init-lock `((when ,init-lock (setq ,flags (logior ,flags DB_INIT_LOCK)))))
      ,@(when init-log `((when ,init-log (setq ,flags (logior ,flags DB_INIT_LOG)))))
      ,@(when init-mpool `((when ,init-mpool (setq ,flags (logior ,flags DB_INIT_MPOOL)))))
      ,@(when init-rep `((when ,init-rep (setq ,flags (logior ,flags DB_INIT_REP)))))
      ,@(when init-txn `((when ,init-txn (setq ,flags (logior ,flags DB_INIT_TXN)))))
      ,@(when recover `((when ,recover (setq ,flags (logior ,flags DB_RECOVER)))))
      ,@(when recover-fatal `((when ,recover-fatal (setq ,flags (logior ,flags DB_RECOVER_FATAL)))))
      ,@(when lockdown `((when ,lockdown (setq ,flags (logior ,flags DB_LOCKDOWN)))))
      ,@(when private `((when ,private (setq ,flags (logior ,flags DB_PRIVATE)))))
      ,@(when system-mem `((when ,system-mem (setq ,flags (logior ,flags DB_SYSTEM_MEM)))))
      ,@(when thread `((when ,thread (setq ,flags (logior ,flags DB_THREAD)))))
      ,@(when force `((when ,force (setq ,flags (logior ,flags DB_FORCE)))))
      ,@(when degree-2 `((when ,degree-2 (setq ,flags (logior ,flags DB_DEGREE_2)))))
      ,@(when read-committed `((when ,read-committed (setq ,flags (logior ,flags DB_READ_COMMITTED)))))
      ,@(when dirty-read `((when ,dirty-read (setq ,flags (logior ,flags DB_DIRTY_READ)))))
      ,@(when read-uncommitted `((when ,read-uncommitted (setq ,flags (logior ,flags DB_READ_UNCOMMITTED)))))
      ,@(when create `((when ,create (setq ,flags (logior ,flags DB_CREATE)))))
      ,@(when excl `((when ,excl (setq ,flags (logior ,flags DB_EXCL)))))
      ,@(when nommap `((when ,nommap (setq ,flags (logior ,flags DB_NOMMAP)))))
      ,@(when rdonly `((when ,rdonly (setq ,flags (logior ,flags DB_RDONLY)))))
      ,@(when truncate `((when ,truncate (setq ,flags (logior ,flags DB_TRUNCATE)))))
      ,@(when txn-nosync `((when ,txn-nosync (setq ,flags (logior ,flags DB_TXN_NOSYNC)))))
      ,@(when txn-nowait `((when ,txn-nowait (setq ,flags (logior ,flags DB_TXN_NOWAIT)))))
      ,@(when txn-sync `((when ,txn-sync (setq ,flags (logior ,flags DB_TXN_SYNC)))))
      ,@(when freelist-only `((when ,freelist-only (setq ,flags (logior ,flags DB_FREELIST_ONLY)))))
      ,@(when free-space `((when ,free-space (setq ,flags (logior ,flags DB_FREE_SPACE)))))
      ,@(when lock-nowait `((when ,lock-nowait (setq ,flags (logior ,flags DB_LOCK_NOWAIT)))))
      ,@(when dup `((when ,dup (setq ,flags (logior ,flags DB_DUP)))))
      ,@(when dup-sort `((when ,dup-sort (setq ,flags (logior ,flags DB_DUPSORT)))))
      ,@(when current `((when ,current (setq ,flags (logior ,flags DB_CURRENT)))))
      ,@(when first `((when ,first (setq ,flags (logior ,flags DB_FIRST)))))
      ,@(when get-both `((when ,get-both (setq ,flags (logior ,flags DB_GET_BOTH)))))
      ,@(when get-both-range `((when ,get-both-range (setq ,flags (logior ,flags DB_GET_BOTH_RANGE)))))
      ,@(when last `((when ,last (setq ,flags (logior ,flags DB_LAST)))))
      ,@(when next `((when ,next (setq ,flags (logior ,flags DB_NEXT)))))
      ,@(when next-dup `((when ,next-dup (setq ,flags (logior ,flags DB_NEXT_DUP)))))
      ,@(when next-nodup `((when ,next-nodup (setq ,flags (logior ,flags DB_NEXT_NODUP)))))
      ,@(when prev `((when ,prev (setq ,flags (logior ,flags DB_PREV)))))
      ,@(when prev-nodup `((when ,prev-nodup (setq ,flags (logior ,flags DB_PREV_NODUP)))))
      ,@(when set `((when ,set (setq ,flags (logior ,flags DB_SET)))))
      ,@(when set-range `((when ,set-range (setq ,flags (logior ,flags DB_SET_RANGE)))))
      ,@(when after `((when ,after (setq ,flags (logior ,flags DB_AFTER)))))
      ,@(when before `((when ,before (setq ,flags (logior ,flags DB_BEFORE)))))
      ,@(when keyfirst `((when ,keyfirst (setq ,flags (logior ,flags DB_KEYFIRST)))))
      ,@(when keylast `((when ,keylast (setq ,flags (logior ,flags DB_KEYLAST)))))
      ,@(when no-dup-data `((when ,no-dup-data (setq ,flags (logior ,flags DB_NODUPDATA)))))
      ,@(when no-overwrite `((when ,no-overwrite (setq ,flags (logior ,flags DB_NOOVERWRITE)))))
      ,@(when nosync `((when ,nosync (setq ,flags (logior ,flags DB_NOSYNC)))))
      ,@(when position `((when ,position (setq ,flags (logior ,flags DB_POSITION)))))
      ,@(when seq-dec `((when ,seq-dec (setq ,flags (logior ,flags DB_SEQ_DEC)))))
      ,@(when seq-inc `((when ,seq-inc (setq ,flags (logior ,flags DB_SEQ_INC)))))
      ,@(when seq-wrap `((when ,seq-wrap (setq ,flags (logior ,flags DB_SEQ_WRAP)))))
      ,@(when set-lock-timeout `((when ,set-lock-timeout (setq ,flags (logior ,flags DB_SET_LOCK_TIMEOUT)))))
      ,@(when set-transaction-timeout `((when ,set-transaction-timeout (setq ,flags (logior ,flags DB_SET_TXN_TIMEOUT)))))
      ,flags)))

;;;; environment fli functions
(def-function ("db_env_cr" %db-env-create)
    ((flags :unsigned-int)
     (errno :int :out))
  :returning :pointer-void)

(def-function ("db_env_close" %db-env-close)
    ((dbenvp :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_env_open" %db-env-open)
    ((dbenvp :pointer-void)
     (home :cstring)
     (flags :unsigned-int)
     (mode :int))
  :returning :int)

(def-function ("db_env_dbremove" %db-env-dbremove)
    ((env :pointer-void)
     (txn :pointer-void)
     (file :cstring)
     (database :cstring)
     (flags :unsigned-int))
  :returning :int)

(DEF-FUNCTION ("DB_ENV_dbrename" %db-env-dbrename)
    ((env :pointer-void)
     (txn :pointer-void)
     (file :cstring)
     (database :cstring)
     (newname :cstring)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_env_remove" %db-env-remove)
    ((env :pointer-void)
     (home :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-remove (env home flags) :flags (force)
	    :cstrings (home)
	    :documentation "Remove an environment.  Can't be called on an open handle.")

(def-function ("db_env_set_flags" %db-env-set-flags)
    ((env :pointer-void)
     (flags :unsigned-int)
     (onoff :int))
  :returning :int)

(wrap-errno db-env-set-flags (env flags onoff)
	    :flags (auto-commit nommap txn-nosync)
	    :documentation "Set flags on an environment.")

(def-function ("db_env_get_flags" %db-env-get-flags)
    ((env :pointer-void)
     (flags :unsigned-int :out))
  :returning :int)

(wrap-errno db-env-get-flags (env) :outs 2
	    :documentation "Get flags of an environment.")

(def-function ("db_env_txn_checkpoint" %db-env-txn-checkpoint)
    ((env :pointer-void)
     (kbyte :unsigned-int)
     (min :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-txn-checkpoint (env kbyte min flags)
	    :flags (force)
	    :documentation "Make a checkpoint.")

(def-function ("db_env_set_error_file" %db-env-set-error-file)
   ((dbenv :pointer-void)
    (file :cstring))
  :returning :void)

(def-function ("db_env_set_data_dir" %db-env-set-data-dir)
   ((dbenv :pointer-void)
    (data-dir :cstring))
  :returning :void)

;;;; Database fli functions
(eval-when (:compile-toplevel :load-toplevel)
  (def-enum DBTYPE ((:BTREE 1) :HASH :QUEUE :RECNO :UNKNOWN)))

(def-function ("db_cr" %db-create)
    ((dbenv :pointer-void)
     (flags :unsigned-int)
     (errno :int :out))
  :returning :pointer-void)

(def-function ("db_close" %db-close)
    ((db :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-close (db flags)
	    :documentation "Close a DB handle.")

(def-function ("db_open" %db-open)
    ((db :pointer-void)
     (txn :pointer-void)
     (file :cstring)
     (database :cstring)
     (type DBTYPE)
     (flags :unsigned-int)
     (mode :int))
  :returning :int)

(wrap-errno db-open (db transaction file database type flags mode)
	    :flags (auto-commit create dirty-read read-uncommitted
				excl nommap rdonly thread truncate
				)
	    :keys ((transaction (txn-default *current-transaction*))
		   (file +NULL-CHAR+)
		   (database #+lispworks nil #-lispworks +NULL-CHAR+)
		   (type DB-UNKNOWN)
		   (mode 416);#o640
                   )
	    :cstrings (file database)
	    :transaction transaction
	    :documentation "Open a DB handle.  If you want transactions, be sure to open the handle with a transaction (or auto-commit.)")

(def-function ("db_remove" %db-remove)
    ((db :pointer-void)
     (file :cstring)
     (database :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-remove (db file database flags)
	    :keys ((database +NULL-CHAR+))
	    :cstrings (file database)
	    :documentation "Remove a DB handle.")

(def-function ("db_rename" %db-rename)
    ((db :pointer-void)
     (file :cstring)
     (database :cstring)
     (newname :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-rename (db file database newname flags)
	    :keys ((database +NULL-CHAR+))
	    :cstrings (file database newname)
	    :documentation "Rename a DB handle.")

(def-function ("db_sync" %db-sync)
    ((db :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sync (db flags) :documentation "Sync a DB.")

(def-function ("db_truncate" %db-truncate)
    ((db :pointer-void)
     (txn :pointer-void)
     (count :unsigned-int :out)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-truncate (db transaction flags) :flags (auto-commit)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :outs 2
	    :transaction transaction
	    :documentation "Truncate (erase) a DB.")

(def-function ("db_set_flags" %db-set-flags)
    ((db :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-set-flags (db flags)
	    :flags (dup dup-sort)
	    :documentation
"Sets flags on a DB handle.  Currently this means only DUP
and DUP-SORT.")

(def-function ("db_get_flags" %db-get-flags)
    ((db :pointer-void)
     (flags :unsigned-int :out))
  :returning :int)

(def-function ("db_set_error_file" %db-set-error-file)
   ((db :pointer-void)
    (file :cstring))
  :returning :void)

;;;; Accessors fli functions
(def-function ("db_get_raw" %db-get-key-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (result-size :unsigned-int :out))
  :returning :int)

(def-function ("db_put_raw" %db-put-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (value array-or-pointer-char)
     (value-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_exists" %db-exists)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_del" %db-delete-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_del_kv" %db-delete-kv)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (value array-or-pointer-char)
     (value-size :unsigned-int))
  :returning :int)

;;;; Compaction for BDB 4.4 fli functions

(def-function ("db_compact" %db-compact)
    ((db :pointer-void)
     (txn :pointer-void)
     (start array-or-pointer-char)
     (start-size :unsigned-int)
     (stop array-or-pointer-char)
     (stop-size :unsigned-int)
     (flags :unsigned-int)
     (end array-or-pointer-char)
     (end-length :unsigned-int)
     (end-size :unsigned-int :out))
  :returning :int)

;;;; Cursors fli functions

(def-function ("db_cursor" %db-cursor)
    ((db :pointer-void)
     (txn :pointer-void)
     (flags :unsigned-int)
     (errnop :int :out))
  :returning :pointer-void)

(def-function ("db_cursor_close" %db-cursor-close)
    ((cursor :pointer-void))
  :returning :int)

(wrap-errno db-cursor-close (cursor) :documentation "Close a cursor.")

(def-function ("db_cursor_del" %db-cursor-delete)
    ((cursor :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_cursor_dup" %db-cursor-dup)
    ((cursor :pointer-void)
     (flags :unsigned-int)
     (errnop (* :int)))
  :returning :pointer-void)

(def-function ("db_cursor_get" %db-cursor-get)
    ((cursor :pointer-void)
     (flags :unsigned-int)
     (key array-or-pointer-char :out)
     (key-size :unsigned-int :out)
     (buffer array-or-pointer-char :out)
     (buffer-size :unsigned-int :out)
     )
  :returning :int)

(def-function ("db_cursor_get_raw" %db-cursor-get-key-buffered)
    ((cursor :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (key-length :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-size :unsigned-int)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (ret-key-size :unsigned-int :out)
     (result-size :unsigned-int :out))
  :returning :int)

(def-function ("db_cursor_pget_raw" %db-cursor-pget-key-buffered)
    ((cursor :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (key-length :unsigned-int)
     (pkey array-or-pointer-char)
     (pkey-size :unsigned-int)
     (pkey-length :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-size :unsigned-int)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (ret-key-size :unsigned-int :out)
     (ret-pkey-size :unsigned-int :out)
     (result-size :unsigned-int :out))
  :returning :int)

(def-function ("db_cursor_put_raw" %db-cursor-put-buffered)
    ((cursor :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (value array-or-pointer-char)
     (value-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

;;;; Transactions fli functions
(def-function ("db_txn_begin" %db-txn-begin)
    ((env :pointer-void)
     (parent :pointer-void)
     (flags :unsigned-int)
     (errno (* :int)))
  :returning :pointer-void)

(def-function ("db_txn_abort" %db-txn-abort)
    ((txn :pointer-void))
  :returning :int)

(wrap-errno (db-transaction-abort %db-txn-abort) (transaction)
	    :declarations (declare (type pointer-void transaction))
	    :documentation "Abort a transaction.")

(def-function ("db_txn_commit" %db-txn-commit)
    ((txn :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno (db-transaction-commit %db-txn-commit) (transaction flags)
	    :flags (txn-nosync txn-sync)
	    :declarations (declare (type pointer-void transaction)
				   (type boolean txn-nosync txn-sync))
	    :documentation "Commit a transaction.")

;;;; Locks and timeouts fli functions
(def-function ("db_txn_id" %db-transaction-id)
    ((transaction :pointer-void))
  :returning :unsigned-int)

(def-function ("db_env_lock_id" %db-env-lock-id)
    ((env :pointer-void)
     (id :unsigned-int :out))
  :returning :int)

(wrap-errno db-env-lock-id (env) :outs 2
	    :documentation "Acquire a new lock ID.")

(def-function ("db_env_lock_id_free" %db-env-lock-id-free)
    ((env :pointer-void)
     (id :unsigned-int))
  :returning :int)

(wrap-errno db-env-lock-id-free (env id)
	    :documentation "Release a lock ID.")

(def-function ("db_env_lock_get" %db-env-lock-get)
    ((env :pointer-void)
     (locker :unsigned-int)
     (flags :unsigned-int)
     (object array-or-pointer-char)
     (object-size :unsigned-int)
     (lock-mode DB-LOCKMODE)
     (lock (* DB-LOCK)))
  :returning :int)

(wrap-errno db-env-lock-get (env locker flags object object-size
				 lock-mode lock)
	    :flags (lock-nowait)
	    :documentation "Acquire a lock.")

(def-function ("db_env_lock_put" %db-env-lock-put)
    ((env :pointer-void)
     (lock (* DB-LOCK)))
  :returning :int)

(wrap-errno db-env-lock-put (env lock)
	    :documentation "Release a lock.")

(def-function ("db_env_lock_vec" %db-env-lock-vec)
    ((env :pointer-void)
     (locker :unsigned-int)
     (flags :unsigned-int)
     (list (:array DB-LOCKREQ))
     (nlist :int)
     (elistp (* (* DB-LOCKREQ))))
  :returning :int)

(def-function ("db_env_set_timeout" %db-env-set-timeout)
    ((env :pointer-void)
     (timeout :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-set-timeout (env timeout flags)
	    :flags (set-lock-timeout set-transaction-timeout)
	    :documentation
"Set a timeouts on locks and transactions.  If you set this,
be prepared to handle deadlock / lock no granted errors.")

(def-function ("db_env_get_timeout" %db-env-get-timeout)
    ((env :pointer-void)
     (timeout :unsigned-int :out)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-get-timeout (env flags) :outs 2
	    :flags (set-lock-timeout set-transaction-timeout)
	    :documentation "Gets the timout.")

(def-function ("db_env_set_cachesize" %db-env-set-cachesize)
    ((env :pointer-void)
     (gbytes :unsigned-int)
     (bytes :unsigned-int)
     (ncache :int))
  :returning :int)

(wrap-errno db-env-set-cachesize (env gbytes bytes ncache)
	    :documentation "Sets the size of the buffer pool cache
            for elephant database data.  Set large if you can!")

(def-function ("db_env_get_cachesize" %db-env-get-cachesize)
    ((env :pointer-void)
     (gbytes :unsigned-int :out)
     (bytes :unsigned-int :out)
     (ncache :int :out))
  :returning :int)

(wrap-errno db-env-get-cachesize (env) :outs 4
	    :documentation "Return the current cache size of
            the BDB environment buffer pool")

(def-function ("db_env_set_lk_detect" %db-env-set-lock-detect)
    ((env :pointer-void)
     (detect :unsigned-int))
  :returning :int)

(wrap-errno db-env-set-lock-detect (env detect)
	    :documentation
"Set whether (or not) to run the deadlock detector on every
time there is a conflict.")

(def-function ("db_env_get_lk_detect" %db-env-get-lock-detect)
    ((env :pointer-void)
     (detect :unsigned-int :out))
  :returning :int)

(wrap-errno db-env-get-lock-detect (env) :outs 2 :documentation
"Get whether the deadlock detector is run on every conflict.")

(def-function ("db_env_lock_detect" %db-env-lock-detect)
    ((env :pointer-void)
     (flags :unsigned-int)
     (atype :unsigned-int)
     (aborted :int :out))
  :returning :int)

(wrap-errno db-env-lock-detect (env flags atype) :outs 2 :documentation
"Run one iteration of the deadlock detector.")

;; Secondary indices

(def-function ("db_associate" %db-associate)
    ((primary :pointer-void)
     (txn :pointer-void)
     (secondary :pointer-void)
     (callback :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-associate (primary transaction secondary callback flags)
	    :flags (create)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :documentation
"Assocate a DB as a secondary index of another DB.  Takes a
callback function which generates secondary keys.")

;;;; Some C Hacks fli functions

(def-function ("db_fake_associate" %db-fake-associate)
    ((primary :pointer-void)
     (txn :pointer-void)
     (secondary :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-fake-associate (primary transaction secondary flags)
	    :flags (auto-commit)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :documentation
"Assocates a DB as a secondary index of another DB.  Uses a
no-op function to generate secondary indices (assuming the
user will manually maintain the secondary index.)")

(def-function ("db_set_bt_compare" %db-set-bt-compare)
    ((db :pointer-void)
     (compare-function :pointer-void))
  :returning :int)

(def-function ("db_set_lisp_compare" %db-set-lisp-compare)
    ((db :pointer-void)
     (version :int))
  :returning :int)

(wrap-errno db-set-lisp-compare (db version) :documentation
"Sets the Btree comparision function to a hand-cooked
function for Elephant to compare lisp values.")

(def-function ("db_set_dup_compare" %db-set-dup-compare)
    ((db :pointer-void)
     (dup-function :pointer-void))
  :returning :int)

(def-function ("db_set_lisp_dup_compare" %db-set-lisp-dup-compare)
    ((db :pointer-void)
     (version :int))
  :returning :int)

(wrap-errno db-set-lisp-dup-compare (db version) :documentation
"Sets the duplicate comparision function to a hand-cooked
function for Elephant to compare lisp values.")

;;;; Sequences fli functions
(def-function ("db_sequence_create2" %db-sequence-create)
    ((db :pointer-void)
     (flags :unsigned-int)
     (errno (* :int)))
  :returning :pointer-void)

(def-function ("db_sequence_open" %db-sequence-open)
    ((seq :pointer-void)
     (txn :pointer-void)
     (key :cstring)
     (key-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sequence-open (sequence transaction key key-size flags)
	    :flags (create excl thread)
	    :cstrings (key)
	    :keys ((key-size (length key))
		   (transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :documentation "Open a sequence.")

(def-function ("db_sequence_close" %db-sequence-close)
    ((seq :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno (db-sequence-close %db-sequence-close) (sequence flags)
	    :documentation "Close a sequence.")

(def-function ("db_sequence_get" %db-sequence-get)
    ((seq :pointer-void)
     (txn :pointer-void)
     (delta :int)
     (low :unsigned-int :out)
     (high :int :out)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_sequence_get_lower" %db-sequence-get-lower)
    ((seq :pointer-void)
     (txn :pointer-void)
     (delta :int)
     (low :int :out)
     (flags :unsigned-int))
  :returning :int)

(def-function ("db_sequence_initial_value" %db-sequence-initial-value)
    ((seq :pointer-void)
     (low :unsigned-int)
     (high :int))
  :returning :int)

(def-function ("db_sequence_remove" %db-sequence-remove)
    ((seq :pointer-void)
     (txn :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sequence-remove (sequence transaction flags)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :flags (txn-nosync)
	    :documentation "Remove a sequence.")

(def-function ("db_sequence_set_cachesize" %db-sequence-set-cachesize)
    ((seq :pointer-void)
     (size :int))
  :returning :int)

(wrap-errno db-sequence-set-cachesize (sequence size)
	    :documentation "Set cache size for a sequence.")

(def-function ("db_sequence_get_cachesize" %db-sequence-get-cachesize)
    ((seq :pointer-void)
     (size :int :out))
  :returning :int)

(wrap-errno db-sequence-get-cachesize (sequence)
	    :outs 2
	    :documentation "Get cache size for a sequence.")

(def-function ("db_sequence_set_flags" %db-sequence-set-flags)
    ((seq :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sequence-set-flags (sequence flags)
	    :flags (seq-dec seq-inc seq-wrap)
	    :documentation "Set cache size for a sequence.")

(def-function ("db_sequence_set_range" %db-sequence-set-range)
    ((seq :pointer-void)
     (minlow :unsigned-int)
     (minhigh :int)
     (maxlow :unsigned-int)
     (maxhigh :int))
  :returning :int)

(def-function ("db_sequence_get_range" %db-sequence-get-range)
    ((seq :pointer-void)
     (minlow :unsigned-int :out)
     (minhigh :int :out)
     (maxlow :unsigned-int :out)
     (maxhigh :int :out))
  :returning :int)

(def-function ("next_counter" %next-counter)
    ((env :pointer-void)
     (db :pointer-void)
     (parent :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (lockid array-or-pointer-char)
     (lockid-size :unsigned-int))
  :returning :int)

