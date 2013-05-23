;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; berkeley-db.lisp -- FFI interface to Berkeley DB
;;;
;;; Initial version 9/10/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;;

(in-package :cl-user)

(defpackage bdb
  (:documentation "A low-level UFFI-based interface to Berkeley
   DB to implement the elephant front-end framework.  Uses the
   libelebdb.c wrapper.  Partly intended to be usable outside
   Elephant, but with some magic for Elephant.  In general there
   is a 1-1 mapping from functions here and functions in
   Berkeley DB, so refer to their documentation for details.")
  (:use common-lisp uffi bdb-memutil)
  #+cmu
  (:use alien)
  #+sbcl
  (:use sb-alien)
  #+cmu
  (:import-from :sys #:sap+)
  #+sbcl
  (:import-from :sb-sys #:sap+)
  #+openmcl
  (:import-from :ccl #:byte-length)
  (:export
   #:load-library-if-necessary
   #:db-create
   #:db-open
   #:db-close
   #:db-put
   #:db-get
   #:db-exists
   #:db-delete
   #:DB-BTREE
   #:register-store
   #:unregister-store
   #:get-store-category
   #:dopen
   #:dclose
   #:dput
   #:dget
   #:dexists
   #:ddel
   ))
