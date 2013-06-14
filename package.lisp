;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: package.lisp
;; Description: package definition.
;; Initial version 8/26/2004 by Ben Lee<blee@common-lisp.net>
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 16:13:00(+0800)
;; Last-Updated: 2013.06.14 13:30:30(+0800)
;;     Update #: 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage bdb
  (:documentation "A UFFI-based interface to Berkeley DB")
  (:use common-lisp uffi)
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
