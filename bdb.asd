;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: bdb.asd
;; Description: asdf definition.
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 16:13:00(+0800)
;; Last-Updated: 2013.08.23 13:54:24(+0800)
;;     Update #: 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

(defpackage bdb-system
  (:use :cl :asdf))

(in-package :bdb-system)

(defsystem bdb
  :name "bdb"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.6.0"
  :maintainer "Jingtao Xu (jingtaozf@gmail.com)"
  :licence "LLGPL"
  :description "Object database for Common Lisp"
  :long-description "Common lisp interface for Berkeley DB, for sbcl and lispworks."
  :components
  ((:file "package")
   (:file "constants")
   (:file "utils")
   (:file "fli")
   (:file "memutil")
   (:file "db")
   (:file "store"))
  :serial t
  :depends-on (:uffi :flexi-streams))

(defsystem :bdb-test
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test"))))
  :depends-on (:bdb))

(defmethod perform ((o test-op) (c (eql (find-system 'bdb))))
  (operate 'load-op 'bdb-test)
  (funcall (intern (symbol-name :run-all-tests)
                   (find-package :bdb-test))))
