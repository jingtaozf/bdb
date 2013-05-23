;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
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
  ((:file "memutil")
   (:file "package")
   (:file "berkeley-constants")
   (:file "berkeley-db")
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
