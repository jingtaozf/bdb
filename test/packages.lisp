;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :bdb-test
  (:use :cl :bdb)
  (:export :run-all-tests))
