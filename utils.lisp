;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: utils.lisp
;; Description: utilities scripts.
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.08.23 13:37:18(+0800)
;; Last-Updated:
;;     Update #: 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
(in-package :bdb)

#+lispworks
(defun %close-foreign-library (name)
  "Close the foreign library NAME."
  (fli:disconnect-module name :remove t))

#-lispworks
(defun %close-foreign-library (name)
  (error "%close-foreign-library not implemented in current lisp platform."))
