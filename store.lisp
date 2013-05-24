;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: store.lisp
;; Description: store and restore lisp objections
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 14:44:06(+0800)
;; Last-Updated: 2013.05.24 09:49:14(+0800)
;;     Update #: 69
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
(in-package :bdb)

;;;; object store

(defun register-store (category setter getter)
  (setf (get 'bdb-store-category category) (cons setter getter))
  category)
(defun unregister-store (category)
  (setf (get 'bdb-store-category category) nil)
  category)
(defun get-store-category (category)
  (get 'bdb-store-category category))

(defun dopen (db-file)
  (let ((db (db-create)))
    (db-open db :file db-file :type DB-BTREE :create t :mode 0)
    db))
(defun dclose (db)
  (db-close db))

(defun dput (db key value &key category)
  (let ((key-string (with-output-to-string (stream)
                      (write key :stream stream)))
        (value-sting (with-output-to-string (stream)
                       (write (if category
                                (let ((category-info (get-store-category category)))
                                  (if category-info
                                    (funcall (car category-info) value)
                                    value))
                                value)
                              :stream stream))))
    (db-put db key-string value-sting)))

(defun dget (db key &key category)
  (let* ((key-string (with-output-to-string (stream)
                       (write key :stream stream)))
         (value-string (db-get db key-string)))
    (when value-string
      (let ((value (ignore-errors (read-from-string value-string nil nil))))
        (if category
          (let ((category-info (get-store-category category)))
            (if category-info
              (funcall (cdr category-info) value)
              value))
          value)))))

(defun dexists (db key)
  (let ((key-string (with-output-to-string (stream)
                      (write key :stream stream))))
    (db-exists db key-string)))

(defun ddel (db key)
  (let ((key-string (with-output-to-string (stream)
                      (write key :stream stream))))
    (db-delete db key-string)))
