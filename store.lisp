;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: store.lisp
;; Description: store and restore lisp objections
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 14:44:06(+0800)
;; Last-Updated: 2013.08.23 15:32:30(+0800)
;;     Update #: 81
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

(defun denv-open (data-dir &key (error-file (format nil "~a/db-err.log" data-dir)))
  (let ((env (db-env-create)))
    (db-env-set-error-file env error-file)
    (db-env-set-data-dir env data-dir)
    (db-env-open env data-dir :create t :init-log t :init-mpool t)
    env))

(defun dopen (db-file &optional (dbenv +NULL-VOID+))
  (let ((db (db-create dbenv)))
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

(defun dcursor-get (cursor)
  (multiple-value-bind (key value)
      (db-cursor-get cursor)
    (when (and key value)
      (values (ignore-errors (read-from-string key nil nil))
              (ignore-errors (read-from-string value nil nil))))))
