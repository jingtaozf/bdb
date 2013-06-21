;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: memutil.lisp
;; Description: FFI interface to UFFI/memory
;; Initial version 8/26/2004 by Ben Lee<blee@common-lisp.net>
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.05.22 16:13:00(+0800)
;; Last-Updated: 2013.06.21 16:12:36(+0800)
;;     Update #: 14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :bdb)

(eval-when (:compile-toplevel) (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun process-struct-slot-defs (slot-defs struct)
    (loop for def in slot-defs
	  collect (list (first def) (list (second def) struct)))))

(defmacro with-struct-slots (slot-defs struct &body body)
  `(symbol-macrolet ,(process-struct-slot-defs slot-defs struct)
    ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; buffer-streams
;;;
;;; a stream-like interface for our buffers; methods are
;;; below.  ultimately we might want a gray / simple -stream
;;; for real, for now who cares?

(defstruct buffer-stream
  "A stream-like interface to foreign (alien) char buffers."
  (buffer (allocate-foreign-object :unsigned-char 10) :type array-or-pointer-char)
  (size 0 :type fixnum)
  (position 0 :type fixnum)
  (length 10 :type fixnum))

(defun reset-buffer-stream (bs)
  "'Empty' the buffer-stream."
  (declare (type buffer-stream bs))
  (setf (buffer-stream-size bs) 0)
  (setf (buffer-stream-position bs) 0))

(defun cleanup-buffer-stream (bs)
  "'cleanup' the buffer-stream."
  (declare (type buffer-stream bs))
  (setf (buffer-stream-size bs) 0)
  (setf (buffer-stream-position bs) 0)
  (setf (buffer-stream-length bs) 0)
  (free-foreign-object (buffer-stream-buffer bs)))

(defun resize-buffer-stream (bs length)
  "Resize the underlying buffer of a buffer-stream, copying the old data."
  (declare (type buffer-stream bs)
	   (type fixnum length))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs
    (when (> length len)
      (let ((newlen (max length (* len 2))))
	(declare (type fixnum newlen))
	(let ((newbuf (allocate-foreign-object :unsigned-char newlen)))
	  ;; technically we just need to copy from position to size.....
	  (when (null-pointer-p newbuf)
	    (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
	  (copy-bufs newbuf 0 buf 0 size)
	  (free-foreign-object buf)
	  (setf buf newbuf)
	  (setf len newlen)
	  nil)))))

(defun resize-buffer-stream-no-copy (bs length)
  "Resize the underlying buffer of a buffer-stream."
  (declare (type buffer-stream bs)
	   (type fixnum length))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (len buffer-stream-length))
    bs
    (when (> length len)
      (let ((newlen (max length (* len 2))))
	(declare (type fixnum newlen))
	(let ((newbuf (allocate-foreign-object :unsigned-char newlen)))
	  (when (null-pointer-p newbuf)
	    (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
	  (free-foreign-object buf)
	  (setf buf newbuf)
	  (setf len newlen)
	  nil)))))


(defun buffer-read-byte (bs)
  "Read a byte."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (incf (buffer-stream-position bs))
    (deref-array (buffer-stream-buffer bs) '(:array :unsigned-char) position)))

(defun buffer-write-byte (b bs)
  "Write a byte."
  (declare (type buffer-stream bs)
	   (type (unsigned-byte 8) b))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs
    (let ((needed (+ size 1)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (setf (deref-array buf '(:array :unsigned-char) size) b)
      (setf size needed))))
