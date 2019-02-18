;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Clasp implementation
;;;

(in-package :static-vectors)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (sys:fill-foreign-memory pointer length value)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (sys:replace-foreign-memory dst-ptr src-ptr length)
  dst-ptr)

(declaim (inline %allocate-static-vector))
(defun %allocate-static-vector (length element-type)
  (sys:make-static-vector (upgraded-array-element-type element-type) length))

(declaim (inline static-vector-address))
(defun static-vector-address (vector)
  "Return a foreign pointer to VECTOR(including its header).
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (sys:static-vector-address vector))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (sys:static-vector-pointer vector offset))


(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (gctools:deallocate-unmanaged-instance vector)
  (values))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type '(unsigned-byte 8))
                                 initial-contents initial-element)
                              &body body)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignore element-type initial-contents initial-element))
  `(let ((,var (make-static-vector ,length ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (free-static-vector ,var))))