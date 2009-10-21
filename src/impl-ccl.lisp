;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ClozureCL implementation.
;;;

(in-package :static-vectors)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (#_memset pointer value length)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (#_memcpy dst-ptr src-ptr length)
  dst-ptr)

(declaim (inline %allocate-static-vector))
(defun %allocate-static-vector (length element-type initial-element)
  (let ((array (ccl:make-heap-ivector length element-type)))
    (when initial-element (fill array initial-element))
    array))

(defun make-static-vector (length &key (element-type '(unsigned-byte 8))
                           (initial-element nil))
  "Create a simple vector of length LENGTH and type ELEMENT-TYPE which will
not be moved by the garbage collector. The vector might be allocated in
foreign memory so you must always call FREE-STATIC-VECTOR to free it."
  (declare (optimize speed))
  (check-type length non-negative-fixnum)
  (%allocate-static-vector length element-type initial-element))

(define-compiler-macro make-static-vector (&whole whole &environment env
                                           length &key (element-type ''(unsigned-byte 8))
                                           (initial-element nil))
  (cond
    ((constantp element-type env)
     (let ((element-type (eval element-type)))
       (if (constantp length env)
           (let ((%length% (eval length)))
             (check-type %length% non-negative-fixnum)
             `(%allocate-static-vector ,%length% ',element-type ,initial-element))
           (with-gensyms (%length%)
             `(let ((,%length% ,length))
                (check-type ,%length% non-negative-fixnum)
                (%allocate-static-vector ,%length% ',element-type ,initial-element))))))
    (t whole)))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (unless (typep vector 'ccl::ivector)
    (ccl::report-bad-arg vector 'ccl::ivector))
  (let ((ptr (null-pointer)))
    (inc-pointer (ccl::%vect-data-to-macptr vector ptr) offset)))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (ccl:dispose-heap-ivector vector)
  (values))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type ''(unsigned-byte 8))
                               (initial-element nil))
                              &body body)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignore element-type initial-element))
  `(let ((,var nil))
     (unwind-protect
          (progn
            (setf ,var (make-static-vector ,length ,@args))
            ,@body)
       (when ,var (free-static-vector ,var)))))
