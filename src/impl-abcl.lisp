;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Armed Bear CL implementation
;;;

(in-package :static-vectors)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  (foreign-funcall "memset" :pointer pointer :int value size-t length :pointer)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (foreign-funcall "memcpy" :pointer dst-ptr :pointer src-ptr size-t length :pointer)
  dst-ptr)

;;; HACK for now
(defvar *static-vector-pointer*
  (make-hash-table :weakness :value))

(declaim (inline %allocate-static-vector))
(defun %allocate-static-vector (length element-type)
  (flet ((size-of (element-type)
           ;; assume 8-bit bytes
           1))
    (let* ((bytes
             (* length (size-of element-type)))
           (heap-pointer
             (jss:new "com.sun.jna.Memory" bytes))
           (bytebuffer
             (#"getByteBuffer" heap-pointer 0 bytes))
           (static-vector
             (ext:make-bytebuffer-byte-vector bytebuffer)))
      (setf (gethash static-vector *static-vector-pointer*)
            heap-pointer)
      (values
       static-vector
       heap-pointer))))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  ;;; FIXME collapse it
  (let ((expected-type 'vector))  ;; FIXME tighten
    (unless (typep vector expected-type)
      (signal 'simple-type-error vector expected-type))
    (let ((pointer (gethash vector *static-vector-pointer*)))
      (unless pointer
        (signal 'simple-error "vector ~a doesn't have an associated pointer to malloc()-ed memory" vector))
      (cffi-sys:inc-pointer pointer offset))))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (let ((pointer (gethash vector *static-vector-pointer*)))
    (when pointer
      (cffi-sys:foreign-free pointer)
      (setf (gethash vector *static-vector-pointer*) nil))))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type '(unsigned-byte 8))
                                 initial-contents initial-element)
                              &body body &environment env)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignorable element-type initial-contents initial-element))
  (multiple-value-bind (real-element-type length type)
      (canonicalize-args env element-type length)
    (remf args :element-type)
    `(let ((,var (make-static-vector ,length ,@args
                                     :element-type ',real-element-type)))
       ,.(if type `((declare (type ,type ,var))) nil)
       (unwind-protect
            (locally ,@body)
         (when ,var (free-static-vector ,var))))))
