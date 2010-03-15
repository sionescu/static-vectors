;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- SBCL implementation.
;;;

(in-package :static-vectors)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-kernel:system-area-ub8-fill value pointer 0 length)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (sb-kernel:system-area-ub8-copy src-ptr 0 dst-ptr 0 length)
  dst-ptr)

(defconstant +array-header-size+
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(declaim (inline vector-widetag-and-n-bits))
(defun vector-widetag-and-n-bits (type)
  (let ((upgraded-type (upgraded-array-element-type type)))
    (case upgraded-type
      ((nil t) (error "~A is not a specializable array element type" type))
      (t       (sb-impl::%vector-widetag-and-n-bits type)))))

(declaim (inline static-alloc))
(defun static-alloc (size)
  (with-foreign-object (ptr :pointer)
    (let ((page-size
            (load-time-value (foreign-funcall "sysconf" :int +sc-pagesize+ :long))))
      (if (zerop (foreign-funcall "posix_memalign"
                                  :pointer ptr size-t page-size size-t size :int))
          (mem-ref ptr :pointer)
          ;; FIXME: signal proper error condition
          (error 'storage-condition)))))

(declaim (inline %allocate-static-vector))
(defun %allocate-static-vector (allocation-size widetag length initial-element)
  (let ((memblock (static-alloc allocation-size)))
    (fill-foreign-memory memblock allocation-size 0)
    (let ((length (sb-vm:fixnumize length)))
      (setf (mem-aref memblock :long 0) widetag
            (mem-aref memblock :long 1) length)
      (let ((array
             (sb-kernel:%make-lisp-obj (logior (pointer-address memblock)
                                               sb-vm:other-pointer-lowtag))))
        (when initial-element (fill array initial-element))
        array))))

(declaim (inline %allocation-size))
(defun %allocation-size (widetag length n-bits)
  (flet ((string-widetag-p (widetag)
           (or (= widetag sb-vm:simple-base-string-widetag)
               #+sb-unicode
               (= widetag sb-vm:simple-character-string-widetag))))
    (+ (* 2 sb-vm:n-word-bytes
          (ceiling
           (* (if (string-widetag-p widetag)
                    (1+ length)  ; for the final #\Null
                    length)
              n-bits)
           (* 2 sb-vm:n-word-bits)))
       +array-header-size+)))

(defun make-static-vector (length &key (element-type '(unsigned-byte 8))
                           (initial-element nil))
  "Create a simple vector of length LENGTH and type ELEMENT-TYPE which will
not be moved by the garbage collector. The vector might be allocated in
foreign memory so you must always call FREE-STATIC-VECTOR to free it."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed))
  (check-type length non-negative-fixnum)
  (multiple-value-bind (widetag n-bits)
      (vector-widetag-and-n-bits element-type)
    (let ((allocation-size
           (%allocation-size widetag length n-bits)))
      (%allocate-static-vector allocation-size widetag length initial-element))))

(define-compiler-macro make-static-vector (&whole whole &environment env
                                           length &key (element-type ''(unsigned-byte 8))
                                           (initial-element nil))
  (cond
    ((constantp element-type env)
     (let ((element-type (eval element-type)))
       (multiple-value-bind (widetag n-bits)
           (vector-widetag-and-n-bits element-type)
         (if (constantp length env)
             (let ((%length% (eval length)))
               (check-type %length% non-negative-fixnum)
               `(sb-ext:truly-the
                 (simple-array ,element-type (,%length%))
                 (%allocate-static-vector ,(%allocation-size widetag %length% n-bits)
                                          ,widetag ,%length% ,initial-element)))
             (with-gensyms (%length%)
               `(let ((,%length% ,length))
                  (check-type ,%length% non-negative-fixnum)
                  (sb-ext:truly-the
                   (simple-array ,element-type (*))
                   (%allocate-static-vector (%allocation-size ,widetag ,%length% ,n-bits)
                                            ,widetag ,%length% ,initial-element))))))))
    (t whole)))

(declaim (inline static-vector-address))
(defun static-vector-address (vector)
  "Return a foreign pointer to VECTOR(including its header).
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (logandc2 (sb-kernel:get-lisp-obj-address vector)
            sb-vm:lowtag-mask))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (make-pointer (+ (static-vector-address vector)
                   +array-header-size+
                   offset)))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (foreign-free (make-pointer (static-vector-address vector)))
  (values))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type ''(unsigned-byte 8))
                               (initial-element nil))
                              &body body)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignore element-type initial-element))
  `(sb-sys:without-interrupts
     (let ((,var (make-static-vector ,length ,@args)))
       (unwind-protect
            (sb-sys:with-local-interrupts ,@body)
         (when ,var (free-static-vector ,var))))))
