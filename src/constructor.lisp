;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- MAKE-STATIC-VECTOR
;;;

(in-package :static-vectors)

(declaim (inline check-initialization-arguments))
(defun check-initialization-arguments (initial-element-p initial-contents-p)
  (when (and initial-element-p initial-contents-p)
    ;; FIXME: signal ARGUMENT-LIST-ERROR
    (error "MAKE-STATIC-VECTOR: You must not specify both ~
:INITIAL-ELEMENT and :INITIAL-CONTENTS")))

(defun check-arguments (length element-type
                        initial-element initial-element-p
                        initial-contents initial-contents-p)
  (check-initialization-arguments initial-element-p initial-contents-p)
  (check-type length non-negative-fixnum)
  (when initial-element-p
    (check-initial-element element-type initial-element))
  (when initial-contents-p
    (check-initial-contents length initial-contents)))

(declaim (inline make-static-vector))
(defun make-static-vector (length &key (element-type '(unsigned-byte 8))
                           (initial-element nil initial-element-p)
                           (initial-contents nil initial-contents-p))
  "Create a simple vector of length LENGTH and type ELEMENT-TYPE which will
not be moved by the garbage collector. The vector might be allocated in
foreign memory so you must always call FREE-STATIC-VECTOR to free it."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed)
           (notinline %allocate-static-vector %initialize-vector))
  (check-arguments length element-type initial-element initial-element-p
                   initial-contents initial-contents-p)
  (let ((vector
          (%allocate-static-vector length element-type)))
    (%initialize-vector vector length element-type
                        initial-element initial-element-p
                        initial-contents initial-contents-p)))

(define-compiler-macro make-static-vector (&whole form &environment env
                                           length &key (element-type ''(unsigned-byte 8))
                                           (initial-element nil initial-element-p)
                                           (initial-contents nil initial-contents-p))
  (check-initialization-arguments initial-element-p initial-contents-p)
  (cond
    ((constantp element-type env)
     (with-gensyms (vector)
       (once-only (length)
         `(let* ((,vector (%allocate-static-vector ,length ,element-type)))
            (cmfuncall %initialize-vector ,vector ,length ,element-type
                       ,initial-element ,initial-element-p
                       ,initial-contents ,initial-contents-p)
            ,vector))))
    (t form)))

(defmacro with-static-vectors (((var length &rest args) &rest more-clauses)
                               &body body)
  "Allocate multiple static vectors at once."
  `(with-static-vector (,var ,length ,@args)
     ,@(if more-clauses
           `((with-static-vectors ,more-clauses
               ,@body))
           body)))
