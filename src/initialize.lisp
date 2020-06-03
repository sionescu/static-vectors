;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions and checking
;;;

(in-package :static-vectors)

(declaim (inline check-initial-element))
(defun check-initial-element (element-type initial-element)
  (when (not (typep initial-element element-type))
    ;; FIXME: signal SUBTYPE-ERROR
    (error "MAKE-STATIC-VECTOR: The type of :INITIAL-ELEMENT ~S is not a subtype ~
of the array's :ELEMENT-TYPE ~S"
           initial-element element-type)))

(declaim (inline check-initial-contents))
(defun check-initial-contents (length initial-contents)
  (let ((initial-contents-length (length initial-contents)))
    (when (/= length initial-contents-length)
      ;; FIXME: signal TYPE-ERROR
      (error "MAKE-STATIC-VECTOR: There are ~A elements in the :INITIAL-CONTENTS, ~
but requested vector length is ~A."
             initial-contents-length length))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +static-vectors-are-garbage-collected+
    #+(or cmucl ecl lispworks) t
    #-(or cmucl ecl lispworks) nil))

(defmacro free-vector-on-error ((vector) &body body)
  (if +static-vectors-are-garbage-collected+
      `(progn ,@body)
      `(unwind-protect-case ()
           (progn ,@body)
         (:abort (free-static-vector ,vector)))))

(defmacro the* (type form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl cl:the
    ,type ,form))

(declaim (inline %initialize-vector))
(defun %initialize-vector (vector length element-type initial-element initial-element-p
                           initial-contents initial-contents-p)
  ;; These two are kept because the compiler-macro uses them to check for the validity
  ;; of the INITIAL-ELEMENT and INITIAL-CONTENTS
  (declare (ignore length element-type))
  (free-vector-on-error (vector)
    (cond
      (initial-element-p
       (fill vector initial-element))
      (initial-contents-p
       (replace vector initial-contents))))
  vector)

(define-compiler-macro %initialize-vector
    (&whole form &environment env
     vector length element-type
     initial-element initial-element-p
     initial-contents initial-contents-p)
  (cond
    ((and (constantp initial-element-p env)
          (eval-constant initial-element-p env))
     (once-only (vector)
       `(free-vector-on-error (,vector)
          ,@(if (and (constantp element-type env)
                     (constantp initial-element env))
                (check-initial-element (eval-constant element-type env) initial-element)
                `((check-initial-element ,element-type ,initial-element)))
          (fill ,vector ,initial-element)
          ,vector)))
    ((and (constantp initial-contents-p env)
          (eval-constant initial-contents-p env))
     (once-only (vector)
       `(free-vector-on-error (,vector)
          ,@(if (and (constantp length env)
                     (constantp initial-contents env))
                (check-initial-contents length (eval-constant initial-contents env))
                `((check-initial-contents ,length ,initial-contents)))
          (replace ,vector ,initial-contents)
          ,vector)))
    (t form)))
