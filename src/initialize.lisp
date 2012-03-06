;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions and checking
;;;

(in-package :static-vectors)

(declaim (inline check-initialization-arguments))
(defun check-initialization-arguments (initial-element-p initial-contents-p)
  (when (and initial-element-p initial-contents-p)
    ;; FIXME: signal ARGUMENT-LIST-ERROR
    (error "MAKE-STATIC-VECTOR: You must not specify both ~
:INITIAL-ELEMENT and :INITIAL-CONTENTS")))

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

(defun check-arguments (length element-type
                        initial-element initial-element-p
                        initial-contents initial-contents-p)
  (check-initialization-arguments initial-element-p initial-contents-p)
  (check-type length non-negative-fixnum)
  (when initial-element-p
    (check-initial-element element-type initial-element))
  (when initial-contents-p
    (check-initial-contents length initial-contents)))

(defconstant +static-vectors-are-garbage-collected+
  #+(or ecl lispworks) t
  #-(or ecl lispworks) nil)

(defmacro free-vector-if-error ((vector) &body body)
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
(defun %initialize-vector (vector initial-element initial-element-p
                           initial-contents initial-contents-p)
  (cond
    (initial-element-p
     (free-vector-if-error (vector)
       (fill vector initial-element)))
    (initial-contents-p
     (free-vector-if-error (vector)
       (replace vector initial-contents))))
  vector)

(define-compiler-macro %initialize-vector
    (&whole form &environment env
     vector initial-element initial-element-p
     initial-contents initial-contents-p)
  (let ((length (macroexpand '$length$ env))
        (element-type (macroexpand '$element-type$ env)))
    (cond
      (initial-element-p
       (once-only (vector)
         `(free-vector-if-error (,vector)
            ,@(if (and (constantp element-type env)
                       (constantp initial-element env))
                  (check-initial-element element-type initial-element)
                  `((check-initial-element ,element-type ,initial-element)))
            (fill ,vector ,initial-element)
            ,vector)))
      (initial-contents-p
       (once-only (vector)
         `(free-vector-if-error (,vector)
            ,@(if (and (constantp length env)
                       (constantp initial-contents env))
                  (check-initial-contents length initial-contents)
                  `((check-initial-contents ,length ,initial-contents)))
            (replace ,vector ,initial-contents)
            ,vector)))
      (t form))))
