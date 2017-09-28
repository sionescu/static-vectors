;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- MAKE-STATIC-VECTOR
;;;

(in-package :static-vectors)

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
  (with-gensyms (len-var vector)
    (let ((len-val length))
      (cond
        ((constantp element-type env)
         (let ((allocation-form
                 (cond
                   ((constantp length env)
                    (setf len-val (eval-constant length env))
                    (check-type len-val non-negative-fixnum)
                    `(cmfuncall %allocate-static-vector ,len-val ,element-type))
                   (t
                    `(progn
                       (check-type ,len-var non-negative-fixnum)
                       (cmfuncall %allocate-static-vector ,len-var ,element-type))))))
           `(let* ((,len-var ,len-val)
                   (,vector ,allocation-form))
              (declare (ignorable ,len-var))
              (cmfuncall %initialize-vector ,vector ,len-var ,element-type
                         ,initial-element ,initial-element-p
                         ,initial-contents ,initial-contents-p))))
        (t form)))))

(defmacro with-static-vectors ((&optional ((var length &rest args)
                                           '(nil nil) supplied-p)
                                &rest more-clauses)
                               &body body)
  "Wrap BODY into multiple invocations of WITH-STATIC-VECTOR."
  (if supplied-p
      `(with-static-vector (,var ,length ,@args)
         (with-static-vectors ,more-clauses
           ,@body))
      `(progn ,@body)))
