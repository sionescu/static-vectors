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
    (%initialize-vector vector initial-element initial-element-p
                        initial-contents initial-contents-p)))

(define-compiler-macro make-static-vector (&whole whole &environment env
                                           length &key (element-type ''(unsigned-byte 8))
                                           (initial-element nil initial-element-p)
                                           (initial-contents nil initial-contents-p))
  (check-initialization-arguments initial-element-p initial-contents-p)
  (cond
    ((constantp element-type env)
     (let ((element-type (eval element-type)))
       (let ((allocation-form
              (if (constantp length env)
                  (let ((length (eval length)))
                    (check-type length non-negative-fixnum)
                    `(%allocate-static-vector ,length ',element-type))
                  (once-only (length)
                    `(progn
                       (check-type ,length non-negative-fixnum)
                       (%allocate-static-vector ,length ',element-type))))))
         (with-gensyms (vector)
           `(let ((,vector ,allocation-form))
              (symbol-macrolet (($length$ ,length)
                                ($element-type$ ',element-type))
                (%initialize-vector ,vector ,initial-element ,initial-element-p
                                    ,initial-contents ,initial-contents-p)))))))
    (t whole)))
