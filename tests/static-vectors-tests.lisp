;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FiveAM Tests
;;;

(defpackage :static-vectors/test
  (:use #:cl #:static-vectors #:fiveam)
  (:import-from #:static-vectors #:cmfuncall))

(in-package :static-vectors/test)

(def-suite :static-vectors)
(def-suite :static-vectors.make-static-vectors :in :static-vectors)

(in-suite :static-vectors.make-static-vectors)

(test (make-static-vector.plain.notinline
       :compile-at :definition-time)
  (locally
      (declare (notinline make-static-vector))
    (let ((v (make-static-vector 5)))
      (is (equal 5 (length v))))))

(test (make-static-vector.plain.compiler-macro.noerror
       :compile-at :definition-time)
  (finishes
    (compile nil '(lambda ()
                   (declare (optimize (speed 3) (debug 1)))
                   (make-static-vector 5)))))

(test (make-static-vector.plain.inline
       :depends-on make-static-vector.plain.compiler-macro.noerror)
      (let ((v (cmfuncall make-static-vector 5)))
        (is (equal 5 (length v)))))

(test (make-static-vector.initial-element.notinline
       :compile-at :definition-time)
  (locally
      (declare (notinline make-static-vector))
    (let ((v (make-static-vector 5 :initial-element 3)))
      (is (equal 5 (length v)))
      (is (not (find 3 v :test-not #'=))))))

(test (make-static-vector.initial-element.compiler-macro.noerror
       :compile-at :definition-time)
  (finishes
    (compile nil '(lambda ()
                   (cmfuncall make-static-vector 5 :initial-element 3)))))

(test (make-static-vector.initial-element.inline
       :depends-on make-static-vector.initial-element.compiler-macro.noerror)
      (let ((v (cmfuncall make-static-vector 5 :initial-element 3)))
        (is (equal 5 (length v)))
        (is (not (find 3 v :test-not #'=)))))

(test (make-static-vector.initial-contents.notinline
       :compile-at :definition-time)
  (locally
      (declare (notinline make-static-vector))
    (let ((v (make-static-vector 5 :initial-contents '(1 2 3 4 5))))
      (is (equal 5 (length v)))
      (is (not (mismatch v '(1 2 3 4 5)))))))

(test (make-static-vector.initial-contents.compiler-macro.noerror
       :compile-at :definition-time)
  (finishes
    (compile nil '(lambda ()
                   (cmfuncall make-static-vector 5 :initial-contents '(1 2 3 4 5))))))

(test (make-static-vector.initial-contents.inline
       :depends-on make-static-vector.initial-contents.compiler-macro.noerror)
  (let ((v (cmfuncall make-static-vector 5 :initial-contents '(1 2 3 4 5))))
    (is (equal 5 (length v)))
    (is (not (mismatch v '(1 2 3 4 5))))))

(test (make-static-vector.initial-element-and-contents.compiler-macro.error
       :compile-at :definition-time)
  (multiple-value-bind (function warnp failp)
      (ignore-errors
       (compile nil '(lambda ()
                      (cmfuncall make-static-vector 5 :initial-element 3
                                 :initial-contents '(1 2 3 4 5)))))
    (declare (ignore warnp))
    (is-false (and function (not failp)))))
