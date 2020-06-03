;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FiveAM Tests
;;;

(defpackage :static-vectors/test
  (:use #:cl #:static-vectors #:fiveam))

(in-package :static-vectors/test)

(in-suite* :static-vectors)

(test (make-static-vector.plain.notinline
       :compile-at :definition-time)
  (locally
      (declare (notinline make-static-vector))
    (let ((v (make-static-vector 5)))
      (is (equal 5 (length v))))))

(test (make-static-vector.initial-element.notinline
       :compile-at :definition-time)
  (locally
      (declare (notinline make-static-vector))
    (let ((v (make-static-vector 5 :initial-element 3)))
      (is (equal 5 (length v)))
      (is (not (find 3 v :test-not #'=))))))

(test (make-static-vector.initial-contents.notinline
       :compile-at :definition-time)
  (locally
      (declare (notinline make-static-vector))
    (let ((v (make-static-vector 5 :initial-contents '(1 2 3 4 5))))
      (is (equal 5 (length v)))
      (is (not (mismatch v '(1 2 3 4 5)))))))

(test (with-static-vector.defaults
       :compile-at :definition-time)
  (with-static-vector (v 5)
    (is (= 5 (length v)))
    (is (equal (array-element-type v)
               (upgraded-array-element-type
                '(unsigned-byte 8))))))

(test (with-static-vector.element-type.literal
       :compile-at :definition-time)
  (with-static-vector (v 3 :element-type '(unsigned-byte 16))
    (is (= 3 (length v)))
    (is (equal (array-element-type v)
               (upgraded-array-element-type
                '(unsigned-byte 16))))))

(deftype eltype ()
  '(unsigned-byte 32))

(test (with-static-vector.element-type.deftype
        :compile-at :definition-time)
  (with-static-vector (v 3 :element-type 'eltype)
    (is (= 3 (length v)))
    (is (equal (array-element-type v)
               (upgraded-array-element-type
                '(unsigned-byte 32))))))

(test (with-static-vector.initial-element
       :compile-at :definition-time)
  (with-static-vector (v 3 :initial-element 7)
    (is (= 3 (length v)))
    (is (equalp v #(7 7 7)))))

(test (with-static-vector.initial-contents
       :compile-at :definition-time)
  (with-static-vector (v 3 :initial-contents '(3 14 29))
    (is (= 3 (length v)))
    (is (equalp v #(3 14 29)))))

(test (with-static-vectors.initialization
       :compile-at :definition-time)
  (with-static-vectors ((v1 3 :initial-element 7)
                        (v2 5 :initial-contents '(1 2 3 4 5)))
    (is (equalp v1 #(7 7 7)))
    (is (equalp v2 #(1 2 3 4 5)))))
