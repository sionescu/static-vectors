;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :static-vectors
  (:use #:common-lisp :alexandria :cffi)
  (:export
   ;; Constructors and destructors
   #:make-static-vector
   #:free-static-vector
   #:with-static-vector

   ;; Accessors
   #:static-vector-pointer

   ;; CFFI wrapper type
   #:static-vector

   ;; Foreign memory operations
   #:copy-foreign-memory
   #:fill-foreign-memory
   ))
