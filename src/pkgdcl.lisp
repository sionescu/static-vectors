;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :static-vectors
  (:use #:common-lisp :alexandria :cffi)
  (:export
   ;; Static vectors
   #:make-static-vector
   #:free-static-vector
   #:with-static-vector

   ;; Accessors
   #:static-vector-address
   #:static-vector-data-pointer

   ;; Foreign memory operations
   #:copy-foreign-memory
   #:fill-foreign-memory
   ))
