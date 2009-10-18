;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Common utility functions.
;;;

(in-package :static-vectors)

(declaim (inline %choose-initial-element))
(defun %choose-initial-element (element-type initial-element initial-element-p)
  (cond
    (initial-element-p
     (coerce initial-element element-type))
    ((subtypep element-type 'number)
     (coerce 0 element-type))
    (t #\Null)))
