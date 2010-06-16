;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Grovel FFI types
;;;

(in-package :static-vectors)

(ctype size-t "size_t")

#+sbcl
(progn
  (include "unistd.h")
  (constant (+sc-pagesize+ "_SC_PAGESIZE")))
