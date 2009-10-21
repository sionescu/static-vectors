;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version "0.0.1"
  :licence "MIT"
  :depends-on (:alexandria :cffi)
  :components ((:file "pkgdcl")
               (:file "impl" :depends-on ("pkgdcl")
                      :pathname #+allegro   "impl-allegro"
                                #+ccl       "impl-ccl"
                                #+lispworks "impl-lispworks"
                                #+sbcl      "impl-sbcl")))
