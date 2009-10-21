;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#+(or allegro lispworks)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version "0.0.1"
  :licence "MIT"
  :depends-on (:alexandria :cffi)
  :components ((:file "pkgdcl")
               #+(or allegro lispworks)
               (cffi-grovel:grovel-file "ffi-types" :depends-on ("pkgdcl"))
               (:file "impl"
                      :depends-on ("pkgdcl" #+(or allegro lispworks) "ffi-types")
                      :pathname #+allegro   "impl-allegro"
                                #+ccl       "impl-ccl"
                                #+lispworks "impl-lispworks"
                                #+sbcl      "impl-sbcl")))
