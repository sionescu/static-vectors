;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#+(or allegro ecl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version "0.0.1"
  :licence "MIT"
  :depends-on (:alexandria :cffi)
  :components ((:file "pkgdcl")
               (:file "initialize" :depends-on ("pkgdcl"))
               #+(or allegro ecl sbcl)
               (cffi-grovel:grovel-file "ffi-types" :depends-on ("pkgdcl"))
               (:file "impl"
                      :depends-on ("pkgdcl" "initialize" #+(or allegro ecl sbcl) "ffi-types")
                      :pathname #+allegro   "impl-allegro"
                                #+ccl       "impl-clozure"
                                #+ecl       "impl-ecl"
                                #+lispworks "impl-lispworks"
                                #+sbcl      "impl-sbcl")
               (:file "constructor" :depends-on ("pkgdcl" "initialize" "impl"))
               (:file "cffi-type-translator" :depends-on ("pkgdcl" "impl"))))
