;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#+(or allegro cmu ecl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version "1.0"
  :licence "MIT"
  :depends-on (:alexandria :cffi)
  :components ((:file "pkgdcl")
               (:file "constantp" :depends-on ("pkgdcl"))
               (:file "initialize" :depends-on ("pkgdcl" "constantp"))
               #+(or allegro cmu ecl sbcl)
               (cffi-grovel:grovel-file "ffi-types" :depends-on ("pkgdcl"))
               (:file "impl"
                      :depends-on ("pkgdcl" "constantp" "initialize"
                                   #+(or allegro cmu ecl sbcl) "ffi-types")
                      :pathname #+allegro   "impl-allegro"
                                #+ccl       "impl-clozure"
                                #+cmu       "impl-cmucl"
                                #+ecl       "impl-ecl"
                                #+lispworks "impl-lispworks"
                                #+sbcl      "impl-sbcl")
               (:file "constructor" :depends-on ("pkgdcl" "constantp" "initialize" "impl"))
               (:file "cffi-type-translator" :depends-on ("pkgdcl" "impl"))))
