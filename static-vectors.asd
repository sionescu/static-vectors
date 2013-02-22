;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(unless (or #+asdf3 (asdf/driver:version<= "2.29" (asdf-version)))
  (error "You need ASDF >= 2.29 to load this system correctly."))

(asdf:defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.lisp-expr")
  :defsystem-depends-on (#+(or allegro cmu ecl sbcl) :cffi-grovel)
  :depends-on (:alexandria :cffi)
  :pathname "src/"
  :components ((:file "pkgdcl")
               (:file "constantp" :depends-on ("pkgdcl"))
               (:file "initialize" :depends-on ("pkgdcl" "constantp"))
               #+(or allegro cmu ecl sbcl)
               (:cffi-grovel-file "ffi-types" :depends-on ("pkgdcl"))
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

(asdf:defsystem :static-vectors/test
  :depends-on (:static-vectors :fiveam)
  :version (:read-file-form "version.lisp-expr")
  :pathname "tests/"
  :components ((:file "static-vectors-tests")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :static-vectors))))
  (asdf:load-system :static-vectors/test)
  (asdf/package:symbol-call :5am :run! :static-vectors))
