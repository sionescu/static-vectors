;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3 (asdf/driver:version<= "2.32" (asdf-version)))
    (error "You need ASDF >= 2.32 to load this system correctly."))

(defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
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

(defsystem :static-vectors/test
  :depends-on (:static-vectors :fiveam)
  :version (:read-file-form "version.sexp")
  :pathname "tests/"
  :components ((:file "static-vectors-tests")))

(defmethod perform ((o test-op) (c (eql (find-system :static-vectors))))
  (load-system :static-vectors/test :force '(:static-vectors/test))
  (uiop:symbol-call :5am :run! :static-vectors))
