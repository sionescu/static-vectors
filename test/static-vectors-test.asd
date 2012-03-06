;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :static-vectors-test
  :depends-on (:static-vectors :fiveam)
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :components ((:file "static-vectors-tests"))
  :in-order-to ((asdf:test-op (asdf:load-op :static-vectors-test)))
  :perform (asdf:test-op :after (op c)
             (funcall (intern (string '#:run!) :fiveam)
                      :static-vectors)))
