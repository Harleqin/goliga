(in-package #:cl-user)

(defpackage #:goliga/asdf
  (:use #:cl
        #:asdf))

(in-package #:goliga/asdf)

(defsystem #:goliga
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:split-sequence)
  :components ((:file "packages")
               (:file "base")
               (:file "constants")
               (:file "liga")
               (:file "event")
               (:file "brett")
               (:file "mannschaft")
               (:file "spieler")
               (:file "rang")
               (:file "begegnung")
               (:file "goliga")
               (:file "tabelle")
               (:file "losung")
               (:file "repl"))
  :in-order-to ((test-op (test-op #:goliga/tests))))

(defsystem #:goliga/tests
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain"
  :serial t
  :depends-on (#:goliga
               #:hu.dwim.stefil)
  :components ((:module "tests"
                        :components ((:file "packages")
                                     (:file "tests")
                                     (:file "liga-test")
                                     (:file "brett-test")
                                     (:file "mannschaft-test")
                                     (:file "rang-test")
                                     (:file "goliga-test"))))
  :perform (test-op (o c) (uiop:symbol-call 'goliga/tests 'goliga-tests)))
