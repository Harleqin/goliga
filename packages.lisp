(in-package #:cl-user)

(defpackage #:goliga
  (:use #:cl
        #:alexandria
        #:cl-ppcre
        #:split-sequence))

(defpackage #:goliga/tests
  (:use #:cl
        #:alexandria
        #:hu.dwim.stefil))
