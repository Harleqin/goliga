(in-package #:cl-user)

(defpackage #:goliga
  (:use #:cl
        #:alexandria
        #:cl-ppcre
        #:split-sequence)
  (:export #:compile-losung))
