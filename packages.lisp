(in-package #:cl-user)

(defpackage #:goliga
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:split-sequence
        #:cl-who)
  (:export #:compile-losung
           #:load-data))
