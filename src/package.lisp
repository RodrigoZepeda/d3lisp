;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(cl:defpackage #:d3
  (:use #:cl #:alexandria)
  (:export :range
           :plot))

(cl:defpackage #:d3-examples
  (:use #:cl #:d3)
  (:export :hipotrochoid
           :ngon
           :epitrochoid
           :hypocycloid
           :epicycloid
           :bicorn
           :involute
           :archimedes))
