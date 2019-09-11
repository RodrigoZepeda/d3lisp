;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-

;;(asdf:load-system :d3)
;;(use-package 'd3)
(cl:defpackage #:d3
  (:use #:cl)
  (:export :range
           :createplot))
