;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: D3 -*-
(in-package #:d3)

;Each plot has a different number to make them distinguishable
(defvar *plot-number* 1)
;(defvar *create-instance* T) ;I think this came from Hunchentoot

;; Set this to the directory where plots should be generated
;; I DIDN'T UNDERSTAND THIS PART
;;(setf *default-pathname-defaults* (truename "/path/to/project/plots/"))

;Examples of curves already made
;(load "curveexamples.lisp")
