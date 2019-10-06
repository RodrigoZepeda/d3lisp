;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;Each plot has a different number to make them distinguishable
(defvar *plot-number* 1)

;; Set this to the directory where plots should be generated
;;(defvar *d3-pathname-defaults* (truename "/path/to/project/"))
(defvar *d3-pathname-defaults* (truename "s:/src/d3/"))
(defun d3-pathname () "Return the base directory for D3." *d3-pathname-defaults*)

(setf (logical-pathname-translations "D3")
      `(("**;*.*.*" ,(merge-pathnames "**/*.*" (d3-pathname)))))

