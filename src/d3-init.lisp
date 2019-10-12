;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

; Each plot has a different number to make them distinguishable
(defvar *plot-number* 1)

;; Plots will be written to a "plots/" subdirectory beneath this directory
;(defvar *d3-pathname-default* (truename "/path/to/project/"))
(defvar *d3-pathname-default* (truename "s:/src/d3/"))
(defun d3-pathname () "Return the base directory for D3." *d3-pathname-default*)
(setf (logical-pathname-translations "D3")
      `(("**;*.*.*" ,(merge-pathnames "**/*.*" (d3-pathname)))))


;;; Browser to use to display a plot. If not set, file will be written,
;;; but not displayed.

;;; Set *browser* to the path to the browser executable on your
;;; system. *browser-options* are passed on the command line to the
;;; browser. Typically *browser-options* are used to remove decorations, tabs,
;;; etc. Each browser will require different options. The example
;;; settings work with Chrome on MS Windows.

;(defparameter *browser* nil)		;Set to path of browser executable
;(defparameter *browser-options* nil)	;Command-line options passed to browser

;Example settings
(defparameter *browser* "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
(defparameter *browser-options* "--app=")






