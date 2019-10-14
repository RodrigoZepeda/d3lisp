;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(asdf:defsystem #:d3
  :description "D3js interface for Common Lisp"
  :version "0.2.0"
  :author "Rodrigo Zepeda-Tello"
  :author "Steven Nunez"
  :license "MIT"
  :depends-on (#:alexandria)
  :pathname "src/"
  :serial t
  :components
  ((:file "package")
   (:file "d3-init")
   (:file "plot")
   (:file "auxiliary-functions")
   (:file "html-wrappers")
   (:file "javascript-wrappers")
   (:file "curves")))

