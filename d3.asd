;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-

(asdf:defsystem #:d3
  :description "D3 interface for Common Lisp"
  :version "0.1"
  :author "Rodrigo Zepeda-Tello"
  :maintainer "Steven Nunez <steve.nunez@symbolics.com.sg"
  :license "MIT"
  :pathname "src/"
  :serial t
  :components
  ((:file "package")
   (:file "d3-init")
   (:file "to_javascript_array")
   (:file "generatevars")
   (:file "htmlcode")
   (:file "createplot")
   (:file "linspace")
   (:file "curveexamples")))
