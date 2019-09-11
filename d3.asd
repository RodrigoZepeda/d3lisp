;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-

;Loading asdf in SBCL http://www.sbcl.org/asdf/Using-asdf-to-load-systems.html

(asdf:defsystem #:d3
  :description "D3js interface for Common Lisp"
  :version "0.1"
  :author "Rodrigo Zepeda-Tello"
  :maintainer "Steven Nunez <steve.nunez@symbolics.com.sg>"
  :license "MIT"
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
