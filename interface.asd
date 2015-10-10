;;;; interface.asd
;;;;
;;;; Copyright (c) 2014 Robert Smith

(asdf:defsystem #:interface
  :description "A system for defining interfaces."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:alexandria #:global-vars)
  :serial t
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "interface")))
