;;;; package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:interface
  (:use #:cl)
  (:nicknames #:intf)

  ;; interface.lisp
  (:export
   #:define-interface                   ; MACRO
   #:define-implementation              ; MACRO
   #:calling-form                       ; FUNCTION
   )
  )

