;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; coding: utf-8-unix -*- ;;;;;;;;80

(defpackage #:gui-demo-system
	(:use #:cl #:asdf))

(in-package #:gui-demo-system)

(defsystem gui-demo
    :description "Demonstration of the DORMOUSE GUI library"
    :author "Paul Sexton <eeeickythump@gmail.com>"
    :components
    ((:file "gui-demo"))
    :depends-on ("dormouse"))

