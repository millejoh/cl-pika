;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; coding: utf-8-unix -*- ;;;;;;;;80

(defpackage #:dormouse-system
	(:use #:cl #:asdf))

(in-package #:dormouse-system)

(defsystem dormouse
    :description "GUI module for libtcod, a truecolour console library."
    :author "Paul Sexton <eeeickythump@gmail.com>"
    :components
    ((:file "dormouse"))
    :depends-on ("tcod" "iterate" "alexandria" "cl-ppcre"))
