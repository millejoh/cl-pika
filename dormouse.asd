;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; coding: utf-8-unix -*- ;;;;;;;;80


(asdf:defsystem "dormouse"
    :description "GUI module for libtcod, a truecolour console library."
    :author "Paul Sexton <eeeickythump@gmail.com>"
    :components
    ((:file "dormouse"))
    :depends-on ("tcod"
                 "iterate"
                 "alexandria"
                 "cl-ppcre"))


;;;; dormouse.asd ends here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
