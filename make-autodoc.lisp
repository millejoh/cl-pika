(load "/home/paul/lisp/init.lisp")
(load-sys :dormouse)
(load-sys :cldoc)
(in-package :dormouse)
(cldoc:extract-documentation 'cldoc:html
       "./html" 
       (asdf:find-system :dormouse)
       :table-of-contents-title
       "Dormouse, a graphical user interface using the Doryen library"
       :section-names '("Arguments:" "Returns:" "Examples:" "See Also:"
                        "Example:" "Notes:" "Description:"))

