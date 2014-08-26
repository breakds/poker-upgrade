;;;; poker-upgrade.asd

(asdf:defsystem #:poker-upgrade
    :serial t
    :depends-on (#:basicl
                 #:parenscript
                 #:bordeaux-threads
                 #:stefil
                 #:realispic)
    :components ((:file "lisp/packages")
                 (:file "lisp/backend")
                 (:file "lisp/poker")
                 (:file "lisp/poker-upgrade-test")))

                        
