;;;; poker-upgrade.asd

(asdf:defsystem #:poker-upgrade
    :serial t
    :depends-on (#:basicl
                 #:parenscript
                 #:bordeaux-threads
                 #:realispic)
    :components ((:file "lisp/backend")
                 (:file "lisp/poker")))

                        
