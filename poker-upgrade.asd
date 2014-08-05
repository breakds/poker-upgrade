;;;; poker-upgrade.asd

(asdf:defsystem #:poker-upgrade
    :serial t
    :depends-on (#:basicl
                 #:parenscript
                 #:realispic)
    :components ((:file "poker")))

                        
