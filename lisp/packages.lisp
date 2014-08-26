;;;; packages.lisp

(defpackage #:breakds.poker-upgrade
  (:nicknames #:poker-upgrade)
  (:import-from #:swiss-knife #:aif #:it)
  (:use #:cl
        #:parenscript
        #:realispic)
  (:export #:make-dealer
           #:id-to-card
           #:major-score
           #:score
           #:card-<
           #:card-pk-<
           #:decomposite))

(defpackage #:breakds.poker-upgrade-test
  (:nicknames #:poker-upgrade-test)
  (:use #:cl
        #:stefil
        #:breakds.poker-upgrade)
  (:export #:test-all))

