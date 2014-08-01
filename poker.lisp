;;;; poker.lisp

(defpackage #:breakds.poker-upgrade
  (:nicknames #:poker-upgrade)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.poker-upgrade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(defun id-to-card (id)
  (let ((id-in-54 (mod id 54)))
    (case id-in-54
      (53 `(:obj ,(cons "suit" 5) ,(cons "num" 0)))
      (52 `(:obj ,(cons "suit" 4) ,(cons "num" 0)))
      (t (multiple-value-bind (suit num) (floor id-in-54 13)
           `(:obj ,(cons "suit" suit)
                  ,(cons "num" (1+ num))))))))
                


(def-rpc initialize ()
  (let ((shuffled (alexandria:shuffle 
                   (loop for i below 108 collect i))))
    (loop 
       for id in shuffled
       for i below 27
       collect (id-to-card id))))

;; (def-widget poker-table ()
;;     ((state (status :init)
;;             (cardset (array (create :num 1 :suit 1))))
;;      (component-did-mount ()
;;                           (chain console (log "haha"))
;;                           (chain this (set-state (create cardset (array (create :num 1 :suit 1)
;;                                                                         (create :num 1 :suit 2)))))
;;                           nil))
;;      ;; (component-did-mount () (with-rpc (initialize)
;;      ;;                           (chain this (set-state (create cardset rpc-result)))
;;      ;;                           (chain console (log (local-state cardset)))
;;      ;;                           nil)))
;;   #jsx(:div () (:poker-deck ((cards (local-state cardset))))))

(def-widget poker-table ()
    ((state (deck-cards (array)))
     (score-card (input-card) (+ (* (@ input-card suit) 100)
                                 (@ input-card num)))
     (on-select-card (sequence)
                     (let ((tmp (local-state deck-cards)))
                       (setf (@ (aref tmp sequence) selected)
                             (not (@ (aref tmp sequence) selected)))
                       (chain this (set-state (create deck-cards tmp)))))
     (component-did-mount ()
                          (with-rpc (initialize)
                            (chain console (log rpc-result))
                            (chain this 
                                   (set-state (create deck-cards 
                                                      ((@ this sorted)
                                                       rpc-result)))))
                          nil)
     (sorted (input-cards) (let ((tmp (chain input-cards (map (lambda (x) x)))))
                             (chain tmp (sort (lambda (x y)
                                                (- (chain this (score-card x))
                                                   (chain this (score-card y))))))
                             (chain tmp (map (lambda (card)
                                               (create :suit (@ card suit)
                                                       :num (@ card num)
                                                       :selected false)))))))
  #jsx(:div ((style :min-width 1200))
            (:div ((style :padding-top 100))
                  "Poker: Upgrade")
            (:div ((style :position "relative"))
                  (chain (local-state deck-cards) (map (lambda (card seq)
                                                         (:poker-card ((suit-id (@ card suit))
                                                                       (key seq)
                                                                       (number (@ card num))
                                                                       (sequence seq)
                                                                       (selected (@ card selected))
                                                                       (on-select-card
                                                                        (@ this on-select-card))))))))))

(def-widget poker-card (suit-id number sequence selected on-select-card)
    ()
  #jsx(:img ((style :z-index (+ sequence 100)
                    :position "absolute"
                    :left (+ (* 2 sequence) "%")
                    :top (if selected "-20" "0")
                    :width "10%"
                    :height "auto")
             (on-click (lambda ()
                         (on-select-card sequence)))
             (src (+ "img/cards_svg/" 
                     (funcall (lambda (x)
                                (case x 
                                  (0 "")
                                  (1 "ace")
                                  (11 "jack")
                                  (12 "queen")
                                  (13 "king")
                                  (t x)))
                              number)
                     (funcall (lambda (x)
                                (case x
                                  (0 "_of_diamonds")
                                  (1 "_of_hearts")
                                  (2 "_of_clubs")
                                  (3 "_of_spades")
                                  (4 "black_joker")
                                  (5 "red_joker")))
                              suit-id)
                     ".svg")))))

(def-realispic-app (poker-upgrade :title "Poker: Upgrade"
                                  :libs ("http://fb.me/react-0.10.0.js"
                                         "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js")
                                  :port 14388
                                  :document-base (merge-pathnames "assets/"
                                                                  (asdf:system-source-directory 'poker-upgrade)))
  #jsx(:poker-table))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

