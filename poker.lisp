;;;; poker.lisp

(defpackage #:breakds.poker-upgrade
  (:nicknames #:poker-upgrade)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.poker-upgrade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(def-widget poker-deck (cards)
    ((state (deck-cards ((@ this sorted) cards)))
     (score-card (input-card) (+ (* (@ input-card suit) 100)
                                 (@ input-card num)))
     (on-select-card (sequence)
                     (let ((tmp (local-state deck-cards)))
                       (setf (@ (aref tmp sequence) selected)
                             (not (@ (aref tmp sequence) selected)))
                       (chain this (set-state (create deck-cards tmp)))))
     (sorted (input-cards) (let ((tmp (chain input-cards (map (lambda (x) x)))))
                             (chain tmp (sort (lambda (x y)
                                                (- (chain this (score-card x))
                                                   (chain this (score-card y))))))
                             (chain tmp (map (lambda (card)
                                               (create :suit (@ card suit)
                                                       :num (@ card num)
                                                       :selected false)))))))
  #jsx(:div ((style :padding-top 100))
            (chain (local-state deck-cards) (map (lambda (card seq)
                                                   (:poker-card ((suit-id (@ card suit))
                                                                 (key seq)
                                                                 (number (@ card num))
                                                                 (sequence seq)
                                                                 (selected (@ card selected))
                                                                 (on-select-card
                                                                  (@ this on-select-card)))))))))

(def-widget poker-card (suit-id number sequence selected on-select-card)
    ()
  #jsx(:img ((style :z-index (+ sequence 100)
                    :position "relative"
                    :left (+ (* -10 sequence) "%")
                    :top (if selected "-20" "0")
                    :width "15%"
                    :height "auto")
             (on-click (lambda ()
                         (on-select-card sequence)))
             (src (+ "img/cards_svg/" 
                     (funcall (lambda (x)
                                (case x 
                                  (0 "")
                                  (11 "jack")
                                  (12 "queen")
                                  (13 "king")
                                  (t x)))
                              number)
                     (funcall (lambda (x)
                                (case x
                                  (0 "_of_diamonds")
                                  (1 "_of_clubs")
                                  (2 "_of_hearts")
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
  #jsx(:poker-deck ((cards (array (create :suit 5
                                          :num 0)
                                  (create :suit 1
                                          :num 4)
                                  (create :suit 2
                                          :num 10))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

