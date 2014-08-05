;;;; poker.lisp

(defpackage #:breakds.poker-upgrade
  (:nicknames #:poker-upgrade)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.poker-upgrade)

(defstruct robot-player
  (cards nil))

(defparameter *players* nil)
(defparameter *pool* nil)
(defparameter *status* "playing")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(defun id-to-card (id)
  (let ((id-in-54 (mod id 54)))
    (case id-in-54
      (53 '(5 0))
      (52 '(4 0))
      (t (multiple-value-bind (suit num) (floor id-in-54 13)
           `(,suit ,(1+ num)))))))
  
(defun to-json-card (card)
  (json "suit" (first card)
        "num" (second card)))

(defun id-to-json-card (id)
  (to-json-card (id-to-card id)))

(def-rpc initialize ()
  (let ((shuffled (alexandria:shuffle 
                   (loop for i below 108 collect i))))
    (loop for i below 3
       do (push (make-robot-player 
                 :cards (loop for j from (* i 27) below (* (1+ i) 27)
                           collect (id-to-card j)))
                *players*))
    (setf *pool* '(nil nil nil nil))
    (loop 
       for id in shuffled
       for i below 27
       collect (id-to-json-card id))))

(def-rpc timed-update ()
  (json "pool" *pool*
        "status" *status*))

(def-rpc play-cards (cards)
  (setf (car *pool*) (list (id-to-json-card 12)
                            (id-to-json-card 50))))

(def-widget poker-table ()
    ((state (deck-cards (array))
            (status "waiting")
            (pool0 (array (create :num 0 :suit 4)
                          (create :num 13 :suit 2)
                          (create :num 13 :suit 2)))
            (pool1 (array (create :num 12 :suit 2)
                          (create :num 13 :suit 2)
                          (create :num 13 :suit 2)))
            (pool2 (array (create :num 12 :suit 2)
                          (create :num 13 :suit 2)
                          (create :num 13 :suit 2)))
            (pool3 (array (create :num 12 :suit 2)
                          (create :num 13 :suit 2)
                          (create :num 13 :suit 2))))
     (score-card (input-card) (+ (* (@ input-card suit) 100)
                                 (if (= 1 (@ input-card num))
                                     14
                                     (@ input-card num))))
     (on-select-card (sequence)
                     (let ((tmp (local-state deck-cards)))
                       (setf (@ (aref tmp sequence) selected)
                             (not (@ (aref tmp sequence) selected)))
                       (chain this (set-state (create deck-cards tmp)))))
     (on-play-card ()
                   (chain this (set-state (create pool0 (array)
                                                  pool1 (array)
                                                  pool2 (array)
                                                  pool3 (array))))
                   (with-rpc (play-cards 12) nil)
                   nil)
     (timed-update ()
                   (chain console (log "ok"))
                   (with-rpc (timed-update)
                     (chain this 
                            (set-state (create pool0 
                                               (aref (@ rpc-result pool) 0))))))
     (component-did-mount ()
                          (set-interval (@ this timed-update) 2000)
                          (with-rpc (initialize)
                            (chain this 
                                   (set-state (create deck-cards ((@ this sorted)
                                                                  rpc-result)
                                                      status "playing"))))
                          nil)
     (sorted (input-cards) (let ((tmp (chain input-cards (map (lambda (x) x)))))
                             (chain tmp (sort (lambda (x y)
                                                (- (chain this (score-card x))
                                                   (chain this (score-card y))))))
                             (chain tmp (map (lambda (card)
                                               (create :suit (@ card suit)
                                                       :num (@ card num)
                                                       :selected false)))))))
  #jsx(:div ()
            (:title-bar)
            (:table-pool ((status (local-state status))
                          (pool0 (local-state pool0))
                          (pool1 (local-state pool1))
                          (pool2 (local-state pool2))
                          (pool3 (local-state pool3))
                          (on-play-card (@ this on-play-card))))
            (:div ((style :position "relative"))
                  (chain (local-state deck-cards) (map (lambda (card seq)
                                                         (:poker-card ((suit-id (@ card suit))
                                                                       (key seq)
                                                                       (number (@ card num))
                                                                       (sequence seq)
                                                                       (selected (@ card selected))
                                                                       (on-select-card
                                                                        (@ this on-select-card))
                                                                       (in-hand true)))))))))


(def-widget title-bar ()
    ()
  #jsx(:div ((class-name "topcoat-navigation-bar"))
            (:div ((class-name "topcoat-navigation-bar__item two-thirds left"))
                  (:div ((class-name "topcoat-navigation-bar__title left")
                         (style :text-shadow "0 3px 3px #888"))
                        (:span ((style :color "Red"
                                       :font-size 28)) "POKER")
                        (:span ((style :color "#888888"
                                       :font-size 28))
                               "UPGRADE")))
            (:div ((class-name "topcoat-navigation-bar__item third right"))
                  (:button ((class-name "topcoat-icon-button--quiet"))
                           (:span ((class-name "icomatic")
                                   (style :color "#000055"))
                                  "settings")))))

(def-widget table-pool (status pool0 pool1 pool2 pool3 on-play-card) 
    ()
  #jsx(:div ()
            (:div ((class-name "class-g"))
                  (:div ((class-name "pure-u-1-3")))
                  (:div ((class-name "pure-u-1-3")) "Partner")
                  (:div ((class-name "pure-u-1-3"))))
            (:div ((style :height 200)
                   (class-name "class-g"))
                  (:div ((class-name "pure-u-1-3")))
                  (:div ((class-name "pure-u-1-3")
                         (style :height 200))
                        (:played-cards ((cards pool2))))
                  (:div ((class-name "pure-u-1-3"))))
            (:div ((class-name "class-g"))
                  (:div ((class-name "pure-u-1-3")) "Enemy 1")
                  (:div ((class-name "pure-u-1-3")))
                  (:div ((class-name "pure-u-1-3")) "Enemy 2"))
            (:div ((style :height 200)
                   (class-name "class-g"))
                  (:div ((class-name "pure-u-1-3")
                         (style :height 200))
                        (:played-cards ((cards pool1))))
                  (:div ((class-name "pure-u-1-3"))
                        (:center-button ((click-action on-play-card)
                                         (disabled (if (= status "playing")
                                                       false
                                                       true)))))
                  (:div ((class-name "pure-u-1-3")
                         (style :height 200))
                        (:played-cards ((cards pool3)))))
            (:div ((style :height 200)
                   (class-name "class-g"))
                  (:div ((class-name "pure-u-1-3")))
                  (:div ((class-name "pure-u-1-3")
                         (style :height 200))
                        (:played-cards ((cards pool0))))
                  (:div ((class-name "pure-u-1-3"))))))

(def-widget center-button (click-action disabled)
    ()
  #jsx(:button ((style :width "80%"
                       :font-size 40
                       :color (if disabled
                                  "gray"
                                  "black")
                       :height 150)
                (on-click click-action))
               "Play Cards"))

(def-widget played-cards (cards)
    ()
  #jsx(:div ((style :position "relative"
                    :height 200))
            (chain cards (map (lambda (card seq)
                                (:poker-card ((suit-id (@ card suit))
                                              (key seq)
                                              (number (@ card num))
                                              (sequence seq)
                                              (selected false)
                                              (on-select-card (lambda () nil))
                                              (in-hand false))))))))

(def-widget poker-card (suit-id number sequence selected on-select-card in-hand)
    ()
  #jsx(:img ((style :z-index (+ sequence 100)
                    :position "absolute"
                    :left (if in-hand (+ (+ (* 1.5 sequence) 5) "%") (* 20 sequence))
                    :top (if selected "-20" "0")
                    :width (if in-hand "8%" "auto")
                    :height (if in-hand "auto" 180)
                    :max-height (if in-hand 1000 180)
                    :height "auto")
             (on-click (lambda ()
                         (when in-hand
                           (on-select-card sequence))))
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
                                  (1 "_of_clubs")
                                  (2 "_of_hearts")
                                  (3 "_of_spades")
                                  (4 "black_joker")
                                  (5 "red_joker")))
                              suit-id)
                     (if (> number 10) "2" "")
                     ".svg")))))

(def-realispic-app (poker-upgrade :title "Poker: Upgrade"
                                  :libs ("http://fb.me/react-0.10.0.js"
                                         "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js")
                                  :css ("http://cdnjs.cloudflare.com/ajax/libs/pure/0.5.0/grids-min.css"
                                        "http://cdnjs.cloudflare.com/ajax/libs/topcoat/0.8.0/css/topcoat-mobile-light.min.css")
                                  :port 14388
                                  :document-base (merge-pathnames "assets/"
                                                                  (asdf:system-source-directory 'poker-upgrade)))
  #jsx(:poker-table))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

