;;;; poker.lisp
(in-package #:breakds.poker-upgrade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(def-widget poker-table ()
    ((state (deck-cards (array))
            (status "DEALING")
            (major-num 0)
            (major-suit -1)
            (major-callable (loop for i from 0 to 4
                               collect (create "active" false
                                               "strength" 0)))
            (selected-count 0)
            (banker false)
            (pool0 (array))
            (pool1 (array))
            (pool2 (array))
            (pool3 (array)))
     (score-card (input-card) (+ (* (@ input-card suit) 100)
                                 (if (= 1 (@ input-card num))
                                     14
                                     (@ input-card num))))
     (on-select-card (sequence)
                     (let ((tmp (local-state deck-cards)))
                       (setf (@ (aref tmp sequence) selected)
                             (not (@ (aref tmp sequence) selected)))
                       (let ((new-count 
                              (if (@ (aref tmp sequence) selected)
                                  (1+ (local-state selected-count))
                                  (1- (local-state selected-count)))))
                       (chain this 
                              (set-state 
                               (create selected-count new-count)))
                       (chain this (set-state (create deck-cards tmp))))))
     (on-play-card ()
                   (chain this (set-state (create pool0 (array)
                                                  pool1 (array)
                                                  pool2 (array)
                                                  pool3 (array))))
                   (with-rpc (play-cards 0 (chain (local-state deck-cards)
                                                  (filter (lambda (x)
                                                            (@ x selected)))))
                     (let ((old-deck (local-state deck-cards)))
                       (chain this (set-state (create status "WAITING"
                                                      deck-cards
                                                      (chain old-deck 
                                                             (filter (lambda (x)
                                                                       (not (@ x selected)))))
                                                      pool0
                                                      (chain old-deck 
                                                             (filter (lambda (x)
                                                                       (@ x selected))))))))
                     nil)
                   nil)
     (pool-update () 
                   (if (= (local-state status) "WAITING")
                       (with-rpc (update-pool 0)
                         (chain this (set-state (create pool0 (aref (@ rpc-result pool) 0)
                                                        pool1 (aref (@ rpc-result pool) 1)
                                                        pool2 (aref (@ rpc-result pool) 2)
                                                        pool3 (aref (@ rpc-result pool) 3)
                                                        status (@ rpc-result status))))))
                   (if (= (local-state status) "DEALING")
                       (with-rpc (update-pool 0)
                         (chain this (set-state (create pool0 (aref (@ rpc-result pool) 0)
                                                        pool1 (aref (@ rpc-result pool) 1)
                                                        pool2 (aref (@ rpc-result pool) 2)
                                                        pool3 (aref (@ rpc-result pool) 3))))))
                   nil)
     (call-major (suit)
                 (with-rpc (call-major 0 suit 
                                       (@ (aref (local-state major-callable) suit) 
                                          strength))
                   nil))
     (update-callable ()
                      (let ((major-counter (array 0 0 0 0 0 0))
                            (new-callable (local-state major-callable)))
                        (loop for card in (local-state deck-cards)
                           when (or (= (@ card num) (local-state major-num))
                                    (> (@ card suit) 3))
                           do (incf (aref major-counter (@ card suit))))
                        (loop for i from 0 to 3
                           do (setf (aref new-callable i)
                                    (create "active" 
                                            (and (> (aref major-counter i) 
                                                    (@ this major-req))
                                                 (not (= (local-state major-suit) i)))
                                            "strength" (aref major-counter i))))
                        (setf (aref new-callable 4)
                              (create "active"
                                      (and (or (= (aref major-counter 5) 2)
                                               (= (aref major-counter 4) 2))
                                           (not (= (local-state major-suit) 4)))
                                      "strength"
                                      2))
                        (chain this (set-state (create major-callable
                                                       new-callable)))
                        ((@ this pool-update)))
                      nil)
     (normalize () 
                (with-rpc (update-unfaced (chain (local-state deck-cards)
                                                 (filter (lambda (x)
                                                           (@ x selected)))))
                  (let ((old-deck (local-state deck-cards)))
                    (setf (@ this pool-update-timer)
                          (set-interval (@ this pool-update) 500))
                    (chain this (set-state (create status "PLAYING"
                                                   deck-cards
                                                   (chain old-deck 
                                                          (filter (lambda (x)
                                                                    (not (@ x selected)))))))))
                  nil)
                nil)
     (deal-card ()
                (with-rpc (query-major)
                  (chain this (set-state (create major-suit (@ rpc-result suit))))
                  (setf (@ this major-req) (@ rpc-result req))
                  (if (= (@ this undealt-cards length) 0)
                      (progn (clear-interval (@ this dealing-timer))
                             (if (not (local-state banker))
                                 (progn
                                   (chain this (set-state (create status "PLAYING")))
                                   (setf (@ this pool-update-timer)
                                         (set-interval (@ this pool-update) 500)))
                                 (progn
                                   (with-rpc (deal-unfaced)
                                     (let ((dealt-cards (local-state deck-cards)))
                                       (setf dealt-cards (chain dealt-cards (concat rpc-result)))
                                       (chain this (set-state (create 
                                                               status "NORMALIZING"
                                                               selected-count 0
                                                               deck-cards ((@ this sorted) 
                                                                           dealt-cards))))))))
                             nil)
                      (let ((dealt-cards (local-state deck-cards)))
                        (chain dealt-cards (push (chain this 
                                                        undealt-cards 
                                                        (pop))))
                        (chain this (set-state 
                                     (create deck-cards 
                                             ((@ this sorted)
                                              dealt-cards))))
                        ((@ this update-callable))))
                  nil)
                nil)
     (component-did-mount ()
                          (with-rpc (initialize)
                            (setf (@ this undealt-cards) (@ rpc-result cards))
                            (chain this (set-state (create banker
                                                           (@ rpc-result banker))))
                            nil)
                          (with-rpc (query-major)
                            (chain this (set-state (create major-num 
                                                           (@ rpc-result "num"))))
                            (setf (@ this major-req) (@ rpc-result "req"))
                            (setf (@ this dealing-timer) 
                                  (set-interval (@ this deal-card) 400))
                            nil)
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
            (:title-bar ((major-num (local-state major-num))
                         (major-suit (local-state major-suit))))
            (:table-pool ((status (local-state status))
                          (major-callable (local-state major-callable))
                          (call-major (@ this call-major))
                          (pool0 (local-state pool0))
                          (pool1 (local-state pool1))
                          (pool2 (local-state pool2))
                          (pool3 (local-state pool3))
                          (selected-count (local-state selected-count))
                          (normalize (@ this normalize))
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


(def-widget title-bar (major-suit major-num)
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
                  (if (> major-suit -1)
                    (let ((names (array "diamonds" "clubs"
                                        "hearts" "spades" "shields")))
                      (:img ((style :width 24 :height 26)
                             (src (+ "img/suit_svg/" (aref names major-suit) ".svg")))))
                    (:div))
                  (:span ((style :color "#000055"
                                 :text-shadow "0 3px 3px #888"
                                 :font-size 30))
                         major-num))))

(def-widget table-pool (status pool0 pool1 pool2 pool3 
                               on-play-card major-callable 
                               call-major normalize selected-count) 
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
                        (funcall 
                         (lambda (x)
                           (case x
                             ("DEALING" 
                              (:major-caller ((callable major-callable)
                                              (call-major-callback call-major))))
                             ("PLAYING" 
                              (:center-button ((click-action on-play-card)
                                               (disabled false))))
                             ("WAITING" 
                              (:center-button ((click-action on-play-card)
                                               (disabled true))))
                             ("NORMALIZING" 
                              (:normalize-button ((do-normalize normalize)
                                                  (disabled (if (= selected-count 8)
                                                                false
                                                                true)))))))
                         status))
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
                (on-click (if disabled 
                              (lambda () nil)
                              click-action)))
               (if disabled 
                   "Waiting ..."
                   "Play Cards")))

(def-widget normalize-button (do-normalize disabled)
    ()
  #jsx(:button ((style :width "80%"
                       :font-size 40
                       :color (if disabled "gray" "blue")
                       :height 150)
                (on-click (if disabled 
                              (lambda () nil)
                              do-normalize)))
               "Discard"))

(def-widget major-caller (callable call-major-callback)
    ()
  #jsx(:div ()
            (let ((names (array "diamonds" "clubs"
                                "hearts" "spades" "shields")))
              (chain names 
                     (map (lambda (name id)
                            (:button ((class-name "topcoat-icon-button")
                                      (on-click (if (@ (aref callable id) active)
                                                    (lambda ()
                                                      (call-major-callback id))
                                                    (lambda () nil))))
                                     (:img ((style :width 24 :height 26)
                                            (src (+ "img/suit_svg/" name
                                                    (if (@ (aref callable id) active) 
                                                        "" "_grey")
                                                    ".svg")))))))))))

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
                    :top (if (and selected in-hand) "-20" "0")
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

