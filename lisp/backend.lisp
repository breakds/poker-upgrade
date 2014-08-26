;;;; backend.lisp

(in-package #:breakds.poker-upgrade)

;;; ---------- Data Structures ----------

(defstruct dealer
  (cards (make-array 4 
                     :initial-element nil)
         :type (simple-array * (4)))
  (pool (make-array 4
                    :initial-element nil)
        :type (simple-array * (4)))
  (unfaced nil)
  (players (make-array 4
                       :initial-element nil))
  (seated 0 :type fixnum)
  (major-suit 0 :type fixnum)
  (major-num 0 :type fixnum)
  (major-req 0 :type fixnum)
  (banker 0)
  (turn-counter 0 :type fixnum))

(defvar *dealer* (make-dealer))

(defstruct poker-player
  (id 0)
  (cards nil)
  (status :waiting))

(defstruct (robot-player (:include poker-player)
                         (:conc-name robot-)))

(defstruct (human-player (:include poker-player)
                         (:conc-name human-))
  (ready nil))



;;; ---------- Card Utilities ----------
;;; Suit: 0 1 2 3 4 5
;;; Number: (0) 1 2 3 .. 13
;;; A card is represented as a pair of numbers, (suit number).
;;; suit 4 and 5 must have a number of 0, as they are jokers.

(declaim (inline card-suit))
(defun card-suit (card)
  (first card))

(declaim (inline card-num))
(defun card-num (card)
  (second card))

(defun suit-name (suit)
  (case suit
    (0 "diamonds")
    (1 "clubs")
    (2 "hearts")
    (3 "spades")
    (4 "black_joker")
    (5 "red_joker")))

(defun num-name (num)
  (case num
    (0 "")
    (1 "ace")
    (11 "jack")
    (12 "queen")
    (13 "king")
    (t num)))

(defun validate-card (card)
  "Return the name of the card if it is a valid one, or nil instead."
  (let ((suit (card-suit card))
        (num (card-num card)))
    (cond ((and (< suit 4) (< 0 num) (< num 14)) 
           (format nil "%a_of_%a" (suit-name suit) (num-name num)))
          ((or (= suit 4) (= suit 5)) (suit-name suit))
          (t nil))))

(defun id-to-card (id)
  "map id (0 - 107) to a card."
  (let ((id-in-54 (mod id 54)))
    (case id-in-54
      (53 '(5 0))
      (52 '(4 0))
      (t (multiple-value-bind (suit num) (floor id-in-54 13)
           `(,suit ,(1+ num)))))))

(defun to-json-card (card)
  (json "suit" (card-suit card)
        "num" (card-num card)))

(defun id-to-json-card (id)
  (to-json-card (id-to-card id)))

(declaim (inline json-to-card))
(defun json-to-card (json-card)
  (list (jsown:val json-card "suit")
        (jsown:val json-card "num")))

(declaim (inline card-equal))
(defun card-equal (card-a card-b)
  (equal card-a card-b))

(defun remove-cards (cards to-be-removed)
  (reduce (lambda (y x) 
            (remove-if (lambda (z) (card-equal z x)) y))
          to-be-removed
          :initial-value cards))

(defun query-pool-size ()
  (loop 
     for i below 4
     for cards = (aref (dealer-pool *dealer*) i)
     when cards
     return (length cards)))

;;; ---------- Rules ----------

(defstruct hand
  ;; type can be :single :pair :tractor
  (type :single)
  ;; for a hand, content must be sorted
  (content nil)
  ;; the id of the player who played the hand
  (player 0)
  ;; pos is the position of the player, it can be
  ;; 
  ;; 0: leader, the first one who played the card in this round
  ;; 1: follower, the second
  ;; 2: supporter, the third
  ;; 4: closer, the forth
  (pos 0))


(declaim (inline major-score))
(defun major-score (card dealer)
  ;; major-score is defined as below:
  ;; when major-suit < 4:
  ;;   = major-num: + 2
  ;;   = major-suit: + 1
  ;;   is black-joker: + 4
  ;;   is red-joker: + 5
  ;; when major-suit = 4:
  ;;   = major-num: + 1
  ;;   = black-joker: + 2
  ;;   = red-joker: + 3
  (let ((suit (card-suit card))
        (num (card-num card))
        (switch (= (dealer-major-suit dealer) 4)))
    (+ (if (and (= suit (dealer-major-suit dealer))
                (not switch))
           1 0)
       (if (= num (dealer-major-num dealer))
           (if switch 1 2) 0)
       (if (> suit 3)
           (- suit (if switch 2 0)) 0))))

(declaim (inline score))
(defun score (card dealer)
  (labels ((num-score (num)
             (if (= num 1)
                 14
                 (+ num
                    (if (or (< num (dealer-major-num dealer))
                            (= 1 (dealer-major-num dealer)))
                        1 0)))))
    (let ((suit (card-suit card))
          (num (card-num card))
          (m-score (major-score card dealer))
          (switch (= (dealer-major-suit dealer) 4)))
      (+ (* (if (zerop m-score) suit 4) 100)
         (if switch
             (case m-score
               (0 (num-score num))
               (t m-score))
             (if (< m-score 2)
                 (num-score num)
                 (+ 13 m-score)))))))

(defun card-< (card-a card-b dealer)
  "The comparator for cards. This function defines the order of cards,
  but does not determine the strength between two cards as it does not
  take consideration of player position."
  (let ((score-a (score card-a dealer))
        (score-b (score card-b dealer)))
    (if (= score-a score-b)
        (< (card-suit card-a) (card-suit card-b))
        (< score-a score-b))))

(defun decomposite (cards dealer)
  (let ((sorted (sort (copy-list cards) #2`,(card-< x1 x2 dealer)))
        singles pair-holder pairs tractor-stack tractors)
    (labels ((try-push-pair (card)
               (if (null pair-holder)
                   (setf pair-holder card)
                   (if (card-equal pair-holder card)
                       (progn (try-push-tractor card)
                              (setf pair-holder nil))
                       (progn (push pair-holder singles)
                              (setf pair-holder card)))))
             (try-push-tractor (card)
               (if (null tractor-stack)
                   (push card tractor-stack)
                   (if (= (abs (- (score (first tractor-stack) dealer)
                                  (score card dealer)))
                          1)
                       (push card tractor-stack)
                       (progn (if (> (length tractor-stack) 1)
                                  (push (reverse tractor-stack) tractors)
                                  (push (first tractor-stack) pairs))
                              (setf tractor-stack (list card)))))))
      (loop for card in sorted
         do (try-push-pair card))
      (when pair-holder 
        (push pair-holder singles))
      (when tractor-stack
        (if (> (length tractor-stack) 1)
            (push (reverse tractor-stack) tractors)
            (push (first tractor-stack) pairs))))
    (values singles pairs tractors)))

(defun card-pk-< (card-a pos-a card-b pos-b dealer)
  (let ((major-score-a (major-score card-a dealer))
        (major-score-b (major-score card-b dealer)))
    (or (< major-score-a major-score-b)
        (and (= major-score-a major-score-b)
             (if (= (card-suit card-a) 
                    (card-suit card-b))
                 (card-< card-a card-b dealer)
                 (< pos-b pos-a))))))
                          

;;; ---------- Interfaces ----------

(defun initialize-robot-player (id)
  (setf (aref (dealer-players *dealer*) id)
        (make-robot-player :id id
                           :status :waiting
                           :cards (copy-list (aref (dealer-cards *dealer*)
                                                   id)))))
(defun initialize-human-player (id)
  (setf (aref (dealer-players *dealer*) id)
        (make-human-player :id id
                           :status :waiting
                           :ready nil
                           :cards (copy-list (aref (dealer-cards *dealer*)
                                                   id)))))

(defun clear-pool ()
  (loop for i below 4
     do (setf (aref (dealer-pool *dealer*) i) nil)))

(defun new-turn ()
  (setf (dealer-turn-counter *dealer*) 0))
  
(def-rpc initialize ()
  ;; Deal cards
  (let ((shuffled (alexandria:shuffle 
                   (loop for i below 108 collect i))))
    (loop for i below 4
       do (setf (aref (dealer-cards *dealer*) i)
                (loop for j below 25
                   collect (id-to-card (pop shuffled)))))
    (setf (dealer-unfaced *dealer*) 
          (mapcar #'id-to-card shuffled)))
  
  ;; initialize turn-counter
  (new-turn)
                
  ;; initialize robot player
  (loop for i below 3 
     do (initialize-robot-player (- 3 i)))
  
  ;; initialize pool
  (clear-pool)

  ;; initialize major
  (setf (dealer-major-suit *dealer*) -1)
  (setf (dealer-major-num *dealer*) 2)
  (setf (dealer-major-req *dealer*) 0)

  ;; initialize banker
  (setf (dealer-banker *dealer*) 0)

  ;; initialize human player
  (initialize-human-player 0)
  (setf (human-status (aref (dealer-players *dealer*) 0)) :playing)
  (json "cards" 
        (mapcar #'to-json-card (human-cards (aref (dealer-players *dealer*)
                                                  0)))
        "banker" t))

(defun robot-think (player-id)
  (sleep 1)
  (let ((num (aif (query-pool-size) it (1+ (random 3)))))
    (handle-play-cards player-id
                       (loop for i below num
                          collect (pop (robot-cards 
                                        (aref (dealer-players *dealer*)
                                              player-id)))))))

(defun handle-play-cards (player-id cards)
  (incf (dealer-turn-counter *dealer*) 0)
  (setf (aref (dealer-cards *dealer*) 
              player-id)
        (remove-cards (aref (dealer-cards *dealer*) 
                            player-id)
                      cards))

  ;; Clear pool and turn-counter if it's a new turn
  (when (= (dealer-turn-counter *dealer*) 0)
    (clear-pool))
  
  (incf (dealer-turn-counter *dealer*))
  (setf (aref (dealer-pool *dealer*) player-id)
        cards)

  ;; Call next player.
  (let ((next-player-id (if (= (dealer-turn-counter *dealer*) 4)
                            (progn (new-turn) 0)
                            (mod (1+ player-id) 4))))
    (if (human-player-p (aref (dealer-players *dealer*)
                              next-player-id))
        (setf (human-status (aref (dealer-players *dealer*)
                                  next-player-id))
              :playing)
        (bordeaux-threads:make-thread
         (lambda () (robot-think next-player-id))))))

(def-rpc play-cards (player-id cards)
  (setf (human-status (aref (dealer-players *dealer*) 
                            player-id))
        :waiting)
  (handle-play-cards player-id 
                     (mapcar #'json-to-card cards))
  nil)
  
(def-rpc update-pool (player-id)
  (json "pool" (loop for cards across (dealer-pool *dealer*)
                  collect (mapcar #'to-json-card cards))
        "status" (human-status (aref (dealer-players *dealer*)
                                     player-id))))

(def-rpc query-major ()
  (json "suit" (dealer-major-suit *dealer*)
        "num" (dealer-major-num *dealer*)
        "req" (dealer-major-req *dealer*)))
                                  
(def-rpc call-major (player-id suit strength)
  (hunchentoot:log-message* :info "suit: ~a strength ~a" suit strength)
  (when (or (> strength (dealer-major-req *dealer*))
            (and (= suit 4) (< (dealer-major-suit *dealer*) 4)))
    (setf (dealer-major-req *dealer*) strength)
    (setf (dealer-major-suit *dealer*) suit)
    (clear-pool)
    (setf (aref (dealer-pool *dealer*) player-id)
          (loop for i below strength
             collect (if (< suit 4)
                         (list suit (dealer-major-num *dealer*))
                         (list suit 0)))))
  nil)

(def-rpc deal-unfaced ()
  (mapcar #'to-json-card
          (dealer-unfaced *dealer*)))
                       

(def-rpc update-unfaced (cards)
  (setf (dealer-unfaced *dealer*)
        (mapcar #'json-to-card cards))
  nil)
    

  

