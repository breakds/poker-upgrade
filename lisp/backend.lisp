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
  (players (make-array 4
                       :initial-element nil))
  (seated 0 :type fixnum)
  (major-suit 0 :type fixnum)
  (major-num 0 :type fixnum)
  (major-req 0 :type fixnum)
  (turn-counter 0 :type fixnum))

(defparameter *dealer* (make-dealer))

(defstruct poker-player
  (id 0)
  (cards nil)
  (status :waiting))

(defstruct (robot-player (:include poker-player)
                         (:conc-name robot-)))

(defstruct (human-player (:include poker-player)
                         (:conc-name human-)))


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

(defun remove-cards (hand-cards cards)
  (reduce (lambda (y x) 
            (remove-if (lambda (z) (card-equal z x)) y))
          cards
          :initial-value hand-cards))

(defun query-pool-size ()
  (loop 
     for i below 4
     for cards = (aref (dealer-pool *dealer*) i)
     when cards
     return (length cards)))

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
                   collect (id-to-card (pop shuffled))))))
  
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

  ;; initialize human player
  (initialize-human-player 0)
  (setf (human-status (aref (dealer-players *dealer*) 0)) :playing)
  (mapcar #'to-json-card (human-cards (aref (dealer-players *dealer*)
                                            0))))

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
                       
  

    

  

