;;;; poker-upgrade-test.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.poker-upgrade-test)

(defsuite* (test-all :in root-suite
                     :documentation "unit tests for backends."))


;;; ---------- Rules ----------

(deftest major-score-test ()
  (let ((dealer (make-dealer :major-suit 2
                             :major-num 4)))
    (is (equal (major-score '(1 2) dealer) 0))
    (is (equal (major-score '(2 3) dealer) 1))
    (is (equal (major-score '(0 4) dealer) 2))
    (is (equal (major-score '(2 4) dealer) 3))
    (is (equal (major-score '(4 0) dealer) 4))
    (is (equal (major-score '(5 0) dealer) 5)))
  (let ((dealer (make-dealer :major-suit 4
                             :major-num 4)))
    (is (equal (major-score '(1 2) dealer) 0))
    (is (equal (major-score '(2 3) dealer) 0))
    (is (equal (major-score '(0 4) dealer) 1))
    (is (equal (major-score '(2 4) dealer) 1))
    (is (equal (major-score '(4 0) dealer) 2))
    (is (equal (major-score '(5 0) dealer) 3))))


(deftest card-<-test ()
  ;; Test case 1
  ;; When 5 of hearts is the major.
  (let ((dealer (make-dealer :major-suit 2
                             :major-num 5)))
    (let ((cards (mapcar #'id-to-card (loop for i below 54 collect i))))
      (is (equal (sort cards #2`,(card-< x1 x2 dealer))
                 '((0 2) (0 3) (0 4) (0 6) (0 7) (0 8) (0 9) (0 10) (0 11) (0 12) (0 13) (0 1)
                   (1 2) (1 3) (1 4) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11) (1 12) (1 13) (1 1)
                   (3 2) (3 3) (3 4) (3 6) (3 7) (3 8) (3 9) (3 10) (3 11) (3 12) (3 13) (3 1)
                   (2 2) (2 3) (2 4) (2 6) (2 7) (2 8) (2 9) (2 10) (2 11) (2 12) (2 13) (2 1)
                   (0 5) (1 5) (3 5) (2 5) (4 0) (5 0))))))
  ;; Test case 2
  ;; When Joker is the major
  (let ((dealer (make-dealer :major-suit 4
                             :major-num 5)))
    (let ((cards (mapcar #'id-to-card (loop for i below 54 collect i))))
      (is (equal (sort cards #2`,(card-< x1 x2 dealer))
                 '((0 2) (0 3) (0 4) (0 6) (0 7) (0 8) (0 9) (0 10) (0 11) (0 12) (0 13) (0 1)
                   (1 2) (1 3) (1 4) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11) (1 12) (1 13) (1 1)
                   (2 2) (2 3) (2 4) (2 6) (2 7) (2 8) (2 9) (2 10) (2 11) (2 12) (2 13) (2 1)
                   (3 2) (3 3) (3 4) (3 6) (3 7) (3 8) (3 9) (3 10) (3 11) (3 12) (3 13) (3 1)
                   (0 5) (1 5) (2 5) (3 5) (4 0) (5 0))))))
  ;; Test case 3
  ;; When A of diamonds is the major
  (let ((dealer (make-dealer :major-suit 0
                             :major-num 1)))
    (let ((cards (mapcar #'id-to-card (loop for i below 54 collect i))))
      (is (equal (sort cards #2`,(card-< x1 x2 dealer))
                 '((1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11) (1 12) (1 13)
                   (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9) (2 10) (2 11) (2 12) (2 13)
                   (3 2) (3 3) (3 4) (3 5) (3 6) (3 7) (3 8) (3 9) (3 10) (3 11) (3 12) (3 13)
                   (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9) (0 10) (0 11) (0 12) (0 13)
                   (1 1) (2 1) (3 1) (0 1) (4 0) (5 0)))))))

(deftest (decomposite-test 
          :cases ((2 5 '((0 3) (0 3)) '() '((0 3)) '())
                  (2 5 '((0 3) (0 3) (0 4)) '((0 4)) '((0 3)) '())
                  (2 5 '((0 5) (0 3) (0 3) (0 4) (0 4)) 
                     '((0 5)) '() '(((0 3) (0 4))))
                  (2 5 '((3 5) (2 4) (2 6) (2 6) (2 4) (0 4) (3 5)) 
                     '((0 4)) '((3 5)) '(((2 4) (2 6))))
                  (2 5 '((3 1) (2 4) (3 1) (2 6) (3 13) (3 13) (2 4)) 
                     '((2 6)) '((2 4)) '(((3 13) (3 1))))
                  (2 5 '((2 10) (2 11) (2 10) (2 12) (2 13) (2 11) (2 13) (2 12)
                         (2 1) (2 1) (3 5) (3 5))
                     '() '() '(((2 10) (2 11) (2 12) (2 13) (2 1) (3 5))))
                  (2 5 '((2 5) (2 11) (2 5) (2 12) (2 13) (2 11) (2 13) (2 12)
                         (2 1) (2 1) (3 5) (3 5))
                     '() '() '(((2 11) (2 12) (2 13) (2 1) (3 5) (2 5))))
                  (2 5 '((2 5) (0 5) (2 5) (2 12) (2 13) (2 11) (2 13) (2 12)
                         (2 1) (2 1) (3 5) (3 5))
                     '((0 5) (2 11)) '() '(((2 12) (2 13) (2 1) (3 5) (2 5))))
                  (2 5 '((0 3) (0 3) (0 4) (0 4))
                     '() '() '(((0 3) (0 4))))))
    (major-suit major-num input expected-singles expected-pairs expected-tractors)
    (let ((dealer (make-dealer :major-suit 2
                               :major-num 5)))
      (multiple-value-bind (singles pairs tractors)
          (decomposite input dealer)
        (is (equal singles expected-singles))
        (is (equal pairs expected-pairs))
        (is (equal tractors expected-tractors)))))



                   
                                                                                                

          
