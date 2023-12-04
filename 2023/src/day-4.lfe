(defmodule day-4
  (export (day-four 1)))

(defun day-four (input)
  (let ((cards (cards input)))
    (lists:sum (lists:map #'match-score/1 (lists:map #'score/1 cards)))))

(defun cards (cards)
  (lists:map #'binary-to-card/1
             (binary:split cards #"\n" '(global trim))))

(defun binary-to-card (b)
  (let* ((`#(,win ,have) (lists:splitwith (lambda (p) (/= #"|" p))
                                          (binary:split b #" " '(global trim_all))))
         ((cons id win) (lists:nthtail 1 win))
         (have (lists:nthtail 1 have)))
    `#(card ,id ,win ,have)))

(defun score
  ((`#(card ,_ ,win ,have))
   (let ((win (sets:from_list win))
         (have (sets:from_list have)))
     (sets:size (sets:intersection win have)))))

(defun match-score
  ((0) 0)
  ((1) 1)
  ((n) (* 2 (match-score (- n 1)))))
