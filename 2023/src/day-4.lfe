(defmodule day-4
  (export (day-four 1)))

(defun day-four (input)
  (let* ((cards (cards input))
         (scores (lists:map #'score/1 cards)))
    `#(,(lists:sum (lists:map #'match-score/1 scores))
       ,(lists:sum (maps:values (process-cards cards #M()))))))

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

(defun process-cards
  ((() copies) copies)
  (((cons `#(card ,id ,win ,have) rest) copies)
   (let* ((current `#(card ,id ,win ,have))
          (copies (add-copies 1 id copies))
          (of-current (map-get copies id))
          (to-copy (lists:sublist rest (score current)))
          (copies (lists:foldl (match-lambda ((`#(card ,id ,_ ,_) copies) (add-copies of-current id copies))) copies to-copy)))
     (process-cards rest copies))))

(defun add-copies (count id copies)
  (maps:update_with id (lambda (n) (+ count n)) count copies))
