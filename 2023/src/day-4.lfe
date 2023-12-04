(defmodule day-4
  (export (day-four 1))
  (import
   (from binary (split 3))
   (from lists
         (foldl 3)
         (nthtail 2)
         (splitwith 2)
         (sublist 2)
         (sum 1))
   (rename lists ((map 2) mapcar))
   (from maps
         (update_with 4)
         (values 1))
   (from sets
         (from_list 1)
         (intersection 2))
   (rename sets ((size 1) set-size))))

(defun day-four (input)
  (let* ((cards (cards input))
         (scores (mapcar #'score/1 cards)))
    `#(,(sum (mapcar #'match-score/1 scores))
       ,(sum (values (process-cards cards #M()))))))

(defun cards (cards)
  (mapcar #'binary-to-card/1
          (split cards #"\n" '(global trim))))

(defun binary-to-card (b)
  (let* ((`#(,win ,have) (splitwith (lambda (p) (/= #"|" p))
                                    (split b #" " '(global trim_all))))
         (`(,id . ,win) (nthtail 1 win))
         (have (nthtail 1 have)))
    `#(card ,id ,win ,have)))

(defun score
  ((`#(card ,_ ,win ,have))
   (let ((win (from_list win))
         (have (from_list have)))
     (set-size (intersection win have)))))

(defun match-score
  ((0) 0)
  ((1) 1)
  ((n) (* 2 (match-score (- n 1)))))

(defun process-cards
  ((() copies) copies)
  ((`(#(card ,id ,win ,have) . ,rest) copies)
   (let* ((current `#(card ,id ,win ,have))
          (copies (add-copies 1 id copies))
          (of-current (map-get copies id))
          (to-copy (sublist rest (score current)))
          (copies (foldl (match-lambda ((`#(card ,id ,_ ,_) copies) (add-copies of-current id copies))) copies to-copy)))
     (process-cards rest copies))))

(defun add-copies (count id copies)
  (update_with id (lambda (n) (+ count n)) count copies))
