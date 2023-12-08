(defmodule day-7
  (export (day-seven 1)))

(defun day-seven (input)
  (let* ((hands (lc ((<- line (string:lexemes input "\n")))
                  (let ((`(,hand ,bid) (string:lexemes line " ")))
                    `#(,(binary-to-hand hand) ,(binary_to_integer bid)))))
         (ranked-hands-1 (lists:enumerate (lists:sort
                                           (match-lambda ((`#(,h1 ,_) `#(,h2 ,_)) (hand-lte #'card-rank/1 h1 h2)))
                                           hands)))
         (ranked-hands-2 (lists:enumerate (lists:sort
                                           (match-lambda ((`#(,h1 ,_) `#(,h2 ,_)) (hand-lte #'joker-rank/1 h1 h2)))
                                           (lc ((<- `#(,hand ,bid) hands)) `#(,(with-jokers hand) ,bid))))))
    `#(,(lists:sum (lc ((<- `#(,rank #(,_ ,bid)) ranked-hands-1))
                     (* rank bid)))
       ,(lists:sum (lc ((<- `#(,rank #(,_ ,bid)) ranked-hands-2))
                     (* rank bid))))))

(defun binary-to-hand (hand)
  (let ((hand (binary_to_list hand)))
    `#(,(distribution-to-type
         (hand-to-distribution hand))
       ,hand)))

(defun hand-to-distribution (hand)
  (lists:sort (maps:values (lists:foldl
                            (lambda (c m)
                              (maps:update_with c (lambda (c) (+ 1 c)) 1 m))
                            #M()
                            hand))))

(defun distribution-to-type
  (((1 1 1 1 1)) 'high-card)
  (((1 1 1 2)) 'pair)
  (((1 2 2)) 'two-pair)
  (((1 1 3)) 'three-of-a-kind)
  (((2 3)) 'full-house)
  (((1 4)) 'four-of-a-kind)
  (((5)) 'five-of-a-kind))

(defun hand-lte
  ((card-rank `#(,t1 ,h1) `#(,t2 ,h2))
   (if (=/= t1 t2)
     (=< (type-rank t1) (type-rank t2))
     (=< (lists:map card-rank h1)
         (lists:map card-rank h2)))))

(defun type-rank
  (('high-card) 1)
  (('pair) 2)
  (('two-pair) 3)
  (('three-of-a-kind) 4)
  (('full-house) 5)
  (('four-of-a-kind) 6)
  (('five-of-a-kind) 7))

(defun card-rank
  ((#\2) 2)
  ((#\3) 3)
  ((#\4) 4)
  ((#\5) 5)
  ((#\6) 6)
  ((#\7) 7)
  ((#\8) 8)
  ((#\9) 9)
  ((#\T) 10)
  ((#\J) 11)
  ((#\Q) 12)
  ((#\K) 13)
  ((#\A) 14))

(defun joker-rank
  ((#\2) 2)
  ((#\3) 3)
  ((#\4) 4)
  ((#\5) 5)
  ((#\6) 6)
  ((#\7) 7)
  ((#\8) 8)
  ((#\9) 9)
  ((#\T) 10)
  ((#\J) 1)
  ((#\Q) 12)
  ((#\K) 13)
  ((#\A) 14))

(defun with-jokers
  ((`#(,type ,hand))
   (let* ((jokers (length (lc ((<- c (when (== #\J c)) hand)) c)))
          (jokerless (lists:delete jokers (hand-to-distribution hand))))
     `#(,(distribution-to-type (case jokerless
                                 (() '(5))
                                 (distribution (++ (lists:droplast distribution)  `(,(+ jokers (lists:last distribution)))))))
        ,hand))))
