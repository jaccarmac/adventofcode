(defmodule day-6
  (export (day-six 1)))

(defun day-six (input)
  (lists:foldl #'*/2 1
               (lc ((<- `(,t ,d) input))
                 (let* ((`(,lo ,hi) (boat-zeroes t d))
                        (hi-int? (if (== (floor hi) (ceil hi)) 1 0))
                        (lo (floor lo))
                        (hi (floor hi)))
                   (- hi lo hi-int?)))))

(defun boat-zeroes (time to-beat)
  `(,(/ (+ (- time) (math:sqrt (- (* time time) (* 4 to-beat)))) -2)
    ,(/ (- (- time) (math:sqrt (- (* time time) (* 4 to-beat)))) -2)))
