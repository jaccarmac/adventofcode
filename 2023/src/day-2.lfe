(defmodule day-2
  (export (day-two 1))
  (import (from lists
                (all 2)
                (filtermap 2)
                (foldl 3)
                (sum 1))
          (rename lists ((map 2) mapcar))
          (from maps
                (get 3)
                (merge_with 3)
                (update_with 4))))

(defun day-two (record)
  (let ((games (parse-games record)))
    `#(,(sum (filtermap #'legal-value/1 games))
       ,(sum (mapcar #'power/1 (mapcar #'minimum-cubes/1 games))))))

(defun parse-games (rest)
  (case rest
    (#""
     (! (self) 'done)
     (gather-games ()))
    (_ (parse-game rest))))

(defun parse-game
  (((binary "Game " (rest binary))) (parse-game-id rest 0)))

(defun parse-game-id
  (((binary n (rest binary)) id) (when (and (=< #\0 n) (=< n #\9)))
   (parse-game-id rest (+ (* 10 id) (- n #\0))))
  (((binary ": " (rest binary)) id)
   (parse-pull rest `#(game ,id ()))))

(defun parse-pull (rest game)
  (let ((`#(game ,id ,pulls) game))
    (parse-pull-number rest 0 `#(game ,id (#M() . ,pulls)))))

(defun parse-pull-number
  (((binary n (rest binary)) count game) (when (and (=< #\0 n) (=< n #\9)))
   (parse-pull-number rest (+ (* 10 count) (- n #\0)) game))
  (((binary " " (rest binary)) count game)
   (parse-pull-color rest #"" game count)))

(defun parse-pull-color
  (((binary ", " (rest binary)) color game count)
   (parse-pull-number rest 0 (add-to-pull game color count)))
  (((binary "; " (rest binary)) color game count)
   (parse-pull rest (add-to-pull game color count)))
  (((binary "\n" (rest binary)) color game count)
   (! (self) (add-to-pull game color count))
   (parse-games rest))
  (((binary c (rest binary)) color game count)
   (parse-pull-color rest (binary (color binary) c) game count)))

(defun add-to-pull
  ((`#(game ,id ,pulls) color count)
   (let ((`(,pull . ,pulls) pulls))
     `#(game ,id (,(update_with color (lambda (n) (+ n count)) count pull) . ,pulls)))))

(defun gather-games (games)
  (receive
    (`#(game ,id ,pulls) (gather-games `(#(game ,id ,pulls) . ,games)))
    ('done games)))

(defun legal-game?
  ((`#(game ,_ ,pulls)) (all #'legal-pull?/1 pulls)))

(defun legal-pull? (m)
  (and (>= 12 (get #"red" m 0))
       (>= 13 (get #"green" m 0))
       (>= 14(get #"blue" m 0))))

(defun legal-value
  ((`#(game ,id ,pulls))
   (if (legal-game? `#(game ,id ,pulls))
     `#(true ,id))))

(defun minimum-cubes
  ((`#(game ,_ ,pulls))
   (foldl
    (lambda (m1 m2) (merge_with
                     (lambda (k1 v1 v2) (max v1 v2))
                     m1 m2))
                #M(#"red" 0 #"green" 0 #"blue" 0) pulls)))

(defun power (m)
  (* (get #"red" m 0) (get #"green" m 0) (get #"blue" m 0)))
