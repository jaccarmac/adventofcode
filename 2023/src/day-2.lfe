(defmodule day-2
  (export (day-two 1)))

(defun day-two (record)
  (let ((games (parse-games record)))
    `#(,(lists:sum (lists:filtermap #'legal-value/1 games))
       ,(lists:sum (lists:map #'power/1 (lists:map #'minimum-cubes/1 games))))))

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
    (parse-pull-number rest 0 `#(game ,id ,(cons #M() pulls)))))

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
   (let (((cons pull pulls) pulls))
     `#(game ,id ,(cons (maps:update_with color (lambda (n) (+ n count)) count pull) pulls)))))

(defun gather-games (games)
  (receive
    (`#(game ,id ,pulls) (gather-games (cons `#(game ,id ,pulls) games)))
    ('done games)))

(defun legal-game?
  ((`#(game ,_ ,pulls)) (lists:all #'legal-pull?/1 pulls)))

(defun legal-pull? (m)
  (and (>= 12 (maps:get #"red" m 0))
       (>= 13 (maps:get #"green" m 0))
       (>= 14(maps:get #"blue" m 0))))

(defun legal-value
  ((`#(game ,id ,pulls))
   (if (legal-game? `#(game ,id ,pulls))
     `#(true ,id))))

(defun minimum-cubes
  ((`#(game ,_ ,pulls))
   (lists:foldl
    (lambda (m1 m2) (maps:merge_with
                     (lambda (k1 v1 v2) (max v1 v2))
                     m1 m2))
                #M(#"red" 0 #"green" 0 #"blue" 0) pulls)))

(defun power (m)
  (* (maps:get #"red" m 0) (maps:get #"green" m 0) (maps:get #"blue" m 0)))
