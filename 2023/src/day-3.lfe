(defmodule day-3
  (export (three-one 1))
  (import
   (from lists
         (filtermap 2)
         (foldl 3)
         (sum 1))
   (rename lists ((map 2) mapcar))
   (from maps
         (get 2)
         (is_key 2)
         (update_with 4))
   (rename maps ((to_list 1) map-to-list))
   (from sets
         (new 0))
   (rename sets
           ((new 0) new-set)
           ((to_list 1) set-to-list))))

(defun three-one (grid)
  (let* ((s (self))
         (summer (spawn (lambda () (summer s)))))
    (scan grid summer)
    (receive (solution solution))))

(defun summer (top)
  (receive
    (`#(nums ,nums) (summer top nums))))

(defun summer (top remaining)
  (summer top remaining 0 #M()))

(defun summer
  ((top 0 sum gears) (! top `#(,sum ,(gear-sum gears))))
  ((top remaining sum gears)
   (receive
     (`#(gear ,at ,n) (summer top remaining sum (update-gears gears at n)))
     (`#(part ,n) (summer top (- remaining 1) (+ sum n) gears)))))

(defun update-gears (gears at n)
  (update_with at (lambda (ns) `(,n . ,ns)) `(,n) gears))

(defun gear-sum (gears)
  (sum (mapcar (match-lambda
                 ((`#(,_ (,g1 ,g2))) (* g1 g2))
                 ((_) 0))
               (map-to-list gears))))

(defun scan (grid summer)
  (scan grid #(0 0) 0 (spawn #'symbol-registry/0) summer))

(defun scan (grid at nums syms summer)
  (case grid
    (#B()
       (! syms 'done)
       (! summer `#(nums ,nums)))
    ((binary #\. (rest binary))
     (scan rest (next-char at) nums syms summer))
    ((binary "\n" (rest binary))
     (scan rest (next-line at) nums syms summer))
    ((binary c (_ binary)) (when (and (=< #\0 c) (=< c #\9)))
     (scan-number grid at nums syms summer))
    ((binary c (rest binary))
     (! syms `#(symbol ,c ,at))
     (scan rest (next-char at) nums syms summer))))

(defun scan-number (grid at nums syms summer)
  (scan-number grid at nums syms summer 0 (new-set)))

(defun scan-number (grid at nums syms summer num neighbors)
  (case grid
    ((binary n (rest binary)) (when (and (=< #\0 n) (=< n #\9)))
     (let* ((digit (- n #\0))
            (me (+ digit (* 10 num))))
       (scan-number rest (next-char at) nums syms summer me (add-neighbors neighbors at))))
    (_
     (spawn (lambda () (check-number num neighbors syms summer)))
     (scan grid at (+ 1 nums) syms summer))))

(defun add-neighbors (neighbors at)
  (foldl #'sets:add_element/2 neighbors (let ((`#(,x ,y) at))
                                          `(#(,(- x 1) ,y)
                                            #(,(- x 1) ,(+ y 1))
                                            #(,x ,(+ y 1))
                                            #(,(+ x 1) ,(+ y 1))
                                            #(,(+ x 1) ,y)
                                            #(,(+ x 1) ,(- y 1))
                                            #(,x ,(- y 1))
                                            #(,(- x 1) ,(- y 1))))))

(defun next-char
  ((`#(,l ,c)) `#(,l ,(+ 1 c))))

(defun next-line
  ((`#(,l ,c)) `#(,(+ 1 l) 0)))

(defun check-number (me neighbors syms summer)
  (! syms `#(check ,(set-to-list neighbors) ,me ,summer)))

(defun symbol-registry ()
  (symbol-registry #M()))

(defun symbol-registry (syms)
  (receive
    (`#(symbol ,s ,at) (symbol-registry (map-set syms at s)))
    ('done (symbol-check syms))))

(defun symbol-check (syms)
  (receive
    (`#(check ,coords ,part ,summer)
     (let ((adjacents (filtermap (lambda (c) (is_key c syms)) coords)))
       (mapcar (lambda (c) (if (== #\* (get c syms)) (! summer `#(gear ,c ,part)))) adjacents)
       (! summer `#(part ,(case adjacents (() 0) (_ part))))
       (symbol-check syms)))))
