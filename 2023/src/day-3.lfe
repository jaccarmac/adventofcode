(defmodule day-3
  (export (three-one 1)))

(defun three-one (grid)
  (let* ((s (self))
         (summer (spawn (lambda () (summer s)))))
    (scan grid summer)
    (receive (n n))))

(defun summer (top)
  (receive
    (`#(nums ,nums) (summer top nums))))

(defun summer (top remaining)
  (summer top remaining 0))

(defun summer
  ((top 0 sum) (! top sum))
  ((top remaining sum) (receive (n (summer top (- remaining 1) (+ sum n))))))

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
  (scan-number grid at nums syms summer 0 (sets:new)))

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
  (lists:foldl #'sets:add_element/2 neighbors (let ((`#(,x ,y) at))
                                                `(#(,(- x 1) ,y)
                                                  #(,(- x 1) ,(+ y 1))
                                                  #(,x ,(+ y 1))
                                                  #(,(+ x 1) ,(+ y 1))
                                                  #(,(+ x 1) ,y)
                                                  #(,(+ x 1) ,(- y 1))
                                                  #(,(- x 1) ,y)
                                                  #(,(- x 1) ,(- y 1))))))

(defun next-char
  ((`#(,l ,c)) `#(,l ,(+ 1 c))))

(defun next-line
  ((`#(,l ,c)) `#(,(+ 1 l) 0)))

(defun check-number (me neighbors syms summer)
  (! syms `#(check ,(self) ,(sets:to_list neighbors)))
  (receive
    ('true (! summer me))
    ('false (! summer 0))))

(defun symbol-registry ()
  (symbol-registry #M()))

(defun symbol-registry (syms)
  (receive
    (`#(symbol ,s ,at) (symbol-registry (map-set syms at s)))
    ('done (symbol-check syms))))

(defun symbol-check (syms)
  (receive
    (`#(check ,p ,coords)
     (! p (lists:any (lambda (c) (maps:is_key c syms)) coords))
     (symbol-check syms))))
