(defmodule day-5
  (export (day-five 1)))

(defun day-five (input)
  (let ((`#(,seeds ,rest) (seeds input)))
    (lists:min (lc ((<- seed seeds)) (seed-through (mappings rest) seed)))))

(defun seed-through (mappings seed)
  (seed-through
   (lists:foldl (match-lambda ((`#(,from ,to ,mapping) m)
                               (mset m from `#(,mapping ,to))))
                #M() mappings)
   #"seed"
   seed))

(defun seed-through
  ((_ #"location" range) range)
  ((mappings step range) (let ((`#(,filters ,to) (mref mappings step)))
                           (seed-through mappings to (range-through filters `(,range))))))

(defun seeds
  (((binary "seeds: " (rest binary)))
   (seeds () rest)))

(defun seeds
  ((nums (binary "\n\n" (rest binary)))
   `#(,(nums-to-ranges nums) ,rest))
  ((nums (binary " " (rest binary)))
   (seeds nums rest))
  ((nums rest)
   (let ((`#(,num ,rest) (num rest)))
     (seeds `(,num . ,nums) rest))))

(defun nums-to-ranges (nums)
  (lc ((<- num nums))
    `#(,num ,num)))

(defun num (input)
  (num #"" input))

(defun num
  ((num (binary n (rest binary))) (when (andalso (>= n #\0) (>= #\9 n)))
   (num (binary (num binary) n) rest))
  ((num rest)
   `#(,(binary_to_integer num) ,rest)))

(defun mappings (input)
  (mappings () input))

(defun mappings
  ((maps #"")
   maps)
  ((maps rest)
   (let ((`#(,mapping ,rest) (mapping rest)))
     (mappings `(,mapping . ,maps) rest))))

(defun mapping (input)
  (let* ((`#(,from ,to ,rest) (mapping-header input))
         (`#(,mapping ,rest) (mapping () rest))
         (mapping (lists:sort (match-lambda ((`#(,f1 ,_ ,_) `#(,f2 ,_ ,_)) (=< f1 f2))) mapping)))
    `#(#(,from ,to ,mapping) ,rest)))

(defun mapping-header (input)
  (mapping-header #() input))

(defun mapping-header
  ((`#(,from ,to) (binary "map:" (rest binary)))
   `#(,from ,to ,rest))
  ((#() rest)
   (let ((`#(,from ,rest) (word rest)))
     (mapping-header `#(,from) rest)))
  ((`#(,from) (binary "to-" (rest binary)))
   (mapping-header `#(,from) rest))
  ((`#(,from) rest)
   (let ((`#(,to ,rest) (word rest)))
     (mapping-header `#(,from ,to) rest))))

(defun word (input)
  (word #"" input))

(defun word
  ((word (binary " " (rest binary)))
   `#(,word ,rest))
  ((word (binary "-" (rest binary)))
   `#(,word ,rest))
  ((word (binary c (rest binary)))
   (word (binary (word binary) c) rest)))

(defun mapping
  ((ranges (binary "\n\n" (rest binary)))
   `#(,ranges ,rest))
  ((ranges #"\n")
   `#(,ranges #""))
  ((ranges (binary "\n" (rest binary)))
   (let ((`#(,range ,rest) (mapping-range #() rest)))
     (mapping `(,range . ,ranges) rest))))

(defun mapping-range
  ((sdr (binary " " (rest binary)))
   (mapping-range sdr rest))
  ((`#() rest)
   (let ((`#(,num ,rest) (num rest)))
     (mapping-range `#(,num) rest)))
  ((`#(,destination) rest)
   (let ((`#(,num ,rest) (num rest)))
     (mapping-range `#(,num ,destination) rest)))
  ((`#(,source ,destination) rest)
   (let ((`#(,num ,rest) (num rest)))
     (mapping-range `#(,source ,destination ,num) rest)))
  ((sdr rest)
   `#(,sdr ,rest)))

(defun range-through (filters inputs)
  (let ((`(,singleton-range) (range-through filters inputs ())))
    `#(,singleton-range ,singleton-range)))

(defun range-through
  ((_ () outputs) outputs)
  ((filters `(#(,from ,to) . ,rest) outputs) (when (== from to))
   (range-through filters rest `(,(case (lists:search (match-lambda ((`#(,source ,_ ,range))
                                                                     (andalso (>= from source) (< from (+ source range)))))
                                                      filters)
                                    (`#(value #(,source ,destination ,_)) (+ from (- destination source)))
                                    ('false from)) . ,outputs))))
