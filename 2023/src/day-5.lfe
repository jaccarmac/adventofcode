(defmodule day-5
  (export (day-five 1)))

(defun day-five (input)
  (let* ((`#(,seeds ,rest) (seeds input))
         (mappings (mappings rest)))
    `#(,(tref (car (seeds-through mappings (lc ((<- seed seeds)) `#(,seed ,seed)))) 1)
       ,(tref (car (seeds-through mappings (seeds-to-ranges seeds))) 1))))

(defun seeds-to-ranges (seeds)
  (seeds-to-ranges seeds ()))

(defun seeds-to-ranges
  ((() ranges) ranges)
  ((`(,length ,start . ,rest) ranges) (seeds-to-ranges rest `(#(,start ,(+ start (- length 1))) . ,ranges))))

(defun seeds-through (mappings seeds)
  (seeds-through
   (lists:foldl (match-lambda ((`#(,from ,to ,mapping) m)
                               (mset m from `#(,mapping ,to))))
                #M() mappings)
   #"seed"
   seeds))

(defun seeds-through
  ((_ #"location" ranges) (sort-merge-ranges ranges))
  ((mappings step ranges)
   (let ((`#(,filters ,to) (mref mappings step)))
     (seeds-through mappings to (range-through filters ranges)))))

(defun sort-merge-ranges (ranges)
  (let ((ranges (lists:sort (match-lambda ((`#(,f1 ,_) `#(,f2 ,_)) (=< f1 f2))) ranges)))
    ranges))

(defun seeds
  (((binary "seeds: " (rest binary)))
   (seeds () rest)))

(defun seeds
  ((nums (binary "\n\n" (rest binary)))
   `#(,nums ,rest))
  ((nums (binary " " (rest binary)))
   (seeds nums rest))
  ((nums rest)
   (let ((`#(,num ,rest) (num rest)))
     (seeds `(,num . ,nums) rest))))

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
  (range-through filters inputs ()))

(defun range-through
  ((_ () outputs) outputs)
  ((filters `(#(,from ,to) . ,rest) outputs)
   (let* ((used (lists:dropwhile (match-lambda ((`#(,source ,_ ,range))
                                                (>= from (+ source range)))) filters))
          (`#(,source ,destination ,range) (case used
                                             (() `#(0 0 ,(+ 1 to)))
                                             (`(#(,source ,destination ,range) . ,_) (if (>= from source)
                                                                                       `#(,source ,destination ,range)
                                                                                       `#(0 0 ,source)))))
          (split (split-range `#(,from ,to) (+ source range)))
          (todo (case split (`#(,_ #()) rest) (`#(,_ ,split) `(,split . ,rest))))
          (transformed (let ((`#(#(,from ,to) ,_) split) (to-add (- destination source))) `#(,(+ to-add from) ,(+ to-add to)))))
     (range-through filters todo `(,transformed . ,outputs)))))

(defun split-range (range loc)
  (let ((`#(,from ,to) range))
    (if (>= to loc)
      `#(#(,from ,(- loc 1)) #(,loc ,to))
      `#(,range #()))))
