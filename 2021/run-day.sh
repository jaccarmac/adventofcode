DAY=$1

sbcl --noinform --noprint \
     --load run-day.lisp \
     --eval "(print-solutions $DAY)" \
     --quit
