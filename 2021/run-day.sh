DAY=$1

sbcl --noinform --noprint --disable-debugger \
     --load run-day.lisp \
     --eval "(print-solutions $DAY)" \
     --quit
