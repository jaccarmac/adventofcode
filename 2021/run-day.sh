DAY=$1
DATA=$2

sbcl --noinform --noprint --non-interactive \
     --load run-day.lisp \
     --eval "(print-solutions $DAY \"$DATA\")"
