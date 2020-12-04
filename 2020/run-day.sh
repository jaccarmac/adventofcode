DAY=$1

guix environment -m guix-manifest.scm -- ./compile-and-run-day.sh $DAY
