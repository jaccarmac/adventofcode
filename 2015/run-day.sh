DAY=$1

declare -A solutions
solutions=( ["1"]="not-quite-lisp")

cd $DAY
elm make ${solutions[$DAY]}.elm
