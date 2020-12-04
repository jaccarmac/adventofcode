YEAR=$1
DAY=$2

cd $YEAR
guix environment -m guix-manifest.scm -- ./run-day.sh $DAY
