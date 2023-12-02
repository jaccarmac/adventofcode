YEAR=$1
DAY=$2
DATA=${3:-$DAY}

cd $YEAR
guix shell --container --network --manifest=manifest.scm -- ./run-day.sh $DAY $DATA
