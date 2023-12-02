DAY=$1
DATA=$2

lfe -eval "(c \"src/2023\")" -eval "(|2023:main| '(\"$DAY\" \"$DATA\"))"
