DAY=$1

cd $DAY
kotlinc $DAY.kt -include-runtime -d $DAY.jar
java -jar $DAY.jar
