DAY=$1

declare -A solutions
solutions=(
    ["1"]="chronal_calibration"
    ["2"]="inventory_management_system"
    ["3"]="no_matter_how_you_slice_it"
    ["4"]="repose_record"
    ["5"]="alchemical_reduction"
    ["6"]="chronal_coordinates"
    ["7"]="the_sum_of_its_parts"
    ["8"]="memory_maneuver"
    ["9"]="marble_mania"
    ["13"]="mine_cart_madness"
    ["14"]="chocolate_charts")

nim c -r src/${solutions[$DAY]}.nim data/$DAY.txt
