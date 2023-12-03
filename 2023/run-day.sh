DAY=$1
DATA=$2

lfe -eval "(c \"src/day-1\")"
lfe -eval "(c \"src/2023\")"
lfe -eval "(|2023:main| '(\"$DAY\" \"$DATA\"))"

# TERM=vt100 guix shell -CNm manifest.scm -E TERM -- lfe
#
# Passing through TERM is the way to get the REPL to work inside a Guix
# container. It even works for Rebar3's REPL, though Rebar3 will still not lfe
# run-escript (some dependency on bash-minimal that's caught in the Guix
# build.) <- TODO
