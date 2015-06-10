#!/bin/bash

if [ $# != 3 ]
then
    echo "Usage ./benchmark.sh <R-script> <title> <multicore:y/n>"
    exit 0
fi

[ -f $2.ps.log ] && rm $2.ps.log
[ -f $2.ps.raw ] && rm $2.ps.raw
if [ "$3" == "y" ]; then
	# Multicore monitoring by process name
	syrupy.py --no-raw-process-log --separator=, --no-align --title=$2 \
		      --poll-command 'R' &
	SYRUPY=$!
	R --vanilla -f $1 > /dev/null
	kill $SYRUPY
else
	# Single core monitoring by PID
	syrupy.py --no-raw-process-log --separator=, --no-align --title=$2 \
		      R --vanilla -f $1
fi
