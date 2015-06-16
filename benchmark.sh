#!/bin/bash

if [ $# != 2 ]; then
    echo "Usage ./benchmark.sh <R-script> <title>"
    exit 0
fi

[ -f $2.ps.log ] && rm $2.ps.log
[ -f $2.ps.raw ] && rm $2.ps.raw

syrupy.py --no-raw-process-log --separator=, --no-align --title=$2 R --vanilla -f $1

