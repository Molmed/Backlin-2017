#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ $# != 2 ]; then
    echo "Usage ./benchmark.sh <R-script> <title>"
    exit 0
fi

[ -f $2.ps.log ] && rm $2.ps.log
[ -f $2.ps.raw ] && rm $2.ps.raw

$DIR/syrupy.py --no-raw-process-log --separator=, --no-align --title=$2 R --vanilla -f $1

