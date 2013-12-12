#! /bin/bash

echo "Job started" >> memstats/$1.log
date +"%Y-%m-%d %H:%M:%S" > memstats/$1.log
cat /proc/meminfo >> memstats/$1.log

while true
do
    sleep 10
    date +"%Y-%m-%d %H:%M:%S" >> memstats/$1.log
    sed 's/^[^:]*:\s*//' /proc/meminfo >> memstats/$1.log
done
