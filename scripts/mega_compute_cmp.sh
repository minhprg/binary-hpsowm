#!/bin/bash

cd ../out/production/src/

for (( i = 1; i <= 40; i++ ))
do
	let "j = $i*500"
	scala application.ComputeCompare 1 $j
done
