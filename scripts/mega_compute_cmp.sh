#!/bin/bash

for (( i = 1; i <= 40; i++ ))
#for (( i = 1; i <= 20; i++))
do
	let "j = $i*500"
	scala application.ComputeCompare 1 $j
done
