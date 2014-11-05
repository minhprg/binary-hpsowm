#!/bin/bash

#rm ./cmp_result/*

cd ../out/production/src/

for (( i = 0; i < 50; i++ ))
do
	scala application.TrainBinaryPSO > ../../../cmp_result/bpso_result_$i
#	sleep 1
done
