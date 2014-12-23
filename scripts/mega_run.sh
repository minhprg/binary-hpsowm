#!/bin/bash

cd ../out/production/src/

for (( i = 0; i < 50; i++ ))
do
	scala application.TrainBinaryHPSOWM > ../../../cmp_result/hpsowm_result_$i
	scala application.TrainBinaryPSO > ../../../cmp_result/bpso_result_$i
	scala application.TrainBinaryGA > ../../../cmp_result/ga_result_$i
done
