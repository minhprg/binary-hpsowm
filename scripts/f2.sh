#!/bin/bash
BHPSOWM_FUNCTION=2
BHPSOWM_OPTIMAL=0
cd ../out/production/src/
for (( i = 0; i < 50; i++ ))
do
	scala application.TrainBinaryHPSOWM $BHPSOWM_FUNCTION $BHPSOWM_OPTIMAL -2.048 2.048 > ../../../cmp_result/$BHPSOWM_FUNCTION/hpsowm_result_$i
	scala application.TrainBinaryPSO $BHPSOWM_FUNCTION $BHPSOWM_OPTIMAL -2.048 2.048 > ../../../cmp_result/$BHPSOWM_FUNCTION/bpso_result_$i
	scala application.TrainBinaryGA $BHPSOWM_FUNCTION $BHPSOWM_OPTIMAL -2.048 2.048 > ../../../cmp_result/$BHPSOWM_FUNCTION/ga_result_$i
done