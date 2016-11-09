#!/bin/bash
. ../../pass_fail.sh

CWD=$(pwd)
didSimulationRun=0

if [ -f $CWD/PASS ]; then
    # already ran this test
    didSimulationRun=0
else
    cd 5MW_Baseline/ServoData/Source/
    python generateTurbineControllerCases.py 8
    cd ../../../
    make -f makefile_DISCON_DLL COMPILER=${COMPILER} BUILD=${BUILD} &> log.make_DISCON_DLL
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; mpirun -np 8 $FAST &> log.Test07"
    else
	mpirun -np 8 $FAST &> log.Test07
    fi
    didSimulationRun="$?"
fi

# write the file based on final status
if [ "$didSimulationRun" -gt 0 ]; then
    PASS_STATUS=0
else
    PASS_STATUS=1
    echo $PASS_STATUS > PASS
fi

# report it; 30 spaces
if [ $PASS_STATUS -ne 1 ]; then
    echo -e "..Test07................ FAILED"
else
    echo -e "..Test07................ PASSED"
fi

exit

