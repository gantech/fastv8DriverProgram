#!/bin/bash
. ../../pass_fail.sh

CWD=$(pwd)
didSimulationDiffAnywhere=0

if [ -f $CWD/PASS ]; then
    # already ran this test
    didSimulationDiffAnywhere=0
else
    cp cDriver.i.1 cDriver.i
    mpirun -np 1 $FAST &> log.Test03.1
    cp cDriver.i.2 cDriver.i
    mpirun -np 1 $FAST &> log.Test03.2
    cp cDriver.i.3 cDriver.i
    mpirun -np 1 $FAST &> log.Test03.3
    cp cDriver.i.4 cDriver.i
    mpirun -np 1 $FAST &> log.Test03.4
    determine_pass_fail t1_Test03.T1.outb t1_Test03.nativeFortran.outb
    t1_pf=$?
    determine_pass_fail t2_Test03.T2.outb t2_Test03.nativeFortran.outb
    t2_pf=$?
    [[ (( t1_pf -eq 0 )) && ((t2_pf -eq 0)) ]]
    didSimulationDiffAnywhere="$?"
fi

# write the file based on final status
if [ "$didSimulationDiffAnywhere" -gt 0 ]; then
    PASS_STATUS=0
else
    PASS_STATUS=1
    echo $PASS_STATUS > PASS
fi

# report it; 30 spaces
if [ $PASS_STATUS -ne 1 ]; then
    echo -e "..Test03................ FAILED"
else
    echo -e "..Test03................ PASSED"
fi

exit
