#!/bin/bash
. ../../pass_fail.sh

CWD=$(pwd)
didSimulationDiffAnywhere=0

if [ -f $CWD/PASS ]; then
    # already ran this test
    didSimulationDiffAnywhere=0
else
    make -f makefile_DISCON_DLL &> log.make_DISCON_DLL
    make -f makefileSC &> log.makeSC
    cp cDriver.i.1 cDriver.i
    mpirun -np 2 $FAST &> log.Test06.1
    cp cDriver.i.2 cDriver.i
    mpirun -np 2 $FAST &> log.Test06.2
    cp cDriver.i.3 cDriver.i
    mpirun -np 2 $FAST &> log.Test06.3
    cp cDriver.i.4 cDriver.i
    mpirun -np 2 $FAST &> log.Test06.4
    determine_pass_fail t1_Test06.T1.outb t1_Test06.nativeFortran.outb
    t1_pf=$?
    determine_pass_fail t2_Test06.T2.outb t2_Test06.nativeFortran.outb
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
    echo -e "..Test06................ FAILED"
else
    echo -e "..Test06................ PASSED"
fi

exit
