#!/bin/bash
. ../../pass_fail.sh

CWD=$(pwd)
didSimulationDiffAnywhere=0

if [ -f $CWD/PASS ]; then
    # already ran this test
    didSimulationDiffAnywhere=0
else
    make -f makefile_DISCON_DLL COMPILER=${COMPILER} BUILD=${BUILD} &> log.make_DISCON_DLL
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $nativeFAST t1_Test05.fst &> log.t1_Test05.nativeFAST; $nativeFAST t2_Test05.fst &> log.t2_Test05.nativeFAST; $nativeFAST t3_Test05.fst &> log.t3_Test05.nativeFAST"
    else
	$nativeFAST t1_Test05.fst &> log.t1_Test05.nativeFAST
	$nativeFAST t2_Test05.fst &> log.t2_Test05.nativeFAST
	$nativeFAST t3_Test05.fst &> log.t3_Test05.nativeFAST
    fi
    mv t1_Test05.outb t1_Test05.nativeFortran.outb
    mv t2_Test05.outb t2_Test05.nativeFortran.outb
    mv t3_Test05.outb t3_Test05.nativeFortran.outb

    cp cDriver.i.1 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; mpirun -np 4 $cFAST &> log.Test05.1"
    else
	mpirun -np 4 $cFAST &> log.Test05.1
    fi
    cp cDriver.i.2 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; mpirun -np 4 $cFAST &> log.Test05.2"
    else
	mpirun -np 4 $cFAST &> log.Test05.2
    fi
    cp cDriver.i.3 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; mpirun -np 4 $cFAST &> log.Test05.3"
    else
	mpirun -np 4 $cFAST &> log.Test05.3
    fi
    cp cDriver.i.4 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; mpirun -np 4 $cFAST &> log.Test05.4"
    else
	mpirun -np 4 $cFAST &> log.Test05.4
    fi
    determine_pass_fail t1_Test05.T1.outb t1_Test05.nativeFortran.outb
    t1_pf=$?
    determine_pass_fail t2_Test05.T2.outb t2_Test05.nativeFortran.outb
    t2_pf=$?
    determine_pass_fail t3_Test05.T3.outb t3_Test05.nativeFortran.outb
    t3_pf=$?
    [[ (( t1_pf -eq 0 )) && ((t2_pf -eq 0)) && ((t3_pf -eq 0)) ]]
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
    echo -e "..Test05................ FAILED"
else
    echo -e "..Test05................ PASSED"
fi

exit
