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
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $nativeFAST t1_Test03.fst &> log.t1_Test03.nativeFAST; $nativeFAST t2_Test03.fst &> log.t2_Test03.nativeFAST"
    else
	$nativeFAST t1_Test03.fst &> log.t1_Test03.nativeFAST
	$nativeFAST t2_Test03.fst &> log.t2_Test03.nativeFAST
    fi
    mv t1_Test03.outb t1_Test03.nativeFortran.outb
    mv t2_Test03.outb t2_Test03.nativeFortran.outb

    cp cDriver.i.1 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $cFAST &> log.Test03.1"
    else
	$cFAST &> log.Test03.1
    fi
    cp cDriver.i.2 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $cFAST &> log.Test03.2"
    else
	$cFAST &> log.Test03.2
    fi
    cp cDriver.i.3 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $cFAST &> log.Test03.3"
    else
	$cFAST &> log.Test03.3
    fi
    cp cDriver.i.4 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $cFAST &> log.Test03.4"
    else
	$cFAST &> log.Test03.4
    fi
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
