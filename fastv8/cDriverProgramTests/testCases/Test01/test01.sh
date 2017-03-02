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
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $nativeFAST Test01.fst &> log.Test01.nativeFAST; $cFAST &> log.Test01"
    else
	$nativeFAST Test01.fst &> log.Test01.nativeFAST
	mv Test01.outb Test01.nativeFortran.outb
	$cFAST &> log.Test01
    fi
    determine_pass_fail Test01.T1.outb Test01.nativeFortran.outb
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
    echo -e "..Test01................ FAILED"
else
    echo -e "..Test01................ PASSED"
fi

exit
