#!/bin/bash
. ../../pass_fail.sh

CWD=$(pwd)
didSimulationDiffAnywhere=0

if [ -f $CWD/PASS ]; then
    # already ran this test
    didSimulationDiffAnywhere=0
else
    make -f makefile_DISCON_DLL COMPILER=${COMPILER} BUILD=${BUILD} &> log.make_DISCON_DLL
    cp cDriver.i.1 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $FAST &> log.Test02.1"
    else
	$FAST &> log.Test02.1
    fi
    cp cDriver.i.2 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $FAST &> log.Test02.2"
    else
	$FAST &> log.Test02.2
    fi
    cp cDriver.i.3 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $FAST &> log.Test02.3"
    else
	$FAST &> log.Test02.3
    fi
    cp cDriver.i.4 cDriver.i
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	ssh `hostname`-mic0 "cd $PWD; source ../../../../phi.env; $FAST &> log.Test02.4"
    else
	$FAST &> log.Test02.4
    fi
    determine_pass_fail Test02.T1.outb Test02.nativeFortran.outb
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
    echo -e "..Test02................ FAILED"
else
    echo -e "..Test02................ PASSED"
fi

exit
