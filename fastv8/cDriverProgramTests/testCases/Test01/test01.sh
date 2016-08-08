#!/bin/bash
. ../../pass_fail.sh

CWD=$(pwd)
didSimulationDiffAnywhere=0

if [ -f $CWD/PASS ]; then
    # already ran this test
    didSimulationDiffAnywhere=0
else
    $(FAST) Test01.fst
    determine_pass_fail Test01.T1.out Test01.nativeFortran.out
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
