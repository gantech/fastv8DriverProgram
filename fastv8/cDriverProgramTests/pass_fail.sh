#!/bin/bash

determine_pass_fail() {

    diffAnywhere=0
    logFileName=$1
    goldLogFileName=$2
    
    if [ ! -f ${logFileName} ]
    then
	diffAnywhere=1
    elif [ ! -f ${goldLogFileName} ]
    then
	diffAnywhere=1
    else
#	diff <(tail -n +3 $logFileName) <(tail -n +3 $goldLogFileName)
	python ../../compareTwoFASTruns.py $logFileName $goldLogFileName

	if [ $? -ne 0 ]
	then
	    diffAnywhere=1
	fi
    fi
    
    return $diffAnywhere
}


determine_pass_fail blah1 blah2
