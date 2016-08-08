#!/bin/bash

determine_pass_fail() {

    diffAnywhere = 0
    logFileName = $1
    goldLogFileName = $2

    diff <(tail -n +3 ${logFileName}) <(tail -n +3 ${goldLogFileName})

    if [$? -ne 0 ]
    then
	diffAnywhere=1
    fi
    
    return $diffAnywhere
}