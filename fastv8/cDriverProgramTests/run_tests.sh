#!/bin/bash
#Fv8cDP - FAST v8 C Driver Program

# possible flag passed in
typeOfTesting=$1

echo "Rtest Begin: " $typeOfTesting

Fv8cDPRtest_DIR=$(pwd)

cd ..
FAST_DIR=$(pwd)

# create the directory in which the tests will be run (if it does not exist)
if [ ! -d "$FAST_DIR/runFv8cDPRtest" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest
fi

# now check for testCases
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases
fi

source ../sourceMods.sh
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${FAST_DIR}/bin/:${FAST_DIR}/CertTest/5MW_Baseline/ServoData/
export FAST=FAST_glin64

# copy pass_fail script
cp $Fv8cDPRtest_DIR/pass_fail.sh $FAST_DIR/runFv8cDPRtest

#=============================================================================
# Test 1
#=============================================================================
if [ ! -d "$baseGitfHub_DIR/runFv8cDPRtest/testCases/test1" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/test1
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/test1
cp $Fv8cDPRtest_DIR/testCases/test1/test1.fst $FAST_DIR/runFv8cDPRtest/testCases/test1/
cp $Fv8cDPRtest_DIR/testCases/test1/5MW_Baseline $FAST_DIR/runFv8cDPRtest/testCases/test1/
cp $Fv8cDPRtest_DIR/xml/milestone.xml $FAST_DIR/runFv8cDPRtest/testCases/test1/
cp $Fv8cDPRtest_DIR/testCases/test1/test1.sh $FAST_DIR/runFv8cDPRtest/testCases/test1/
cp $Fv8cDPRtest_DIR/testCases/test1/test1.norm.gold* $FAST_DIR/runFv8cDPRtest/testCases/test1
# run it...  
./test1.sh


echo "Rtest End"

