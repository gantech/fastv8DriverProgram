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
# Test 01
#=============================================================================
if [ ! -d "$baseGitfHub_DIR/runFv8cDPRtest/testCases/test01" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/test01
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/test01
cp $Fv8cDPRtest_DIR/testCases/test01/test01.fst $FAST_DIR/runFv8cDPRtest/testCases/test01/
cp $Fv8cDPRtest_DIR/testCases/test01/5MW_Baseline $FAST_DIR/runFv8cDPRtest/testCases/test01/
cp $Fv8cDPRtest_DIR/testCases/test01/test01.sh $FAST_DIR/runFv8cDPRtest/testCases/test01/
cp $Fv8cDPRtest_DIR/testCases/test01/Test01.nativeFortran.out $FAST_DIR/runFv8cDPRtest/testCases/test01/
# run it...  
./test01.sh


echo "Rtest End"

