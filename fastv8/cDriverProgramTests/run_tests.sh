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
export PATH=$PATH:${FAST_DIR}/bin/
export FAST=FAST_cDriverProg_cInterface_glin64

# copy pass_fail script
cp $Fv8cDPRtest_DIR/pass_fail.sh $FAST_DIR/runFv8cDPRtest
cp $Fv8cDPRtest_DIR/*py $FAST_DIR/runFv8cDPRtest

#=============================================================================
# Test 01
#=============================================================================
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases/Test01" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/Test01
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/Test01
cp $Fv8cDPRtest_DIR/testCases/Test01/Test01.fst $FAST_DIR/runFv8cDPRtest/testCases/Test01/
cp $Fv8cDPRtest_DIR/testCases/Test01/cDriver.i* $FAST_DIR/runFv8cDPRtest/testCases/Test01/
cp $Fv8cDPRtest_DIR/testCases/Test01/5MW_Baseline $FAST_DIR/runFv8cDPRtest/testCases/Test01/ -r
cp $Fv8cDPRtest_DIR/testCases/Test01/test01.sh $FAST_DIR/runFv8cDPRtest/testCases/Test01/
cp $Fv8cDPRtest_DIR/testCases/Test01/Test01.nativeFortran.outb $FAST_DIR/runFv8cDPRtest/testCases/Test01/
# run it...  
#./test01.sh

#=============================================================================
# Test 02
#=============================================================================
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases/Test02" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/Test02
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/Test02
cp $Fv8cDPRtest_DIR/testCases/Test02/Test02.fst $FAST_DIR/runFv8cDPRtest/testCases/Test02/
cp $Fv8cDPRtest_DIR/testCases/Test02/cDriver.i* $FAST_DIR/runFv8cDPRtest/testCases/Test02/
cp $Fv8cDPRtest_DIR/testCases/Test02/5MW_Baseline/ $FAST_DIR/runFv8cDPRtest/testCases/Test02/ -r
cp $Fv8cDPRtest_DIR/testCases/Test02/test02.sh $FAST_DIR/runFv8cDPRtest/testCases/Test02/
cp $Fv8cDPRtest_DIR/testCases/Test02/Test02.nativeFortran.outb $FAST_DIR/runFv8cDPRtest/testCases/Test02/
# run it...  
#./test02.sh

#=============================================================================
# Test 03
#=============================================================================
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases/Test03" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/Test03
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/Test03
cp $Fv8cDPRtest_DIR/testCases/Test03/t*Test03.fst $FAST_DIR/runFv8cDPRtest/testCases/Test03/
cp $Fv8cDPRtest_DIR/testCases/Test03/cDriver.i* $FAST_DIR/runFv8cDPRtest/testCases/Test03/
cp $Fv8cDPRtest_DIR/testCases/Test03/5MW_Baseline/ $FAST_DIR/runFv8cDPRtest/testCases/Test03/ -r
cp $Fv8cDPRtest_DIR/testCases/Test03/test03.sh $FAST_DIR/runFv8cDPRtest/testCases/Test03/
cp $Fv8cDPRtest_DIR/testCases/Test03/t*Test03.nativeFortran.outb $FAST_DIR/runFv8cDPRtest/testCases/Test03/
# run it...  
#./test03.sh


#=============================================================================
# Test 04
#=============================================================================
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases/Test04" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/Test04
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/Test04
cp $Fv8cDPRtest_DIR/testCases/Test04/cDriver.i $FAST_DIR/runFv8cDPRtest/testCases/Test04/
cp $Fv8cDPRtest_DIR/testCases/Test04/test04.sh $FAST_DIR/runFv8cDPRtest/testCases/Test04/
cp $Fv8cDPRtest_DIR/testCases/Test04/log.Test04.gold $FAST_DIR/runFv8cDPRtest/testCases/Test04/
# run it...  
#./test04.sh


#=============================================================================
# Test 05
#=============================================================================
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases/Test05" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/Test05
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/Test05
cp $Fv8cDPRtest_DIR/testCases/Test05/t*Test05.fst $FAST_DIR/runFv8cDPRtest/testCases/Test05/
cp $Fv8cDPRtest_DIR/testCases/Test05/cDriver.i* $FAST_DIR/runFv8cDPRtest/testCases/Test05/
cp $Fv8cDPRtest_DIR/testCases/Test05/5MW_Baseline $FAST_DIR/runFv8cDPRtest/testCases/Test05/ -r
cp $Fv8cDPRtest_DIR/testCases/Test05/test05.sh $FAST_DIR/runFv8cDPRtest/testCases/Test05/
cp $Fv8cDPRtest_DIR/testCases/Test05/t*Test05.nativeFortran.outb $FAST_DIR/runFv8cDPRtest/testCases/Test05/
# run it...  
#./test05.sh

#=============================================================================
# Test 06
#=============================================================================
if [ ! -d "$FAST_DIR/runFv8cDPRtest/testCases/Test06" ]; then
    mkdir $FAST_DIR/runFv8cDPRtest/testCases/Test06
fi

cd $FAST_DIR/runFv8cDPRtest/testCases/Test06
cp $Fv8cDPRtest_DIR/testCases/Test06/t*Test06.fst $FAST_DIR/runFv8cDPRtest/testCases/Test06/
cp $Fv8cDPRtest_DIR/testCases/Test06/cDriver.i* $FAST_DIR/runFv8cDPRtest/testCases/Test06/
cp $Fv8cDPRtest_DIR/testCases/Test06/5MW_Baseline $FAST_DIR/runFv8cDPRtest/testCases/Test06/ -r
cp $Fv8cDPRtest_DIR/testCases/Test06/libScontroller.so $FAST_DIR/runFv8cDPRtest/testCases/Test06/
cp $Fv8cDPRtest_DIR/testCases/Test06/test06.sh $FAST_DIR/runFv8cDPRtest/testCases/Test06/
cp $Fv8cDPRtest_DIR/testCases/Test06/t*Test06.nativeFortran.outb $FAST_DIR/runFv8cDPRtest/testCases/Test06/
# run it...  
./test06.sh


echo "Rtest End"



