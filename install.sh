#!/bin/bash

passFail() {
    if [ $1 -eq 0 ]
    then
	echo "... PASSED"
    else
	echo "... FAILED"
    fi
}

prepareSourceMods() {
#Prepare sourceMods.sh
    echo -n "Sourcing modules"
    echo -n $PWD > /tmp/fastDir
    echo "COMPILER=${COMPILER}" > sourceMods.sh
    echo "BUILD=${BUILD}" >> sourceMods.sh
    echo "LAPACK=${LAPACK}" >> sourceMods.sh
    sed -e "s/FASTDIR/$(sed 's:/:\\/:g' /tmp/fastDir)/" .sourceMods.sh >> sourceMods.sh
    source sourceMods.sh
    module list
    passFail $?
    
    echo -n "Setting up lib and bin directories"
    [ -d fastv8/lib ] || mkdir fastv8/lib
    [ -d fastv8/bin ] || mkdir fastv8/bin
    passFail $?
}

compileRegistry() {
#Registry
    echo -n "Compiling Registry"
    cd fastv8/Source/dependencies/Registry/
    make clean &> /dev/null
    make COMPILER=${COMPILER} &> log.make
    passFail $?
    cd ../../../../
}

compileLapack() {
#Registry
    echo -n "Compiling Lapack"
    [ -d fastv8/Source/dependencies/lapack ] || mkdir fastv8/Source/dependencies/lapack
    cd fastv8/Source/dependencies/lapack
    wget http://www.netlib.org/lapack/lapack-3.6.0.tgz &> log.wget
    tar -zxf lapack-3.6.0.tgz &> log.untar
    [ -d build ] || mkdir build
    cd build
    make clean
    rm CMakeCache.txt
    cmake -DCMAKE_CXX_COMPILER=g++ -DCMAKE_C_COMPILER=gcc -DCMAKE_FORTRAN_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=../../../../ -DCMAKE_BUILD_TYPE=RELEASE -DBUILD_SHARED_LIBS=ON -DBUILD_DEPRECATED=ON -DLAPACKE=ON ../lapack-3.6.0 &> log.config
    make -j 8 &> log.make
    make install &> log.makeInstall
    passFail $?
    cd ../../../../../
}

compileMapPlusPlus() {
#map-plus-plus
    echo -n "Compiling map-plus-plus"
    cd fastv8/Source/dependencies/map-plus-plus/src/
    make clean &> /dev/null
    if [ "${COMPILER}" == 'intelPhi' ] ; then
	icpc -c -mmic  -o lmroutines.o lmroutines.cc
    fi
    make COMPILER=${COMPILER} BUILD=${BUILD} LAPACK=${LAPACK} &> log.make
    passFail $?
    cd ../../../../../
}

compileYAMLcpp() {
#yaml-cpp
    echo "Compiling yaml-cpp"
    echo -n "   Setting up build directory"
    cd fastv8/Source/dependencies/yaml-cpp
    rm -rf build
    [ -d build ] || mkdir build
    cd build
    passFail $?
    echo -n "   Configuring"
    if [ "${COMPILER}" == 'gnu' ] ; then
	cmake ../yaml-cpp/ -DCMAKE_CXX_COMPILER=g++ -DCMAKE_CXX_FLAGS="-std=c++11" -DCMAKE_INSTALL_PREFIX=../../../../ &> log.cmake
    elif [ "${COMPILER}" == 'intel' ] ; then
	cmake ../yaml-cpp/ -DCMAKE_CXX_COMPILER=icpc -DCMAKE_CXX_FLAGS="-std=c++11" -DCMAKE_INSTALL_PREFIX=../../../../ &> log.cmake
    elif [ "${COMPILER}" == 'intelPhi' ] ; then
	cmake ../yaml-cpp/ -DCMAKE_CXX_COMPILER=icpc -DCMAKE_CXX_FLAGS="-std=c++11 -mmic" -DCMAKE_INSTALL_PREFIX=../../../../ &> log.cmake
    fi
    passFail $? 
    echo -n "   Compiling"
    make -j 8 &> log.make
    passFail $?
    echo -n "   Installing"
    make install &> log.makeInstall
    passFail $?
    cd ../../../../../
}

compileFAST() {
#FAST 
    cd fastv8/Compiling
    echo "Compiling FAST"
    echo -n "   Compiling FAST Library"
#    make clean &> /dev/null
    make COMPILER=${COMPILER} BUILD=${BUILD} LAPACK=${LAPACK} &> log.make #Basic FAST Library
    passFail $?
    echo -n "   Compiling Bladed style controller libraries"
    make -f makefile_DISCON_DLL COMPILER=${COMPILER} BUILD=${BUILD} LAPACK=${LAPACK} &> log.make_DISCON_DLL #Bladed style controller libraries 
    passFail $?
    echo -n "   Compiling Fortran driver program"
    make FAST_driver=FAST_Prog COMPILER=${COMPILER} BUILD=${BUILD} LAPACK=${LAPACK} &> log.make_FAST_Prog #Fortran driver program
    passFail $?
    echo -n "   Compiling C driver program"
    make -f makefileCDriverProg COMPILER=${COMPILER} BUILD=${BUILD} LAPACK=${LAPACK} &> log.makeCDriverProg #C driver program
    passFail $?
    cd ../../
}

prepPhiEnv() {
    echo -n "Prepping phi.env"
    echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" > phi.env
    echo "export PATH=$PATH" >> phi.env
    echo "export FAST=FAST_ProgC_glin64" >> phi.env
    passFail $?
}


prepareSourceMods
compileRegistry
if [ "${LAPACK}" == 'lapack' ]; then
    if [ "${COMPILER}" == 'gnu' ] ; then
	compileLapack
    else
	echo  "Can't use lapack with Intel compilers. Please use mkl instead."
	exit 1
    fi
fi
compileMapPlusPlus
compileYAMLcpp
compileFAST
if [ "${COMPILER}" == 'intelPhi' ]; then
    prepPhiEnv
fi

