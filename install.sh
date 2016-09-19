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
    sed -e "s/FASTDIR/$(sed 's:/:\\/:g' /tmp/fastDir)/" sourceMods.sh > /tmp/updatedFile
    cp /tmp/updatedFile sourceMods.sh
    source sourceMods.sh
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
#    make clean &> /dev/null
    make &> log.make
    passFail $?
}

compileMapPlusPlus() {
#map-plus-plus
    echo -n "Compiling map-plus-plus"
    cd ../map-plus-plus/src/
#    make clean &> /dev/null
    make &> log.make
    passFail $?
}

compileYAMLcpp() {
#yaml-cpp
    echo "Compiling yaml-cpp"
    echo -n "   Setting up build directory"
    cd ../../yaml-cpp
    [ -d build ] || mkdir build
    cd build
    passFail $?
    echo -n "   Configuring"
    cmake ../yaml-cpp/ -DCMAKE_INSTALL_PREFIX=../../../../ &> log.cmake
    passFail $? 
    echo -n "   Compiling"
    make &> log.make
    passFail $?
    echo -n "   Installing"
    make install &> log.makeInstall
    passFail $?
}

compileFAST() {
#FAST 
    cd ../../../../Compiling
    echo "Compiling FAST"
    echo -n "   Compiling FAST Library"
#    make clean &> /dev/null
    make &> log.make #Basic FAST Library
    passFail $?
    echo -n "   Compiling Bladed style controller libraries"
    make -f makefile_DISCON_DLL &> log.make_DISCON_DLL #Bladed style controller libraries 
    passFail $?
    echo -n "   Compiling Fortran driver program"
    make FAST_driver=FAST_Prog &> log.make_FAST_Prog #Fortran driver program
    passFail $?
    echo -n "   Compiling C driver program"
    make -f makefileCDriverProg &> log.makeCDriverProg #C driver program
    passFail $?
}



prepareSourceMods
compileRegistry
compileMapPlusPlus
compileYAMLcpp
compileFAST
