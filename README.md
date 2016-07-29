# Driver Program for FAST v8

 I plan to use this repository to build a lightweight driver program for FASTv8 in C++. I will build on FAST_Prog.c developed by Bonnie Jonkman at NREL and various other programs shown below.

* MAP library from https://bitbucket.org/mmasciola/map-plus-plus.git
* FASTv8 from https://nwtc.nrel.gov/system/files/FAST_v8.16.00a-bjj.tar.gz
* Crunch from https://nwtc.nrel.gov/system/files/Crunch_v3.02.00c-mlb.tar.gz
* NWTC_Library from https://nwtc.nrel.gov/system/files/NWTC_Lib_v1.07.02a-mlb.tar.gz

 This is configured to run only on Linux systems with the gnu compiler for now. When this project is complete, it will be an example to couple with any simulation package in C++ with FASTv8.

To compile this on NREL Peregrine, do

```bash
git clone https://github.com/gantech/fastv8DriverProgram.git
cd fastv8DriverProgram
mkdir fastv8/bin
cd fastv8/map-plus-plus/src
make &> log.make
mkdir 
cp libmap-1.20.10.so ../../bin/
cd ../../Compiling
make -f makefile_DISCON_DLL &> log.makeDISCON_DLL
make &> log.make
make -f makefileCDriverProg &> log.makeCDriverProg
```

