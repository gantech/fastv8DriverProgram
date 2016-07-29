gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/lmder.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/dpmpar.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/lmpar.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/qrsolv.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/enorm.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/enorm_u.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK cminpack/qrfac.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK simclist/simclist.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK bstring/bstrlib.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK bstring/bstraux.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK freedata.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK mapinit.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK maperror.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK lineroutines.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK numeric.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK outputstream.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK mapapi.c
g++    -c -o lmroutines.o lmroutines.cc
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK jacobian.c
gcc -c -m64 -g -fPIC -std=c99 -D DEBUG -DGITVERSION=\"3111-dirty\" -D WITH_LAPACK residual.c
gcc -m64 -g -shared -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -o libmap-1.20.10.so  lmder.o dpmpar.o lmpar.o qrsolv.o enorm.o enorm_u.o qrfac.o simclist.o bstrlib.o bstraux.o freedata.o mapinit.o maperror.o lineroutines.o numeric.o outputstream.o mapapi.o lmroutines.o jacobian.o residual.o  -lm -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm 
