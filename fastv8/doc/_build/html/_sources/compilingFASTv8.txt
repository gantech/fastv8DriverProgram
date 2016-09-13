Compiling FASTv8
----------------


FASTv8 :cite:`fastv8` is available from https://nwtc.nrel.gov/FAST8. The latest version as of this writing is v8.16.00a-bjj released by Bonnie Jonkman on 23-July-2016. This doesn't compile right away with gfortran. 

Shreyas's cmake version
+++++++++++++++++++++++

Shreyas Ananthan has adapted the FAST v8.15.00a-bjj source code to use the CMake build system. This is available at https://github.com/sayerhs/fast8_cmake and seems to work out of the box with gfortran-5.3 compiler on my mac. However, I have decided to stop using this on Mike's advice.

Michael Sprague's instructions to compile on Peregrine
++++++++++++++++++++++++++++++++++++++++++++++++++++++

Mike is aware that the version of FAST available on the NREL website doesn't compile out of the box. Here are his instructions to compile it on Peregrine.

I just compiled MAP & FAST successfully on peregrine. Here are some notes and the steps that I took. There are some steps listed that I’m sure you already took. I would say the important things are the appropriate peregrine modules, and the patches to the makefiles.

First, FYI, ../Source/dependencies/NWTC_Library/SysGnuLinux.f90 does not compile with the older gcc compilers. I will look further into that. 

Second, load the following peregrine modules:

.. code-block:: bash

   module purge
   module load gcc/4.8.1 lapack/3.4.2/gcc mkl
 
Third, thanks to Avi for the “LAPACK_LINK"

To Compile MAP:

(1) Get latest version of map

.. code-block:: bash

   git clone https://bitbucket.org/mmasciola/map-plus-plus

Let ${MAP_DIR} be the full path of map-plus-plus

(2) Apply the following changes (shown below in patchfile) to makefile in ${MAP_DIR}/src

.. code-block:: bash

   login4 >> cat patchfile.makefile
   30a31,32
   >  LAPACK_LIBS = -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm
   >
   41c43
   <   LDFLAGS   = $(BITS) -g -shared -llapacke
   ---
   >   LDFLAGS   = $(BITS) -g -shared ${LAPACK_LIBS}
   76c78
   < $(CC_TOOLS) $(LDFLAGS) -o libmap-1.20.00.so $(DEBUG) $(OBJ) -lm -llapacke
   ---
   > $(CC_TOOLS) $(LDFLAGS) -o libmap-1.20.00.so $(DEBUG) $(OBJ) ${LAPACK_LIBS}


(3) Apply the following changes (shown below in patchfile) to ${MAP_DIR}/src/lineroutines.c

.. code-block:: bash

   login4 >> cat patchfile.lineroutines.c
   28c28
   < #  include "lapack\lapacke.h"
   ---
   > #  include "lapack/lapacke.h"

(4) “make” it

.. code-block:: bash

   make

NOTE: Seems to pass the python test

To Compile FAST:

(1) Make directory where you’re going to untar fast, call it ${FAST_LOC}  (FAST_DIR is defined differently in makefile)

(2) cd ${FAST_LOC}

(3) wget https://nwtc.nrel.gov/system/files/FAST_v8.12.00a-bjj.tar.gz

(4) tar -xvf FAST_v8.12.00a-bjj.tar.gz

(5) mkdir ${FAST_LOC}/bin/

(6) cp ${MAP_DIR}/src/libmap-1.20.00.so ${FAST_LOC}/bin/  

(7) cd ${FAST_LOC}/Compiling

(8) make -f makefile_DISCON_DLL

(9) Apply following changes (shown below in patchfile) to makefile:

.. code-block:: bash

   login3 >> cat patchfile.makefile
   15,16c15,16
   < BITS = 32
   < #BITS = 64
   ---
   > #BITS = 32
   > BITS = 64
   105c105,106
   <    MAP_lib      = $(BIN_DIR)/libmap-1.10.01.so
   ---
   >    #MAP_lib      = $(BIN_DIR)/libmap-1.10.01.so
   >    MAP_lib      = $(BIN_DIR)/libmap-1.20.00.so
   117d117
   <       LAPACK_LINK  = -llapack -lblas
   118a119,120
   >       LAPACK_LINK = -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -Wl,-rpath,/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/lib/intel64 -L/nopt/intel/13.0/composer_xe_2013.3.163/mkl/../compiler/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm
   >

(10) “make it”

.. code-block:: bash

   make


This seems to work great with the native executable version of FAST in fortran, i.e. `FAST_Prog.f90`. However, this doesn't seem to compile the c file `FAST_Prog.c`. Manually compiling the c file and linking it to the FAST library simply gives a fatal error.

Avi's version
+++++++++++++

Avi Purkayastha was gracious enough to share his latest version of FAST v8 on Peregrine. The `C` driver program works in his version. After extensive digging, I found that he used the compilation flag `DSTATIC_LOAD_DLL`. Bonnie Jonkman had mentioned in her ChangeLog that this option was necessary to get FAST to work with MPI.



