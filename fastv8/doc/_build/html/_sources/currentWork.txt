Current work - Overview
-----------------------

I got the latest version of FAST v8.16.00 released by Bonnie Jonkman on 23 July 2016. The progress of my work can be tracked at `https://github.com/gantech/fastv8DriverProgram <https://github.com/gantech/fastv8DriverProgram>`_. 

Fix existing code
+++++++++++++++++

FAST doesn't compile out of the box on linux systems based on gfortran. I applied the same techniques as Avi Purkayastha to compile it. The key here is the use of ``-DSTATIC_DLL_LOAD`` :sidenote:`<staticDLLload> This is used within the BladedInterface module. When this flag is enabled, the module calls the subroutine in the external bladed style library directly instead of calling the external library through a dynamic load call in the middle of the program at run time.`  directive in the makefile. Both the native ``fortran`` and ``C`` driver programs now run from scratch, i.e. t=0s. See commit `3f7c967bf661c22 <https://github.com/gantech/fastv8DriverProgram/commit/3f7c967bf661c22b78e1481ff67aaea9cb5d72de>`_.

The example C driver program in FAST v8.16.00 had two major problems regarding restart. First, the number of arguments in the C and Fortran definition of ``FAST_OpFM_Restart`` were different. Second, the name of the checkpoint file passed to the restart routine was getting mangled up. I fixed both of these issues and the restart works ok in commit `17ee6f027a0357 <https://github.com/gantech/fastv8DriverProgram/commit/17ee6f027a0357b55c0bd2f2eeebfb36f044594c>`_ .

The FAST library had some issues with unpacking the binary data from the ``checkpoint`` file. Greg Hayman helped fix these issues using Visual Studio on windows. The same fixes work in Linux as well. The version in commit `5c41b918b050f0 <https://github.com/gantech/fastv8DriverProgram/commit/5c41b918b050f04db1299724ae3565565d73b9c7>`_ restarts correctly with the native fortran version and has been tested with certification tests Test01 and Test18.

Understanding time stepping in FAST
+++++++++++++++++++++++++++++++++++

.. code-block:: fortran

  DO n_t_global=n_t_global_start, ntMax   ! Starts from n_t_global=0 in the first run
   
     IF (writeCheckpoint) THEN
        writeCheckPoint        !The restart file is written at the start of the time step.  
     END IF

     FAST_Step
 
     ! n_t_global still hasn't changed here. Changes only on the next time step
     
  END DO


Consider an example where FAST is run for 1600 time steps from :math:`t=0`. Thus, :math:`ntMax=1599`. Let us say that I asked FAST to write a restart file every 1600 time steps as well. Since the checkpoint file is written at the start of the time step, FAST will not write a checkpoint file at the end of the simulation. To get FAST to do this, I will have to run FAST for one time step more than the desired number of time steps. The C driver for FAST will do this.

C++ interface class for FAST
++++++++++++++++++++++++++++

The main aim of building a C++ interface class for FAST is to keep the driver program very simple as follows:

.. code-block:: c++

    #include "FAST_cInterface.h"
    #include <iostream>
    #include <mpi.h>
    
    int main() {
      int iErr;
      int nProcs;
      int rank;
    
      iErr = MPI_Init(NULL, NULL);
      iErr = MPI_Comm_size( MPI_COMM_WORLD, &nProcs);
      iErr = MPI_Comm_rank( MPI_COMM_WORLD, &rank);
    
      std::string cDriverInputFile="cDriver.i";
      FAST_cInterface FAST;
      try {
        FAST.readInputFile(cDriverInputFile);
      }
      catch( const std::runtime_error & ex) {
        std::cerr << ex.what() << std::endl ;
        std::cerr << "Program quitting now" << std::endl ;
        return 1;
      }
    
      if( !FAST.isDryRun() ) {
        for (int nt = FAST.get_ntStart(); nt <= FAST.get_ntEnd(); nt++) {
          FAST.step();
        }
      }
    
      FAST.end() ;
      MPI_Finalize() ;
    
      return 0;
        
    }


The C++ interface class will read a YAML style input file like the one below.

.. code-block:: yaml

    nTurbinesGlob: 3
    dryRun:  False
    restart: False
    tStart:  0.0
    tEnd:    10.0
    tMax:    60.0
    ntStart: 0
    ntEnd:   1600
    dtFAST:  0.00625
    nEveryCheckPoint: 1600
    
    Turbine0:
      procNo: 0
      TurbinePos: [ 0.0, 0.0, 0.0 ]
      numScOutputs: 0
      numScInputs: 0
      restartFileName: "banana"
      FASTInputFileName: "t1_Test05.fst"
      TurbID:  1
    
    Turbine1:
      procNo: 0
      TurbinePos: [ 0.0, 0.0, 0.0 ]
      numScOutputs: 0
      numScInputs: 0
      restartFileName: "banana"
      FASTInputFileName: "t2_Test05.fst"
      TurbID:  2
    
    Turbine2:
      procNo: 2
      TurbinePos: [ 0.0, 0.0, 0.0 ]
      numScOutputs: 0
      numScInputs: 0
      restartFileName: "banana"
      FASTInputFileName: "t3_Test05.fst"
      TurbID:  3


Test framework for the C driver program
+++++++++++++++++++++++++++++++++++++++

Following the philosophy of test driven development, the test framework for the C driver program is being built in the ``cDriverProgramTests`` on github. This will mimic the regression testing framework of NALU :cite:`naluDoc` from `https://github.com/spdomin/NaluRtest <https://github.com/spdomin/NaluRtest>`_. The basic idea behind all tests is to compare the output of the C driver program to that of the native fortran program. The current test framework (upto commit `51de163b <https://github.com/gantech/fastv8DriverProgram/commits/51de163bb48002109d6fc341002c0a4e15cf9c90>`_ has the following tests that pass:

* Test01 - The same as CertTest/Test18 in FAST. Run from :math:`t=0s` to :math:`t=60s` on one core and compare output to that from native FAST.
* Test02 - The same as Test01, except is done in 4 parts with restart:

    * Run 1 - :math:`t=0s` to :math:`t=10s`,
    * Run 2 - :math:`t=10s` to :math:`t=30s`,
    * Run 3 - :math:`t=30s` to :math:`t=50s`,
    * Run 4 - :math:`t=50s` to :math:`t=60s`.
* Test03 - Run two turbines with different controllers on one core. The rest of the details are the same as Test02.
* Test04 - Dry run to check allocation of turbines to specific cores as specified in the input file. This will only allocate memory for the turbine datastructure and associated input parameters and deallocate them on exit.
* Test05 - Run three turbines on four processors with two turbines on the first processor, one turbine on the third processor and no turbines in the other processors. The three turbines have different bladed style controllers. The rest of the details are the same as Test02.


Running multiple turbines in one core
+++++++++++++++++++++++++++++++++++++

The native version of FAST is capable of handling several different turbines when running on one core. However, this capability breaks down when FAST is used with an external library like a externally defined bladed-style controller for ServoDyn or user defined routines in ServoDyn and ElastoDyn. All variables used in subroutines of externally defined libraries become global variables by default. This leads to a conflict when using the same external library for multiple turbines. In a private discussion, Bonnie suggested that FAST could work with multiple turbines if the external libraries used for each turbine had different names as shown in Figure :num:`multiturbinediscon`.

.. _multiturbinediscon:

.. figure:: images/multipleTurbineFAST_08_18_2016/multiTurbineDISCON.png
   :align: center
   :width: 80%

   How to run multiple turbines on a single core in FAST with external bladed-style controller libraries. 

See `Running multiple turbines in one core <multiTurbineOneCore.html>`_ for more details.

Multiple turbines on multiple cores
+++++++++++++++++++++++++++++++++++

The ``C++`` glue code for FAST is designed to run all the following example cases:

* one turbine on one processor,
* multiple turbines on one processor,
* 50 turbines on 5 processors,
* 5 turbines on 5 processors in a larger code running on 100,000 processors.

We use the MPI :cite:`MPI-3.1` standard for parallelization. All the ``MPI`` commands and related stuff are enclosed in ``#ifdef HAVE_MPI`` to allow the glue-code to run in both serial and parallel modes. See `Running multiple turbines on multiple cores with C driver <multiTurbineMultiCore.html>`_ for more details.


Supercontroller
+++++++++++++++

The C++ glue code should be capable of controlling all the turbines in a given simulation through a super controller. Test06, designed to develop this capabilty, changes the minimum pitch setting of two turbines as a function of time as:

* ``Turbine0``
  
  #  0-20s: 0 degrees
  #  20-40s: 1.5 degrees
  #  40-60s: 3 degrees

* ``Turbine1``

  #  0-20s: 0.5 degrees
  #  20-40s: 1 degrees
  #  40-60s: 2.5 degrees



See the `supercontroller <supercontroller.html>`_ page for more details.

Next steps
++++++++++

1. Test the capability for a simple super controller to set the minimum pitch angle on different turbines as a function of time.
2. Test the capability to specify the velocity at the actuator points.
3. Test the capability to turn off the structural properties of the turbine using an input parameter.
4. Upgrade the testing framework to be fully automated based on ``git clone`` including the compilation steps.
5. Test if the different bladed style controller files for the turbines can be simlinks of the same file.