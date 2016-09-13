Requirements and Design
-----------------------

The broad requirements of the light weight driver program for FAST v8 are:

1. the program should be very similar to how it would be used in NALU :cite:`naluDoc`.
2. the program should be capable of having a time step that is independent of FAST's time step.
3. the program should have an option to turn off the structural modes in FAST.
4. the program should be capable of instantiating multiple turbines inside FAST on the same processor core.
5. the program should have a restart capability.
6. the design of the program should allow a super controller to be built around all the turbines.

Meeting with Matt - 03/08/2016
++++++++++++++++++++++++++++++

I explained the general structure of the C driver program and how the current framework supports only one turbine per instance of FAST. The current FAST-SOWFA interface framework is:

* The framework supports an arbitrary number of turbines `n`.
* The FAST instances for turbines `1` through `n` are placed in processors `1` through `n` respectively.
* Global MPI reductions/broadcasts are used to transfer velocity and actuator force between FAST and the CFD solver.

Matt and I agreed that the current framework of using global MPI communications for each turbine may not scale well to large number of processors. The proposed framework for future CFD-FAST interface is:

* The new framework will have groups of processors within `MPI_WORLD` for each turbine.
* Each group will contain the CFD domain decomposition of all cells interacting with the turbine and the FAST instance. 
* These groups of processors would communicate amongst themselves for all stuff related to that tubine.
* Ideally, the FAST instance for this turbine should be located within one of the processors that contain the CFD domain decomposition for that turbine. However, this is not a requirement. Adding 1 extra processor to this communication group shouldn't increase the total communication load. However, this might be an issue if the processor containing the FAST instance is not physically close to the other processors containing the domain decomposition within the MPI network.
* The domain decomposition/other pre-processing program for the CFD solver should be capable of identifying all processors that contain any cells that interact with the wind tubine. While this is possible in any software that we release, this would have to be a requirement for all external CFD software that want to interface with FAST in the future.
* The domain decomposition could happen in such a way that a processor interacts with two turbines that are close to each other. In this case, the framework could decide to place the FAST instance for both turbines on the same processor. This would require FAST to handle more than 1 turbine from C++. This can be avoided by requiring that the FAST instances for turbines `1` through `n` are placed in processors `1` through `n` respectively.

Meeting with Mike in lunch room - 03/08/2016
++++++++++++++++++++++++++++++++++++++++++++

Mike continues to insist that the light weight driver program for FAST in C++ should be capable of running multiple turbines in 1 processor. However, we also discussed that the light weight driver program should be capable of running in parallel. So I have to figure out a way of incorporating it into the requirements and designing a test for it.

Super Controller requirements from Paul Fleming
+++++++++++++++++++++++++++++++++++++++++++++++
Paul Fleming created a super controller :cite:`fleming2013` to control all the turbines in a wind farm inside SOWFA. The new driver program for FAST v8 should be designed such that it can be easily extended to include the super controller natively. Paul Fleming provided this rather long story about the development of the super controller using FAST v7 :cite:`fastv7` and what he envisions when using FAST v8 :cite:`fastv8ModFramework`.

So, back when SOWFA was a FOA award, one of my first tasks was to implement the super controller inside of SOWFA.  I wrote this up in the attached (admittedly not great) paper for EWEA.  But at least, Figure 3 is helpful in explaining the architecture.  I was working with the version of SOWFA in which FAST7 is coupled to openfoam.  Working with John Michalakes, Matt, Sang and Pieter we did something like this:

1. FAST can be set to call a compiled controller (usually referred to as Bladed-style since the interface is designed to match what BLADED expects and it’s convenient to use this well-known interface). You see this interface on the left of Fig 3.
2. The next thing we did was make a new function called the “superController” which implements the wind farm control.

  a. Here’s where things got too technical for me and John really did everything.
  b. We made sure that there was always one more processor then we need for Openfoam, and then the super controller is some how made to exist there.
  c. Then the individual turbine controllers call the super controller by reference to the last MPI proc, and pass along a number so the super controller knows who is calling
  d. It may seem funny the turbine controllers call the super controller rather than the other way around, but we thought about this a lot and couldn’t really see any issue with this, who calls who seems more a software question without much impact on the controls.

Ok, finally, this all works, we’ve used it now for a ton of projects, it’s great!
 
Chapter 2, In which we meet FAST8
 
Ok, so then (1, 2 years ago?) there was a push to convert the FAST7 code to FAST8, including the super controller code.  We had some initial discussions with Avi, Sang and Jason and Bonnie Jonkman, and one of the main outcomes was that the architecture shown in Fig 3 violates FAST8 conventions.   As I understand, the individual turbine controllers talking to the super controller beyond FAST’s reach does not meet FAST’s modular framework.  Therefore a new approach was agreed on, the super controller would exist basically to the left of Openfoam, openfoam would transfer the data from the super controller to each FAST instance, which would propagate the information to the individual controllers.
 
This approach had another nice benefit, FAST8 and openfoam have restart, and by having all the turbine and super controller states managed this way, could enable SOWFA + Super Controller to have restart too, which is awesome.
 
So this had been going on, Avi and Sang we’re doing the programming.  In my understanding however, this effort ran out of money before it could be completed (or maybe it’s still ongoing?)  Maybe good to check in with Avi and Sang.
 
And in the last bit of history, in the last few months, we in the controls group have begun using, in addition to SOWFA + Super controller, other versions of SOWFA.  For example, the actuator disk version.  We like this because, for example, to train FLORIS we need a lot of runs with the turbine operating in stationary but controlled way (like offset in yaw) and we can get way more data with the actuator disk against a coarse mesh grid.  Also, certain partners would like to run SOWFA, and while the “main-line” SOWFA is easy enough to install, SOWFA + Super Controller may have only been successfully installed by John Michalakes, who left NREL some years back.
 
So, I hope all this has been helpful.  If everything is on the table now, my plans for including the super controller would be:
 
* Make it an option in the main “SOWFA/NALU” distribution, not something that needs to be separately compiled
* Make it generic, able to work coupled to FAST, or AL or AD implementations
* Make sure it fits FAST8 module framework, and includes restart
* Make the individual turbine controllers, and super controller compiled separate, so they can be swapped out without recompiling SOWFA (not how we do it now, and that is too bad)


Design for testing
++++++++++++++++++

The `C` driver program should reproduce the same results as the native fortran program for the same input parameters. Since the `C` and fortran programs are compiled on the same machine, there should be no difference between the output from the two codes upto machine precision. The comparison of the output from the `C` and fortran driver programs will be done with the `diff` program. I will follow the design of the tests in NALU `https://github.com/spdomin/NaluRtest <https://github.com/spdomin/NaluRtest>`_.



Interface design
++++++++++++++++

There are a few key differences between the `C` and fortran driver programs. These differences along with the testing procedure will dictate the design of the `C` driver program.

* The fortran driver program controls the time step of FAST directly, while the `C` driver program will control the time stepping of FAST externally. Thus, the `C` driver program should be aware of the start (`tStart`) and end (`tEnd`) time points and the corresponding time steps (`ntStart` and `ntEnd`), the time step (`dtFAST`), and the frequency with which the checkpoint files have to be written (`nCheckPoint`).
* The `C` driver program may suppply an additional number of parameters to FAST during initialization. These are defined in `FAST_Types.f90` under the type `FAST_ExternInitType`

.. code-block:: fortran

   TYPE, PUBLIC :: fast_externinittype
     REAL(DbKi)  :: tmax = -1
     INTEGER(IntKi)  :: sensortype = sensortype_none      
     LOGICAL  :: lidradialvel
     INTEGER(IntKi)  :: turbineid
     REAL(ReKi) , DIMENSION(1:3)  :: turbinepos
     INTEGER(IntKi)  :: numsc2ctrl
     INTEGER(IntKi)  :: numctrl2sc
   END TYPE fast_externinittype

This suggests that the `C` driver program should have atleast the following variables:

.. code-block:: c++

   double dtFAST;
   double TMax;
   float TurbinePos[3];
   int TurbID;
   char InputFileName[INTERFACE_STRING_LENGTH]; 
   char CheckpointFileRoot[INTERFACE_STRING_LENGTH]; 
   double tStart, tEnd;
   double ntStart, ntEnd; // The time step to start and end the FAST simulation
   int nCheckPoint;
   int NumScOutputs;  // # outputs from the supercontroller == # inputs to the controller == NumSC2Ctrl
   int NumScInputs;   // # inputs to the supercontroller == # outputs from the controller == NumCtrl2SC

   


This project is really about building an interface class to FAST. The driver program is really secondary and should simply use functions from the class. No one is actually expected to use the driver program as it is. The design of the interface class to FAST should be such that the driver program only contains simple calls as follows.

.. code-block:: c++
   
   setInitialConditions() // Optional to state external initial conditions that don't belong in the input file.
   init()
   step()
   end()

Everything else should be abstracted away into either input files or the functions themselves.

Thus, the variables listed above should be the variables in the C++ class. The same variables should be read in from an input file. Since we're following NALU's :cite:`naluDoc` footsteps, I propose that the input file to the `C` driver program/class be in YAML and read using `yaml-cpp <https://github.com/jbeder/yaml-cpp>`_.