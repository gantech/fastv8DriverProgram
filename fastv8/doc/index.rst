FASTv8 - C++ Glue Code
======================

The aim of this project is to develop an light weight driver program for FAST v8 :cite:`fastv8ModFramework` :sidenote:`<Jon13> Jason M. Jonkman. The New Modularization Framework for the FAST Wind Turbine CAE Tool. Technical Report NREL/CP-5000-57228, National Renewable Energy Laboratory, January 2013.` in C++. This program will serve as an example for people who want to interface FAST v8 with their own simulation framework. The broad requirements of the light weight driver program for FAST v8 are:

1. the program should be very similar to how it would be used in NALU :cite:`naluDoc` :sidenote:`<Dom15> Stefan Domino. Sierra Low Mach Module: Nalu Theory Manual 1.0. Technical Report SAND2015-3107W, Sandia National Laboratories Unclassified Unlimited Release (UUR), https://github.com/spdomin/NaluDoc, 2015.`.
2. the program should be capable of having a time step that is independent of FAST's time step.
3. the program should have an option to turn off the structural modes in FAST.
4. the program should be capable of instantiating multiple turbines inside FAST on the same processor core.
5. the program should have a restart capability.
6. the design of the program should allow a super controller to be built around all the turbines.


.. toctree::
   :numbered:
   :maxdepth: 2

   requirements
   workDoneSoFar.rst
   developmentProcess.rst
   compilingFASTv8.rst
   currentWork.rst
   multiTurbineOneCore.rst
   multiTurbineMultiCore.rst
   supercontroller.rst
   zrefs.rst


