Running multiple turbines on multiple cores
-------------------------------------------

The ``C++`` glue code for FAST is designed to run all the following example cases:

* one turbine on one processor,
* multiple turbines on one processor,
* 50 turbines on 5 processors,
* 5 turbines on 5 processors in a larger code running on 100,000 processors.

We use the MPI :cite:`MPI-3.1` standard for parallelization. All the ``MPI`` commands and related stuff are enclosed in ``#ifdef HAVE_MPI`` to allow the glue-code to run in both serial and parallel modes.


How to place each turbine on a specific processor? - Test04
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

In my current design, the input file dictates the processor number for each turbine. If the user doesn't want to select this, he/she would give it a value of :math:`-1` and the driver program should automatically assign each turbine to a processor. After reading the input file, each processor creates ``std::map turbineMapGlobToProc`` and ``std::map turbineMapProcToGlob``. ``std::set turbineSetProcs``, a set of processors containing atleast one turbine is used to create ``MPI_Group fastMPIGroup`` and ``MPI_Comm fastMPIComm`` so that all the turbines could interact amongst themselves or with a supercontroller in the future. The number of turbines ``nTurbinesProc`` will be different on each processor and could be zero. `Test04 <https://github.com/gantech/fastv8DriverProgram/commit/008cf737879fb47f0233d4b567e5737f841bf9c1>`_ is designed to use the ``dryRun`` :sidenote:`<dryRun> Use this option  to enable runtime checking of the assignment of turbines to processors and the corresponding memory allocation/deallocation without running FAST for any turbine` option to explicitly test the assignment of turbines to processors as shown in Fig. :num:`multiturbine-multicore-assignment-test04`.


.. _multiturbine-multicore-assignment-test04:

.. figure:: images/multiTurbineMultiCoreAssignmentTest04.pdf
   :align: center
   :width: 100%
   
   Assignment of turbines to processors/cores in Test04


Test05
++++++

Test05 is designed to run 3 turbines on 4 processors from t=0-60s with 3 restarts in the middle. The turbines are distributed amongst the processors as:

* Proc 0: Turbine0 and Turbine1
* Proc 1: no turbines
* Proc 2: Turbine 2
* Proc 3: no turbines


``Turbine0`` and ``Turbine1`` are identical to the turbines used in Test04, while ``Turbine2`` is identical to Turbine0. The details of the restart are the same as in Tests 03 and 04. Test05 passes in commit `51de163bb480021 <https://github.com/gantech/fastv8DriverProgram/commit/51de163bb48002109d6fc341002c0a4e15cf9c90>`_.