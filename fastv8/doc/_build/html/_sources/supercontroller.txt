Supercontroller
---------------

The C++ glue code controls all the turbines in a given simulation through a super controller. A supercontroller cannot be a part of FAST because FAST modules are fundamentally designed to simulate individual turbines while a supercontroller is designed to act on a group of turbines. Hence the supercontroller is designed to be a part of the C++ glue code.


Workflow
++++++++

 The workflow of the supercontroller around the time advancement of turbines in FAST is shown in Figure :num:`supercontrollerconcept` and was arrived at after discussions with Paul Fleming and Jason Jonkman. The two main functions of the supercontroller ``updateStates`` and ``calcOutputs`` are based on the FAST framework :cite:`fastProgrammersHandbook` to separate time advancement of the internal states of the super controller and calculation of outputs to FAST.

.. _supercontrollerconcept:

.. figure:: images/superControllerConcept.pdf
   :align: center
   :width: 100%
   
   Flow of data when using supercontroller in C++ glue code.


A brief description of each function of the supercontroller is given below:

* ``init`` - Initializes the supercontroller
* ``updateStates(scInputs)`` - Advances the internal states by one time step based on inputs and possibly existing states.
* ``calcOutputs(scOutputs)`` - Calculates and writes the output into ``scOutputs`` based on existing states. **No** time advancement or modification of internal states.
* ``readRestart(nt_global)`` - Reads the checkpoint file corresponding to time step ``nt_global``.
* ``writeRestart(nt_global)`` - Writes the checkpoint file corresponding to time step ``nt_global``.


The supercontroller can have internal states of any kind. The suggested internal states are:

* 1D array of ``globStates[nGlobStates]``
* 2D array of ``turbineStates[nTurbines, nTurbineStates]``

The checkpoint files are written and read using the HDF5 interface :cite:`hdf5`.

Interface with glue code
++++++++++++++++++++++++

The supercontroller functions are expected to act on all the global turbines. Hence, the turbine numbering system inside the supercontroller is the global turbine number. There are two additional helper functions in the C++ glue code to transfer the information to and from all the turbines on possibly different processors.

* ``fillScInputsGlob`` - Combines input from turbines on different processors into the correct order for the supercontroller.
* ``fillScOutputsLoc`` - Copies the output from the supercontroller into the corresponding local turbines.

Interface with FAST
+++++++++++++++++++

The supercontroller or the glue code does not directly interface with the individual turbine controllers. A separate ``Supercontroller`` module is created inside FAST for each turbine whose only purpose is to exchange data with the ``ServoDyn`` module before and after the time advancement as

.. code-block:: fortran

   CALL SC_SetOutputs(Turbine(iTurb)%p_FAST, Turbine(iTurb)%SrvD%Input(1), Turbine(iTurb)%SC, ErrStat, ErrMsg)

   CALL FAST_Solution0_T(Turbine(iTurb), ErrStat, ErrMsg )

   CALL SC_SetInputs(Turbine(iTurb)%p_FAST, Turbine(iTurb)%SrvD%y, Turbine(iTurb)%SC, Errstat, ErrMsg)


and


.. code-block:: fortran

   CALL SC_SetOutputs(Turbine(iTurb)%p_FAST, Turbine(iTurb)%SrvD%Input(1), Turbine(iTurb)%SC, ErrStat, ErrMsg)

   CALL FAST_Solution_T( t_initial, n_t_global, Turbine(iTurb), ErrStat, ErrMsg ) 

   CALL SC_SetInputs(Turbine(iTurb)%p_FAST, Turbine(iTurb)%SrvD%y, Turbine(iTurb)%SC, ErrStat, ErrMsg)


Thus, the supercontroller object in the C++ glue code interfaces only exchanges data with the supercontroller module for each turbine.


Software details
++++++++++++++++

The supercontroller is designed to be a C++ class that is loaded dynamically following instructions from `http://tldp.org/HOWTO/C++-dlopen/ <http://tldp.org/HOWTO/C++-dlopen/>`_. This will allow the supercontroller to be compiled separately as well as distributed without the source code. The C++ glue code will however need to use a header file ``SC.h`` that describes the interface to the supercontroller functions.


Test06
++++++

Test06 changes the minimum pitch setting of two turbines as a function of time as:

* ``Turbine0``
  
  *  0-20s: 0 degrees
  *  20-40s: 1.5 degrees
  *  40-60s: 3 degrees

* ``Turbine1``

  *  0-20s: 0.5 degrees
  *  20-40s: 1 degrees
  *  40-60s: 2.5 degrees


The correct answer is computed by running native FAST with modified individual turbine controllers. Test06 passes in commit `df6b1e7fc9d74 <https://github.com/gantech/fastv8DriverProgram/commit/df6b1e7fc9d7429b33c48f7157ca9f8a690e5b0a>`_.
