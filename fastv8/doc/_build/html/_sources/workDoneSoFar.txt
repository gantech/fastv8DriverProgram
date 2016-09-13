Work done so far
----------------
Bonnie Jonkman has written an example program in C to call FASTv8 in/from SOWFA. Here are some notes from email exchanges between Mike and Bonnie Jonkman. There seems to be an (svn?) repository somewhere. I need to figure this out.

FAST_Prog.c
+++++++++++

I wrote that as an example for Avi and Sang on how to call FAST8 in SOWFA.  There is no documentation other than what is in the source code.

Funding for the project ran out at the end of the end of September so we all stopped working on it.

There seems to be a memory issue with the current implementation. I think it works for one turbine but when Avi ran it (along with the super-controller) for multiple turbines, it did one of three things (1) worked normally, (2) produced a segmentation fault, or (3) stopped responding (waiting for something?). At one point they said Sang had it working for multiple turbines without the super-controller, but then they decided he hadn't run it enough times to know if that was actually true.

I think Avi is hoping he can figure out what the issue is when he works on Jason's LDRD project, FAST.Farm. It's not using OpenFOAM, but it's a similar setup with multiple turbines.

More email exchanges with Bonnie Jonkman
++++++++++++++++++++++++++++++++++++++++

Avi and Sang tell me there is a memory leak when using more than one turbine. For a while they told me it worked without the supercontroller, but later they said they didn't know if they had run it enough without the supercontroller to know if it was actually working or if they were just getting lucky. I had suggested that we modify the Fortran-C++ interface if they can't figure out the issue, but Avi wanted to do more debugging first. Since there is no funding for that task anymore, I doubt they have done much on it since October.

I have a C driver that I have tested calling the FAST-OpenFOAM interface with only one turbine and it works fine. That portion of the code is in the standard FAST 8 SVN repo.

You'll have to ask Avi or Sang about where the latest SOWFA code is. No one appears to use https://github.com/NREL/SOWFA for keeping track of branches. Avi tells me Matt Churchfield and Paul Fleming also have different versions, and Avi will only give me a tar file whenever I have asked to see code. 

Actually, I should clarify what I said about a memory leak: the word "leak" is not accurate. There is an issue with the memory management. Avi says there are times the simulation seems to hang (like getting into an infinite loop), times it gives a segmentation fault and dies, and other times it runs fine. 


Understanding FAST driver programs in FORTRAN and C
+++++++++++++++++++++++++++++++++++++++++++++++++++

The main program of the native version of FAST is FAST_Prog.f90. All the subroutines called in this program are defined in FAST_Subs.f90.

.. code-block:: fortran

   PROGRAM FAST
      USE FAST_Subs
      
      CALL NWTC_Init()
      IF (restart) THEN
         CALL FAST_RestoreFromCheckpoint_Tary(t_initial, Restart_step, Turbine, CheckpointRoot, ErrStat, ErrMsg  )
      ELSE
         Restart_step = 0
         DO i_turb = 1, NumTurbines
 	    CALL FAST_InitializeAll_T( t_initial, i_turb, Turbine(i_turb), ErrStat, ErrMsg ) 
	    CALL FAST_Solution0_T( Turbine(i_turb), ErrStat, ErrMsg )
         END DO    
      END IF

      DO n_t_global = Restart_step, Last_timestep

        IF (writeCheckPoint)
	   CALL FAST_CreateCheckpoint_Tary(t_initial, n_t_global, Turbine, CheckpointRoot, ErrStat, ErrMsg)
        END IF
	
        DO i_turb = 1,NumTurbines
           CALL FAST_Solution_T( t_initial, n_t_global, Turbine(i_turb), ErrStat, ErrMsg )
        END DO	
      
      END DO

      DO i_turb = 1,NumTurbines
        CALL ExitThisProgram_T( Turbine(i_turb), ErrID_None )
      END DO

   END PROGRAM


FAST_Prog.C is intended to be the counterpart to FAST_Prog.f90 in C. All functions are subroutines defined in FAST_Library.f90 and called from C. The conversion of Fortran datatypes to C datatypes in the function input/output is defined in FAST_Library.h. The OpFM subroutines used in FAST_Prog.C instead call the corresponding high level subroutines that are used in FAST_Prog.f90.

.. code-block:: c

   int main()
      
      
      // No equiv of CALL NWTC_Init()
      if (restart) {
         FAST_OpFM_Restart(CheckpointFileRoot, &AbortErrLev, &dt, &NumBlades, &NumElementsPerBlade, &n_t_global_start, OpFM_Input_from_FAST, OpFM_Output_to_FAST, &ErrStat, ErrMsg); }
      else {
         n_t_global_start = 0;
         FAST_OpFM_Init(&TMax, InputFileName, &TurbID, &NumScOutputs, &NumScInputs, TurbinePos, &AbortErrLev, &dt, &NumBlades, &NumElementsPerBlade, OpFM_Input_from_FAST, OpFM_Output_to_FAST, &ErrStat, ErrMsg);
         setOutputsToFAST(OpFM_Input_from_FAST, OpFM_Output_to_FAST);
	 FAST_OpFM_Solution0(&ErrStat, ErrMsg);
      }

      for(n_t_global = n_t_global_start; nt_global < Last_timestep; n_t_global++) {
        if (n_t_global == n_checkpoint) {
	   FAST_CreateCheckpoint(CheckpointFileRoot, &ErrStat, ErrMsg);
        }
	setOutputsToFAST(OpFM_Input_from_FAST, OpFM_Output_to_FAST);
	FAST_OpFM_Step(&ErrStat, ErrMsg);      
      }

      FAST_End();

      return 0;
   }

Meeting with Avi Purkayastha - 07/28/2016
+++++++++++++++++++++++++++++++++++++++++

* The driver program FAST_Prog.c used to work in the version of FAST they had last September.
* The super controller exists in the windPlantSolverFAST8 application/solvers folder. The super controller doesn't exist in the bladed style controller file within fast. FAST now just has a set of arrays and routines that pass info to the super controller and receive info back from it.
* The current fast coupling to C/C++ is still designed to have only one turbine per processor. However, the supercontroller is not on the `n+1` processor anymore. The supercontroller is desgined to operate in parallel and presumably uses global MPI commands to collect the controller info from all turbines and send signals back to them.
* The super controller would have to be written in the NALU framework for our project.
* The restart from checkpoint feature works natively in FAST, but not from OpenFOAM, atleast not consistently.
* Avi recommends using one processor per turbine because the core running multiple turbines with FAST may run out of memory.
* Digging into the code myself, I found that the current datastructure in FAST_Library.f90 doesn't support more than one turbine per core. Fortran modules are not C++ classes. The `turbine` datastructure in the module supports only one turbine. This would have to be replaced with an array of turbines to add the multiple turbine per core feature. This would need to be followed up with an extensive programming/rewriting of the data transfer routines between Fortran and C++.


