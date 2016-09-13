Development Process
-------------------

I like the idea of test driven development, where the tests for the final version and the units are written before the development of the actual source code itself. I learnt that there are two kinds of tests in software development:

1. Unit tests: These tests are written to verify that each unit (module, function etc.) of the program functions as expected. 
2. Regression tests: These tests verify that the old features of the program function as expected after changes are made to the code base. These changes may include software enhancements, patch fixes, upgrading to newer versions of external libraries, using new compilers etc.

So unit tests are more important during the development process and regression tests are more important after the first release. I propose that we use the same testing framework as in NALU :cite:`naluDoc`.
 
Considering the requirements for this project, the high level test for the new driver program for FAST should be

1. Run two different turbines with a prescribed velocity at each of the actuator points for a prescribed time, stop and dump state. 
2. Restart from the previous state and continue running for a precribed time and dump state again.
3. Repeat step 2 for a prescribed number of times.
4. Concatenate all output and compare against a similar native run of FAST v8.
5. Declare test as "passed" if there are no differences between the multi-part new driver simulation and the native run of FAST v8.

When running the native version of FAST v8, I have to make sure that it's run with WakeMod=0 in the AeroDyn module. The induction has to be ignored to compare against the run using the new driver. 

