#ifndef FAST_cInterface_h
#define FAST_cInterface_h

#include "FAST_Library.h"
#include "stdio.h"
#include <string.h>
#include <math.h>
#include <stdlib.h> 
#include <malloc.h>

class FAST_cInterface {

 private:
  
  double dtFAST;
  double TMax;
  float TurbinePos[3];
  int TurbID;
  char FastInputFileName[INTERFACE_STRING_LENGTH];
  char CheckpointFileRoot[INTERFACE_STRING_LENGTH];
  double tStart, tEnd;
  double ntStart, ntEnd; // The time step to start and end the FAST simulation
  int nCheckPoint;
  int NumScOutputs;  // # outputs from the supercontroller == # inputs to the controller == NumSC2Ctrl
  int NumScInputs;   // # inputs to the supercontroller == # outputs from the controller == NumCtrl2SC

  OpFM_InputType_t* cDriver_Input_from_FAST = NULL;
  OpFM_OutputType_t* cDriver_Output_to_FAST = NULL;
  

 public:
  
  // Constructor
  FAST_cInterface(string fastcInterfaceInputFile);
  
  // Destructor
  virtual ~FAST_cInterface() {
    
    FAST_End();

    // deallocate types we allocated earlier
    if (cDriver_Input_from_FAST != NULL) {
      free(cDriver_Input_from_FAST);
      cDriver_Input_from_FAST = NULL;
    }
    if (cDriver_Output_to_FAST != NULL) {
      free(cDriver_Output_to_FAST);
      cDriver_Output_to_FAST = NULL;
    }

  }

  int cDriverStep();
  int get_ntStart() { return ntStart };
  int get_ntEnd() { return ntEnd };

 private:

  int cDriverReadInputFile();
  int cDriverInit();
  int cDriverRestart();
  
  
}

#endif
