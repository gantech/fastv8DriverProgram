#ifndef FAST_cInterface_h
#define FAST_cInterface_h

#include "FAST_Library.h"
#include "sys/stat.h"
#include "stdio.h"
#include <string>
#include <cstring>
#include <math.h>
#include <stdlib.h> 
#include <malloc.h>
#include <stdexcept>

class FAST_cInterface {

 private:

  bool   restart;
  double dtFAST;
  double TMax;
  float TurbinePos[3];
  int TurbID;
  char FASTInputFileName[INTERFACE_STRING_LENGTH];
  char CheckpointFileRoot[INTERFACE_STRING_LENGTH];
  double tStart, tEnd;
  int nt_global;           
  int ntStart, ntEnd;      // The time step to start and end the FAST simulation
  int nEveryCheckPoint;    // Check point files will be written every 'nEveryCheckPoint' time steps
  int numBlades;           // Number of blades
  int numElementsPerBlade;
  int numTwrElements;
  int numScOutputs;  // # outputs from the supercontroller == # inputs to the controller == NumSC2Ctrl
  int numScInputs;   // # inputs to the supercontroller == # outputs from the controller == NumCtrl2SC
  

  OpFM_InputType_t * cDriver_Input_from_FAST;
  OpFM_OutputType_t * cDriver_Output_to_FAST;
  
  int ErrStat;
  char ErrMsg[INTERFACE_STRING_LENGTH];  // make sure this is the same size as IntfStrLen in FAST_Library.f90

 public:
  
  // Constructor 
  FAST_cInterface() ;
  
  // Destructor
  ~FAST_cInterface() {
    
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

  int readInputFile(std::string cInterfaceInputFile);  
  int init();
  int step();
  int get_ntStart() { return ntStart; }
  int get_ntEnd() { return ntEnd; }

 private:
  void checkError(const int ErrStat, const char * ErrMsg);
  void setOutputsToFAST(OpFM_InputType_t* cDriver_Input_from_FAST, OpFM_OutputType_t* cDriver_Output_to_FAST) ;

  int cDriverRestart();
  inline bool checkFileExists(const std::string& name);
  
};

#endif
