#ifndef FAST_cInterface_h
#define FAST_cInterface_h

#include "FAST_Library.h"
#include "sys/stat.h"
#include "math.h"
#include <iostream>
#include <string>
#include <cstring>
#include <malloc.h>
#include <stdexcept>
#include "yaml-cpp/yaml.h"

class FAST_cInterface {

 private:
  
  int nTurbines;
  bool restart;
  double dtFAST;
  double tMax;
  float ** TurbinePos;
  int * TurbID;
  char ** FASTInputFileName;
  char ** CheckpointFileRoot;
  double tStart, tEnd;
  int nt_global;           
  int ntStart, ntEnd;      // The time step to start and end the FAST simulation
  int nEveryCheckPoint;    // Check point files will be written every 'nEveryCheckPoint' time steps
  int * numBlades;           // Number of blades
  int * numElementsPerBlade;
  int * numTwrElements;
  int * numScOutputs;  // # outputs from the supercontroller == # inputs to the controller == NumSC2Ctrl
  int * numScInputs;   // # inputs to the supercontroller == # outputs from the controller == NumCtrl2SC
  
  OpFM_InputType_t ** cDriver_Input_from_FAST;
  OpFM_OutputType_t ** cDriver_Output_to_FAST;

  int ErrStat;
  char ErrMsg[INTERFACE_STRING_LENGTH];  // make sure this is the same size as IntfStrLen in FAST_Library.f90

 public: 

  // Constructor 
  FAST_cInterface() ;
  
  // Destructor
  ~FAST_cInterface() {
    
    for (int iTurb=0; iTurb < nTurbines; iTurb++) {
      FAST_End(&iTurb);
    }

    // deallocate types we allocated earlier
    for (int iTurb=0; iTurb < nTurbines; iTurb++) {
      delete[] TurbinePos[iTurb];
      delete[] FASTInputFileName[iTurb];
      delete[] CheckpointFileRoot[iTurb];
    }

    delete[] TurbinePos;
    delete[] FASTInputFileName;
    delete[] CheckpointFileRoot;
    delete[] TurbID;
    delete[] numBlades;
    delete[] numElementsPerBlade;
    delete[] numTwrElements;
    delete[] numScOutputs;
    delete[] numScInputs;
    
    for (int iTurb=0; iTurb < nTurbines; iTurb++) {
      
      if (cDriver_Input_from_FAST[iTurb] != NULL) {
	free(cDriver_Input_from_FAST[iTurb]);
	cDriver_Input_from_FAST[iTurb] = NULL;
      }
      if (cDriver_Output_to_FAST[iTurb] != NULL) {
	free(cDriver_Output_to_FAST[iTurb]);
	cDriver_Output_to_FAST[iTurb] = NULL;
      }
    }
    delete[] cDriver_Input_from_FAST;
    delete[] cDriver_Output_to_FAST;
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

  void readTurbineData(int iTurb, YAML::Node turbNode);
  void allocateInputData();

};

#endif
