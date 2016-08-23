#include "FAST_cInterface.h"


//Constructor
FAST_cInterface::FAST_cInterface():
cDriver_Input_from_FAST(NULL),
cDriver_Output_to_FAST(NULL),
numScInputs(0),
numScOutputs(0)
{
  
}

inline bool FAST_cInterface::checkFileExists(const std::string& name) {
  struct stat buffer;   
  return (stat (name.c_str(), &buffer) == 0); 
}

int FAST_cInterface::init() {

  // Allocate memory for Turbine datastructure for all turbines
   FAST_AllocateTurbines(&nTurbines, &ErrStat, ErrMsg);

  // Allocate memory for OpFM Input types in FAST

   cDriver_Input_from_FAST = new OpFM_InputType_t* [nTurbines] ;
   cDriver_Output_to_FAST = new OpFM_OutputType_t* [nTurbines] ;
   for (int iTurb=0; iTurb < nTurbines; iTurb++) {
     cDriver_Input_from_FAST[iTurb] = malloc(sizeof(OpFM_InputType_t));
     cDriver_Output_to_FAST[iTurb] = malloc(sizeof(OpFM_OutputType_t));
     if (cDriver_Input_from_FAST[iTurb] == NULL || cDriver_Output_to_FAST[iTurb] == NULL) {
       throw std::runtime_error("Error allocating space for OpFM interface types.\n") ;
     }
   }
  
   // If restart 
   if (restart == true) {

      /* note that this will set nt_global inside the FAST library */
      FAST_OpFM_Restart(&iTurbTmp, CheckpointFileRoot, &AbortErrLev, &dtFAST, &numBlades, &numElementsPerBlade, &ntStart, cDriver_Input_from_FAST[iTurbTmp], cDriver_Output_to_FAST[iTurbTmp], &ErrStat, ErrMsg);
      checkError(ErrStat, ErrMsg);
      nt_global = ntStart;
      ntEnd = int((tEnd - tStart)/dtFAST) + ntStart;

   } else {
     
      // this calls the Init() routines of each module

      FAST_OpFM_Init(&iTurbTmp, &tMax, FASTInputFileName, &TurbID, &numScOutputs, &numScInputs, TurbinePos, &AbortErrLev, &dtFAST, &numBlades, &numElementsPerBlade, cDriver_Input_from_FAST[iTurbTmp], cDriver_Output_to_FAST[iTurbTmp], &ErrStat, ErrMsg);
      checkError(ErrStat, ErrMsg);

      numTwrElements = cDriver_Output_to_FAST[iTurbTmp]->u_Len - numBlades*numElementsPerBlade - 1;

      // set wind speeds at initial locations
      //      setOutputsToFAST(cDriver_Input_from_FAST[iTurbTmp], cDriver_Output_to_FAST[iTurbTmp]);

      FAST_OpFM_Solution0(&iTurbTmp, &ErrStat, ErrMsg);
      checkError(ErrStat, ErrMsg);
     
   }
   
   return 0;

}

int FAST_cInterface::step() {

  if ( ((nt_global - ntStart) % nEveryCheckPoint) == 0 ) {
    //sprintf(CheckpointFileRoot, "../../CertTest/Test18.%d", nt_global);
    sprintf(CheckpointFileRoot, " "); // if blank, it will use FAST convention <RootName>.nt_global
    FAST_CreateCheckpoint(&iTurbTmp, CheckpointFileRoot, &ErrStat, ErrMsg);
    checkError(ErrStat, ErrMsg);
  }
  /* ******************************
     set inputs from this code and call FAST:
  ********************************* */

  //  set wind speeds at original locations 
  //  setOutputsToFAST(cDriver_Input_from_FAST[iTurbTmp], cDriver_Output_to_FAST[iTurbTmp]);


  // this advances the states, calls CalcOutput, and solves for next inputs. Predictor-corrector loop is imbeded here:
  // (note OpenFOAM could do subcycling around this step)
  FAST_OpFM_Step(&iTurbTmp, &ErrStat, ErrMsg);
  checkError(ErrStat, ErrMsg);

  nt_global = nt_global + 1;
  
  return 0;
}

int FAST_cInterface::readInputFile(std::string cInterfaceInputFile ) {

  // Check if the input file exists and call init
  if ( checkFileExists(cInterfaceInputFile) ) {

    YAML::Node cDriverInp = YAML::LoadFile(cInterfaceInputFile);
    
    nTurbines = cDriverInp["nTurbines"].as<int>();
    TurbID = cDriverInp["TurbID"].as<int>();
    std::cout << cDriverInp["FASTInputFileName"].as<std::string>() << std::endl ;
    strcpy(FASTInputFileName, cDriverInp["FASTInputFileName"].as<std::string>().c_str() );
    strcpy(CheckpointFileRoot, cDriverInp["restartFileName"].as<std::string>().c_str() );
    restart = cDriverInp["restart"].as<bool>();
    tStart = cDriverInp["tStart"].as<double>();
    tEnd = cDriverInp["tEnd"].as<double>();
    tMax = cDriverInp["tMax"].as<double>();
    nEveryCheckPoint = cDriverInp["nEveryCheckPoint"].as<int>();
    if (restart == false) {
      ntStart = 0;
      nt_global = ntStart;
      dtFAST = cDriverInp["dtFAST"].as<double>();
      ntEnd = int((tEnd - tStart)/dtFAST);
      if (cDriverInp["TurbinePos"].IsSequence() ) {
	std::vector<double> tp = cDriverInp["TurbinePos"].as<std::vector<double> >() ;
	for(int i=0;i<3;i++) {
	  TurbinePos[i] = tp[i];
	}
      }
    }
    numScInputs = cDriverInp["numScInputs"].as<int>();
    numScInputs = cDriverInp["numScOutputs"].as<int>();

    std::cout << "tStart = " << tStart << std::endl ;
    std::cout << "tEnd = " << tEnd << std::endl ;
    std::cout << "TubID = " << TurbID << std::endl ;
    std::cout << "nEverycheckpoint = " << nEveryCheckPoint << std::endl ;
    std::cout << "TurbinePos = " << TurbinePos[0] << " " << TurbinePos[1] <<  " " << TurbinePos[2] << std::endl ;

    init();

  } else {
    throw std::runtime_error("Input file " + cInterfaceInputFile + " does not exist or I cannot access it");
  }

}


void FAST_cInterface::checkError(const int ErrStat, const char * ErrMsg){

   if (ErrStat != ErrID_None){

      if (ErrStat >= AbortErrLev){
         FAST_End();
	 throw std::runtime_error(ErrMsg);
      }

   }

}

void FAST_cInterface::setOutputsToFAST(OpFM_InputType_t* cDriver_Input_from_FAST, OpFM_OutputType_t* cDriver_Output_to_FAST){
   int j;

   // routine sets the u-v-w wind speeds used in FAST and the SuperController inputs

   for (j = 0; j < cDriver_Output_to_FAST->u_Len; j++){
      cDriver_Output_to_FAST->u[j] = (float) 10.0*pow((cDriver_Input_from_FAST->pz[j] / 90.0), 0.2); // 0.2 power law wind profile using reference 10 m/s at 90 meters
      cDriver_Output_to_FAST->v[j] = 0.0;
      cDriver_Output_to_FAST->w[j] = 0.0;
   }

   // call supercontroller

   for (j = 0; j < cDriver_Output_to_FAST->SuperController_Len; j++){
      cDriver_Output_to_FAST->SuperController[j] = (float) j; // set it somehow.... (would be set from the SuperController outputs)
   }


   return;
}
