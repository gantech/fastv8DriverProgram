#include "FAST_cInterface.h"


//Constructor
FAST_cInterface::FAST_cInterface():
cDriver_Input_from_FAST(NULL),
cDriver_Output_to_FAST(NULL)
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

     for (int iTurb=0; iTurb < nTurbines; iTurb++) {
       /* note that this will set nt_global inside the FAST library */
       FAST_OpFM_Restart(&iTurb, CheckpointFileRoot[iTurb], &AbortErrLev, &dtFAST, &numBlades[iTurb], &numElementsPerBlade[iTurb], &ntStart, cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb], &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);
       nt_global = ntStart;
       ntEnd = int((tEnd - tStart)/dtFAST) + ntStart;
     }

   } else {
     
      // this calls the Init() routines of each module

     for (int iTurb=0; iTurb < nTurbines; iTurb++) {
       FAST_OpFM_Init(&iTurb, &tMax, FASTInputFileName[iTurb], &TurbID[iTurb], &numScOutputs[iTurb], &numScInputs[iTurb], TurbinePos[iTurb], &AbortErrLev, &dtFAST, &numBlades[iTurb], &numElementsPerBlade[iTurb], cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb], &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);
       
       numTwrElements[iTurb] = cDriver_Output_to_FAST[iTurb]->u_Len - numBlades[iTurb]*numElementsPerBlade[iTurb] - 1;

       // set wind speeds at initial locations
       //      setOutputsToFAST(cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb]);

       FAST_OpFM_Solution0(&iTurb, &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);
     }

   }
   
   return 0;

}

int FAST_cInterface::step() {

  if ( ((nt_global - ntStart) % nEveryCheckPoint) == 0 ) {
    //sprintf(CheckpointFileRoot, "../../CertTest/Test18.%d", nt_global);
    for (int iTurb=0; iTurb < nTurbines; iTurb++) {
      sprintf(CheckpointFileRoot[iTurb], " "); // if blank, it will use FAST convention <RootName>.nt_global
      FAST_CreateCheckpoint(&iTurb, CheckpointFileRoot[iTurb], &ErrStat, ErrMsg);
      checkError(ErrStat, ErrMsg);
    }
  }
  /* ******************************
     set inputs from this code and call FAST:
  ********************************* */

   for (int iTurb=0; iTurb < nTurbines; iTurb++) {

     //  set wind speeds at original locations 
     //  setOutputsToFAST(cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb]);


     // this advances the states, calls CalcOutput, and solves for next inputs. Predictor-corrector loop is imbeded here:
     // (note OpenFOAM could do subcycling around this step)
     FAST_OpFM_Step(&iTurb, &ErrStat, ErrMsg);
     checkError(ErrStat, ErrMsg);
   }

  nt_global = nt_global + 1;
  
  return 0;
}

int FAST_cInterface::readInputFile(std::string cInterfaceInputFile ) {

  // Check if the input file exists and call init
  if ( checkFileExists(cInterfaceInputFile) ) {

    YAML::Node cDriverInp = YAML::LoadFile(cInterfaceInputFile);
    
    nTurbines = cDriverInp["nTurbines"].as<int>();

    if (nTurbines > 0) {
   // Allocate memory for all inputs that are dependent on the number of turbines

      allocateInputData();
      
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
      }

      for (int iTurb=0; iTurb < nTurbines; iTurb++) {
	//	if (cDriverInp["Turbine" + std::to_string(iTurb)] == YAML::Node) {
	  readTurbineData(iTurb, cDriverInp["Turbine" + std::to_string(iTurb+1)] );
	  //	} else {
	  //	  throw std::runtime_error("Node for Turbine" + std::to_string(iTurb) + " not present in input file");
	  //	}
      }

      std::cout << "tStart = " << tStart << std::endl ;
      std::cout << "tEnd = " << tEnd << std::endl ;
      std::cout << "ntEnd = " << ntEnd << std::endl ;
      std::cout << "nEverycheckpoint = " << nEveryCheckPoint << std::endl ;
      
      init();
      
    } else {
      throw std::runtime_error("Number of turbines < 0 ");
    }
    
  } else {
    throw std::runtime_error("Input file " + cInterfaceInputFile + " does not exist or I cannot access it");
  }
  
}


void FAST_cInterface::checkError(const int ErrStat, const char * ErrMsg){

   if (ErrStat != ErrID_None){

      if (ErrStat >= AbortErrLev){
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


void FAST_cInterface::allocateInputData() {
	
  //Allocates memory for all the input data to be read from the file

  TurbID = new int[nTurbines];
  TurbinePos = new float* [nTurbines];
  FASTInputFileName = new char * [nTurbines];
  CheckpointFileRoot = new char * [nTurbines];
  numBlades = new int[nTurbines];
  numElementsPerBlade = new int[nTurbines];
  numTwrElements = new int[nTurbines];
  numScOutputs = new int[nTurbines];
  numScInputs = new int[nTurbines];
  
  for (int iTurb=0; iTurb < nTurbines; iTurb++) {
    TurbinePos[iTurb] = new float[3];
    FASTInputFileName[iTurb] = new char[INTERFACE_STRING_LENGTH];
    CheckpointFileRoot[iTurb] = new char[INTERFACE_STRING_LENGTH];
  }

  return;
}

void FAST_cInterface::readTurbineData(int iTurb, YAML::Node turbNode) {

  //Read turbine data for a given turbine using the YAML node
  
  std::cout << turbNode["TurbID"].as<int>() << std::endl;
  TurbID[iTurb] = turbNode["TurbID"].as<int>();

  std::cout << turbNode["FASTInputFileName"].as<std::string>() << std::endl ;
  std::strcpy(FASTInputFileName[iTurb], turbNode["FASTInputFileName"].as<std::string>().c_str()) ;

  if (turbNode["TurbinePos"].IsSequence() ) {
    std::vector<double> tp = turbNode["TurbinePos"].as<std::vector<double> >() ;
    for(int i=0;i<3;i++) {
      TurbinePos[iTurb][i] = tp[i];
      std::cout << TurbinePos[iTurb][i] << std::endl ;
    }
  }
  numScInputs[iTurb] = turbNode["numScInputs"].as<int>();
  std::cout << turbNode["numScInputs"].as<int>() << std::endl;
  numScOutputs[iTurb] = turbNode["numScOutputs"].as<int>();
  std::cout << turbNode["numScOutputs"].as<int>() << std::endl;

  std::cout << turbNode["restartFileName"].as<std::string>() << std::endl ;
  std::strcpy(CheckpointFileRoot[iTurb], turbNode["restartFileName"].as<std::string>().c_str() );

  std::cout << std::flush ;


  return ;

}
