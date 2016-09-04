#include "FAST_cInterface.h"


//Constructor
FAST_cInterface::FAST_cInterface():
cDriver_Input_from_FAST(NULL),
cDriver_Output_to_FAST(NULL),
nTurbinesGlob(0),
nTurbinesProc(0),
scStatus(false)
{
#ifdef HAVE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &worldMPIRank);
  MPI_Comm_group(MPI_COMM_WORLD, &worldMPIGroup);
#endif
}

inline bool FAST_cInterface::checkFileExists(const std::string& name) {
  struct stat buffer;   
  return (stat (name.c_str(), &buffer) == 0); 
}

int FAST_cInterface::init() {

   // Allocate memory for Turbine datastructure for all turbines
   FAST_AllocateTurbines(&nTurbinesProc, &ErrStat, ErrMsg);

   // Allocate memory for OpFM Input types in FAST

   cDriver_Input_from_FAST = new OpFM_InputType_t* [nTurbinesProc] ;
   cDriver_Output_to_FAST = new OpFM_OutputType_t* [nTurbinesProc] ;
   for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
     cDriver_Input_from_FAST[iTurb] = malloc(sizeof(OpFM_InputType_t));
     cDriver_Output_to_FAST[iTurb] = malloc(sizeof(OpFM_OutputType_t));
     if (cDriver_Input_from_FAST[iTurb] == NULL || cDriver_Output_to_FAST[iTurb] == NULL) {
       throw std::runtime_error("Error allocating space for OpFM interface types.\n") ;
     }
   }

   // If restart 
   if (restart == true) {

     for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
       /* note that this will set nt_global inside the FAST library */
       FAST_OpFM_Restart(&iTurb, CheckpointFileRoot[iTurb], &AbortErrLev, &dtFAST, &numBlades[iTurb], &numElementsPerBlade[iTurb], &ntStart, cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb], &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);
       nt_global = ntStart;
       ntEnd = int((tEnd - tStart)/dtFAST) + ntStart;
     }

   } else {
     
      // this calls the Init() routines of each module

     for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
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
    for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
      sprintf(CheckpointFileRoot[iTurb], " "); // if blank, it will use FAST convention <RootName>.nt_global
      FAST_CreateCheckpoint(&iTurb, CheckpointFileRoot[iTurb], &ErrStat, ErrMsg);
      checkError(ErrStat, ErrMsg);
    }
  }
  /* ******************************
     set inputs from this code and call FAST:
  ********************************* */

   for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {

     //  set wind speeds at original locations 
     //  setOutputsToFAST(cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb]);

     // this advances the states, calls CalcOutput, and solves for next inputs. Predictor-corrector loop is imbeded here:
     // (note OpenFOAM could do subcycling around this step)
     FAST_OpFM_Step(&iTurb, &ErrStat, ErrMsg);
     checkError(ErrStat, ErrMsg);

     /* if(scStatus) { */
     /*   DISCON_SuperController(cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb]); */
     /* } */
	
   }

  nt_global = nt_global + 1;
  
  return 0;
}

int FAST_cInterface::readInputFile(std::string cInterfaceInputFile ) {

  // Check if the input file exists and call init
  if ( checkFileExists(cInterfaceInputFile) ) {

    YAML::Node cDriverInp = YAML::LoadFile(cInterfaceInputFile);
    
    nTurbinesGlob = cDriverInp["nTurbinesGlob"].as<int>();

    if (nTurbinesGlob > 0) {
      
      if(cDriverInp["dryRun"]) {
	dryRun = cDriverInp["dryRun"].as<bool>();
      } else {
	dryRun = false;
      }
      
      allocateTurbinesToProcs(cDriverInp);

      allocateInputData(); // Allocate memory for all inputs that are dependent on the number of turbines

      
      restart = cDriverInp["restart"].as<bool>();
      tStart = cDriverInp["tStart"].as<double>();
      tEnd = cDriverInp["tEnd"].as<double>();
      tMax = cDriverInp["tMax"].as<double>();
      nEveryCheckPoint = cDriverInp["nEveryCheckPoint"].as<int>();
      
      loadSuperController(cDriverInp);

      if (restart == false) {
	ntStart = 0;
	nt_global = ntStart;
	dtFAST = cDriverInp["dtFAST"].as<double>();
	ntEnd = int((tEnd - tStart)/dtFAST);
      }

      for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
	//	if (cDriverInp["Turbine" + std::to_string(iTurb)] == YAML::Node) {
	  readTurbineData(iTurb, cDriverInp["Turbine" + std::to_string(turbineMapProcToGlob[iTurb])] );
	  //	} else {
	  //	  throw std::runtime_error("Node for Turbine" + std::to_string(iTurb) + " not present in input file");
	  //	}
      }

      if ( !dryRun ) {
	init();
      }
      
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

  TurbID = new int[nTurbinesProc];
  TurbinePos = new float* [nTurbinesProc];
  FASTInputFileName = new char * [nTurbinesProc];
  CheckpointFileRoot = new char * [nTurbinesProc];
  numBlades = new int[nTurbinesProc];
  numElementsPerBlade = new int[nTurbinesProc];
  numTwrElements = new int[nTurbinesProc];
  numScOutputs = new int[nTurbinesProc];
  numScInputs = new int[nTurbinesProc];
  
  for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
    TurbinePos[iTurb] = new float[3];
    FASTInputFileName[iTurb] = new char[INTERFACE_STRING_LENGTH];
    CheckpointFileRoot[iTurb] = new char[INTERFACE_STRING_LENGTH];
  }

  return;
}

void FAST_cInterface::readTurbineData(int iTurb, YAML::Node turbNode) {

  //Read turbine data for a given turbine using the YAML node
  
  TurbID[iTurb] = turbNode["TurbID"].as<int>();
  std::strcpy(FASTInputFileName[iTurb], turbNode["FASTInputFileName"].as<std::string>().c_str()) ;
  std::strcpy(CheckpointFileRoot[iTurb], turbNode["restartFileName"].as<std::string>().c_str() );
  if (turbNode["TurbinePos"].IsSequence() ) {
    std::vector<double> tp = turbNode["TurbinePos"].as<std::vector<double> >() ;
    for(int i=0;i<3;i++) {
      TurbinePos[iTurb][i] = tp[i];
    }
  }
  numScInputs[iTurb] = turbNode["numScInputs"].as<int>();
  numScOutputs[iTurb] = turbNode["numScOutputs"].as<int>();

  return ;
}


void FAST_cInterface::allocateTurbinesToProcs(YAML::Node cDriverNode) {
  
  // Allocate turbines to each processor
  
  for (int iTurb=0; iTurb < nTurbinesGlob; iTurb++) {
    turbineMapGlobToProc[iTurb] = cDriverNode["Turbine" + std::to_string(iTurb)]["procNo"].as<int>();
    if (dryRun) {
      if(worldMPIRank == 0) {
	std::cout << "iTurb = " << iTurb << " turbineMapGlobToProc[iTurb] = " << turbineMapGlobToProc[iTurb] << std::endl ;
      }
    }
    if(worldMPIRank == turbineMapGlobToProc[iTurb]) {
      turbineMapProcToGlob[nTurbinesProc] = iTurb;
      nTurbinesProc++ ;
    }
    turbineSetProcs.insert(turbineMapGlobToProc[iTurb]);
  }
  if ( dryRun ) {
    MPI_Barrier(MPI_COMM_WORLD);  
  }

  int nProcsWithTurbines=0;
  turbineProcs = new int[turbineSetProcs.size()];
  for (std::set<int>::const_iterator p = turbineSetProcs.begin(); p != turbineSetProcs.end(); p++) {
    turbineProcs[nProcsWithTurbines] = *p;

    if (dryRun) {
      if ( worldMPIRank == turbineProcs[nProcsWithTurbines] ) {
	for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
	  std::cout << "Proc " << worldMPIRank << " loc iTurb " << iTurb << " glob iTurb " << turbineMapProcToGlob[iTurb] << std::endl ;
	}
      }
      MPI_Barrier(MPI_COMM_WORLD);  
    }

    nProcsWithTurbines++ ;
  }
    
#ifdef HAVE_MPI
  // Construct a group containing all procs running atleast 1 turbine in FAST
  MPI_Group_incl(worldMPIGroup, nProcsWithTurbines, turbineProcs, &fastMPIGroup) ;
  int fastMPIcommTag = MPI_Comm_create(MPI_COMM_WORLD, fastMPIGroup, &fastMPIComm);
#endif

  return ;
}


void FAST_cInterface::end() {

    // Deallocate types we allocated earlier
  
    if ( !dryRun) {
      for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
	FAST_End(&iTurb);
      }
    }

    for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
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
    
    if ( !dryRun ) {
      for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
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

#ifdef HAVE_MPI
    MPI_Group_free(&fastMPIGroup);
    if (MPI_COMM_NULL != fastMPIComm) {
      MPI_Comm_free(&fastMPIComm);
    }
    MPI_Group_free(&worldMPIGroup);
#endif    

    if(scStatus) {

      if(scLibHandle != NULL) {
	dlclose(scLibHandle);
      }
      
    }

  }


void FAST_cInterface::loadSuperController(YAML::Node c) {

  if(c["superController"]) {
    scStatus = c["superController"].as<bool>();
    std::cout << "scStatus = " << scStatus << std::endl ;
    scLibFile = c["scLibFile"].as<std::string>();
    scLibHandle = dlopen("libScontroller.so", RTLD_LAZY);
    DISCON_sc_t DISCON_SuperController = (DISCON_sc_t) dlsym(scLibHandle, "DISCON_SuperController");

    const char *dlsym_error = dlerror();
    if (dlsym_error) {
      dlclose(scLibHandle);
      throw std::runtime_error("Cannot load symbol 'DISCON_SuperController' from shared library\n") ;
    }

  } else {
    scStatus = false;
  }


}
