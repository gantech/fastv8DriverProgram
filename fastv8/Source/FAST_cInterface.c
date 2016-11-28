#include "FAST_cInterface.h"

#ifndef Contiguous2DArrayHack
#define Contiguous2DArrayHack

// Neat hack from http://stackoverflow.com/questions/21943621/how-to-create-a-contiguous-2d-array-in-c to allocate and deallocate contiguous 2D arrays in C++

  /* double **dPtr = create2DArray<double>(10,10); */
  /* dPtr[0][0] = 10;  // for example */
  /* delete2DArray(dPtr);  // free the memory */

template <typename T> T** create2DArray(unsigned nrows, unsigned ncols) {

  T** ptr = new T*[nrows];  // allocate pointers
  T* pool = new T[nrows*ncols];  // allocate pool
  for (unsigned i = 0; i < nrows; ++i, pool += ncols )
    ptr[i] = pool;
  return ptr;
}

template <typename T> void delete2DArray(T** arr) {

  delete [] arr[0];  // remove the pool
  delete [] arr;     // remove the pointers
}

#endif


//Constructor
FAST_cInterface::FAST_cInterface():
cDriver_Input_from_FAST(NULL),
cDriver_Output_to_FAST(NULL),
cDriverSC_Input_from_FAST(NULL),
cDriverSC_Output_to_FAST(NULL),
nTurbinesGlob(0),
nTurbinesProc(0),
scStatus(false),
timeZero(false)
{
#ifdef HAVE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &worldMPIRank);
  MPI_Comm_group(MPI_COMM_WORLD, &worldMPIGroup);
#else 
  worldMPIRank = 0;
#endif
}

inline bool FAST_cInterface::checkFileExists(const std::string& name) {
  struct stat buffer;   
  return (stat (name.c_str(), &buffer) == 0); 
}

int FAST_cInterface::init() {

    if (restart == false) {
      ntStart = 0;
      nt_global = ntStart;
      ntEnd = int((tEnd - tStart)/dtFAST);
      timeZero = true;
    }

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

   cDriverSC_Input_from_FAST = new SC_InputType_t* [nTurbinesProc] ;
   cDriverSC_Output_to_FAST = new SC_OutputType_t* [nTurbinesProc] ;
   for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
     cDriverSC_Input_from_FAST[iTurb] = malloc(sizeof(SC_InputType_t));
     cDriverSC_Output_to_FAST[iTurb] = malloc(sizeof(SC_OutputType_t));
     if (cDriverSC_Input_from_FAST[iTurb] == NULL || cDriverSC_Output_to_FAST[iTurb] == NULL) {
       throw std::runtime_error("Error allocating space for SC interface types.\n") ;
     }
   }

   // If restart 
   if (restart == true) {

     for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
       /* note that this will set nt_global inside the FAST library */
       FAST_OpFM_Restart(&iTurb, CheckpointFileRoot[iTurb], &AbortErrLev, &dtFAST, &numBlades[iTurb], &numElementsPerBlade[iTurb], &ntStart, cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb], cDriverSC_Input_from_FAST[iTurb], cDriverSC_Output_to_FAST[iTurb], &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);
       nt_global = ntStart;
       ntEnd = int((tEnd - tStart)/dtFAST) + ntStart;
     }

     if(scStatus) {
	 sc->readRestartFile(nt_global);
     }

   
   } else {
     
      // this calls the Init() routines of each module

     for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
       FAST_OpFM_Init(&iTurb, &tMax, FASTInputFileName[iTurb], &TurbID[iTurb], &numScOutputs, &numScInputs, TurbinePos[iTurb], &AbortErrLev, &dtFAST, &numBlades[iTurb], &numElementsPerBlade[iTurb], cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb], cDriverSC_Input_from_FAST[iTurb], cDriverSC_Output_to_FAST[iTurb], &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);
       
       numTwrElements[iTurb] = cDriver_Output_to_FAST[iTurb]->u_Len - numBlades[iTurb]*numElementsPerBlade[iTurb] - 1;


       if ( isDebug() ) {
	 for (int iNode=0; iNode < get_numNodes(iTurb); iNode++) {
	   std::cout << "Node " << iNode << " Position = " << cDriver_Input_from_FAST[iTurb]->px[iNode] << " " << cDriver_Input_from_FAST[iTurb]->py[iNode] << " " << cDriver_Input_from_FAST[iTurb]->pz[iNode] << " " << std::endl ;
	 }
       }
     }
     
   }
   
   return 0;

}

int FAST_cInterface::solution0() {

       // set wind speeds at initial locations
       //      setOutputsToFAST(cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb]);
     
     if(scStatus) {

       sc->init(nTurbinesGlob, numScInputs, numScOutputs);

       sc->calcOutputs(scOutputsGlob);
       fillScOutputsLoc();
     }

     for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {

       FAST_OpFM_Solution0(&iTurb, &ErrStat, ErrMsg);
       checkError(ErrStat, ErrMsg);

     }

     if (scStatus) {
       fillScInputsGlob(); // Update inputs to super controller
     }

     timeZero = false; // Turn the flag to compute solution0 off

     return 0;
}

int FAST_cInterface::step() {

  if ( (((nt_global - ntStart) % nEveryCheckPoint) == 0 )  && (nt_global != ntStart) ) {
    //sprintf(CheckpointFileRoot, "../../CertTest/Test18.%d", nt_global);
    for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
      sprintf(CheckpointFileRoot[iTurb], " "); // if blank, it will use FAST convention <RootName>.nt_global
      FAST_CreateCheckpoint(&iTurb, CheckpointFileRoot[iTurb], &ErrStat, ErrMsg);
      checkError(ErrStat, ErrMsg);
    }
    if(scStatus) {
#ifdef HAVE_MPI
      if (fastMPIRank == 0) {
#endif
      sc->writeRestartFile(nt_global);
#ifdef HAVE_MPI
      }
#endif
    }
  }
  /* ******************************
     set inputs from this code and call FAST:
  ********************************* */

   if(scStatus) {
     sc->calcOutputs(scOutputsGlob);
     fillScOutputsLoc();
   }

   for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {

     //  set wind speeds at original locations 
     //  setOutputsToFAST(cDriver_Input_from_FAST[iTurb], cDriver_Output_to_FAST[iTurb]);

     // this advances the states, calls CalcOutput, and solves for next inputs. Predictor-corrector loop is imbeded here:
     // (note OpenFOAM could do subcycling around this step)
     if ( isDebug() ) {
       for (int iNode=0; iNode < get_numNodes(iTurb); iNode++) {
	 std::cout << "Node " << iNode << " Velocity = " << cDriver_Output_to_FAST[iTurb]->u[iNode] << " " << cDriver_Output_to_FAST[iTurb]->v[iNode] << " " << cDriver_Output_to_FAST[iTurb]->w[iNode] << " " << std::endl ;
       }
     }

     FAST_OpFM_Step(&iTurb, &ErrStat, ErrMsg);
     checkError(ErrStat, ErrMsg);

     if ( isDebug() ) {
       for (int iNode=0; iNode < get_numNodes(iTurb); iNode++) {
	 std::cout << "Node " << iNode << " Position = " << cDriver_Input_from_FAST[iTurb]->px[iNode] << " " << cDriver_Input_from_FAST[iTurb]->py[iNode] << " " << cDriver_Input_from_FAST[iTurb]->pz[iNode] << " " << std::endl ;
       }
       for (int iNode=0; iNode < get_numNodes(iTurb); iNode++) {
	 std::cout << "Node " << iNode << " Type " << getNodeType(turbineMapProcToGlob[iTurb], iNode) << " Force = " << cDriver_Input_from_FAST[iTurb]->fx[iNode] << " " << cDriver_Input_from_FAST[iTurb]->fy[iNode] << " " << cDriver_Input_from_FAST[iTurb]->fz[iNode] << " " << std::endl ;
       }
     }

   }

   if(scStatus) {
     sc->updateStates(scInputsGlob); // Go from 'n' to 'n+1' based on input at previous time step
     fillScInputsGlob(); // Update inputs to super controller for 'n+1'
   }

   nt_global = nt_global + 1;
  
   return 0;
}

int FAST_cInterface::setGlobalInputs(int nTsGlob, bool dRun, bool sController, std::string sControlLib, bool rStart, double timeStart, double timeEnd, double timeMax, double timeStep, int nCheckpoint, int nScOutputs, int nScInputs ) {
  
  // Read inputs from arguments instead of a file

  nTurbinesGlob = nTsGlob;

  if (nTurbinesGlob > 0) {
    
    dryRun = dRun ; 
    
    allocateTurbinesToProcsSimple();
    
    allocateInputData(); // Allocate memory for all inputs that are dependent on the number of turbines
    
    restart = rStart;
    tStart = timeStart;
    tEnd = timeEnd;
    tMax = timeMax;
    nEveryCheckPoint = nCheckpoint;
    
    //    loadSuperController(); - Super controller functionality disabled for now
    
    if (restart == false) {
      ntStart = 0;
      nt_global = ntStart;
      dtFAST = timeStep;
      ntEnd = int((tEnd - tStart)/dtFAST);
    }
    
  } else {
    throw std::runtime_error("Number of turbines < 0 ");
  }
  
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
	solution0();
      }
      
    } else {
      throw std::runtime_error("Number of turbines < 0 ");
    }
    
  } else {
    throw std::runtime_error("Input file " + cInterfaceInputFile + " does not exist or I cannot access it");
  }
  
}

int FAST_cInterface::readInputFile(const YAML::Node & cDriverInp) {

  nTurbinesGlob = cDriverInp["n_turbines_glob"].as<int>();

  if (nTurbinesGlob > 0) {
    
    if(cDriverInp["dry_run"]) {
      dryRun = cDriverInp["dry_run"].as<bool>();
    } else {
      dryRun = false;
    }
    
    if(cDriverInp["debug"]) {
      debug = cDriverInp["debug"].as<bool>();
    } else {
      debug = false;
    }

    allocateTurbinesToProcs(cDriverInp);
    
    allocateInputData(); // Allocate memory for all inputs that are dependent on the number of turbines
    
    
    /* restart - Has to come from cfd solver */ 
    /* tStart - Has to come from cfd solver */
    /* tEnd - Has to come from cfd solver */
    /* dtFAST - Has to come from cfd solver */
    tMax = cDriverInp["tMax"].as<double>(); // tMax is the total duration to which you want to run FAST. This should be the same as the end time given in the FAST fst file. Choose this carefully as FAST writes the output file only at this point if you choose the binary file output.
    nEveryCheckPoint = cDriverInp["n_every_checkpoint"].as<int>();
    
    loadSuperController(cDriverInp);
    
    for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
      readTurbineData(iTurb, cDriverInp["Turbine" + std::to_string(turbineMapProcToGlob[iTurb])] );
    }
    
  } else {
    throw std::runtime_error("Number of turbines < 0 ");
    return 1;
  }
  
  return 0;
  
}

void FAST_cInterface::setRestart(const bool & isRestart) {
  /* Set whether the simulation is restarted or from scratch */
  restart = isRestart;
}

void FAST_cInterface::setTstart(const double & cfdTstart) {
  /* Set the end time for the simulation */
  tStart = cfdTstart;
}

void FAST_cInterface::setDt(const double & cfdDt) {
  /* Set the time step for the simulation */
  dtFAST = cfdDt;
}

void FAST_cInterface::setTend(const double & cfdTend) {
  /* Set the end time for the simulation */
  tEnd = cfdTend;
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

void FAST_cInterface::getCoordinates(double *currentCoords, int iNode) {

  // Set coordinates at current node of current turbine - Only one turbine for now
  currentCoords[0] = cDriver_Input_from_FAST[0]->px[iNode] ;
  currentCoords[1] = cDriver_Input_from_FAST[0]->py[iNode] ;
  currentCoords[2] = cDriver_Input_from_FAST[0]->pz[iNode] ;

}

void FAST_cInterface::getForce(std::vector<double> & currentForce, int iNode) {

  // Set forces at current node of current turbine - Only one turbine for now
  currentForce[0] = -cDriver_Input_from_FAST[0]->fx[iNode] ;
  currentForce[1] = -cDriver_Input_from_FAST[0]->fy[iNode] ;
  currentForce[2] = -cDriver_Input_from_FAST[0]->fz[iNode] ;

}

void FAST_cInterface::setVelocity(std::vector<double> & currentVelocity, int iNode) {

  // Set velocity at current node of current turbine - Only one turbine for now
  cDriver_Output_to_FAST[0]->u[iNode] = currentVelocity[0];
  cDriver_Output_to_FAST[0]->v[iNode] = currentVelocity[1];
  cDriver_Output_to_FAST[0]->w[iNode] = currentVelocity[2];

}

ActuatorNodeType FAST_cInterface::getNodeType(int iTurbGlob, int iNode) {
  // Return the type of actuator node for the given node number. The node ordering (from FAST) is 
  // Node 0 - Hub node
  // Blade 1 nodes
  // Blade 2 nodes
  // Blade 3 nodes
  // Tower nodes

  int iTurbLoc = get_localTurbNo(iTurbGlob);
  if (iNode) {
    if ( (iNode + 1 - (get_numNodes(iTurbLoc) - get_numTwrNodes(iTurbLoc)) ) > 0) {
      return TOWER; 
    }
    else {
      return BLADE;
    }
  }
  else {
    return HUB; 
  }
  
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
  
  for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
    TurbinePos[iTurb] = new float[3];
    FASTInputFileName[iTurb] = new char[INTERFACE_STRING_LENGTH];
    CheckpointFileRoot[iTurb] = new char[INTERFACE_STRING_LENGTH];
  }

  return;
}

void FAST_cInterface::readTurbineData(int iTurb, YAML::Node turbNode) {

  //Read turbine data for a given turbine using the YAML node
  
  TurbID[iTurb] = turbNode["turb_id"].as<int>();
  std::strcpy(FASTInputFileName[iTurb], turbNode["FAST_input_filename"].as<std::string>().c_str()) ;
  std::strcpy(CheckpointFileRoot[iTurb], turbNode["restart_filename"].as<std::string>().c_str() );
  if (turbNode["turbine_pos"].IsSequence() ) {
    std::vector<double> tp = turbNode["turbine_pos"].as<std::vector<double> >() ;
    for(int i=0;i<3;i++) {
      TurbinePos[iTurb][i] = tp[i];
    }
  }

  return ;
}

void FAST_cInterface::setTurbineData(int tid, std::string fastInpFile, std::string fastRstartFile, std::vector<double> tPos) {

  //Read turbine data for a given turbine using arguments - Assumes only one turbine on a given processor
  
  TurbID[0] = tid;
  std::strcpy(FASTInputFileName[0], fastInpFile.c_str()) ;
  std::strcpy(CheckpointFileRoot[0], fastRstartFile.c_str() );
  for(int i=0;i<3;i++) {
    TurbinePos[0][i] = tPos[i];
  }

  return ;
}


void FAST_cInterface::allocateTurbinesToProcsSimple() {

  //Simple allocation - n Turbines go on to the first n procs
  
  for (int iTurb=0; iTurb < nTurbinesGlob; iTurb++) {
    turbineMapGlobToProc[iTurb] = iTurb;
    if (dryRun) {
      if(worldMPIRank == 0) {
	std::cout << "iTurb = " << iTurb << " turbineMapGlobToProc[iTurb] = " << turbineMapGlobToProc[iTurb] << std::endl ;
      }
    }
    if(worldMPIRank == turbineMapGlobToProc[iTurb]) {
      turbineMapProcToGlob[nTurbinesProc] = iTurb;
      reverseTurbineMapProcToGlob[iTurb] = nTurbinesProc;
      nTurbinesProc++ ;
    }
    turbineSetProcs.insert(turbineMapGlobToProc[iTurb]);
  }
#ifdef HAVE_MPI  
  if ( dryRun ) {
    MPI_Barrier(MPI_COMM_WORLD);  
  }
#endif

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
#ifdef HAVE_MPI
      MPI_Barrier(MPI_COMM_WORLD);  
#endif
    }

    nProcsWithTurbines++ ;
  }
    
#ifdef HAVE_MPI
  // Construct a group containing all procs running atleast 1 turbine in FAST
  MPI_Group_incl(worldMPIGroup, nProcsWithTurbines, turbineProcs, &fastMPIGroup) ;
  int fastMPIcommTag = MPI_Comm_create(MPI_COMM_WORLD, fastMPIGroup, &fastMPIComm);
  if (MPI_COMM_NULL != fastMPIComm) {
    MPI_Comm_rank(fastMPIComm, &fastMPIRank);
  }
#endif

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
      reverseTurbineMapProcToGlob[iTurb] = nTurbinesProc;
      nTurbinesProc++ ;
    }
    turbineSetProcs.insert(turbineMapGlobToProc[iTurb]);
  }
#ifdef HAVE_MPI  
  if ( dryRun ) {
    MPI_Barrier(MPI_COMM_WORLD);  
  }
#endif

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
#ifdef HAVE_MPI
      MPI_Barrier(MPI_COMM_WORLD);  
#endif
    }

    nProcsWithTurbines++ ;
  }
    
#ifdef HAVE_MPI
  // Construct a group containing all procs running atleast 1 turbine in FAST
  MPI_Group_incl(worldMPIGroup, nProcsWithTurbines, turbineProcs, &fastMPIGroup) ;
  int fastMPIcommTag = MPI_Comm_create(MPI_COMM_WORLD, fastMPIGroup, &fastMPIComm);
  if (MPI_COMM_NULL != fastMPIComm) {
    MPI_Comm_rank(fastMPIComm, &fastMPIRank);
  }
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

      if (scStatus) {
	for (int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
	  if (cDriverSC_Input_from_FAST[iTurb] != NULL) {
	    free(cDriverSC_Input_from_FAST[iTurb]);
	    cDriverSC_Input_from_FAST[iTurb] = NULL;
	  }
	  if (cDriverSC_Output_to_FAST[iTurb] != NULL) {
	    free(cDriverSC_Output_to_FAST[iTurb]);
	    cDriverSC_Output_to_FAST[iTurb] = NULL;
	  }
	}
	delete[] cDriverSC_Input_from_FAST;
	delete[] cDriverSC_Output_to_FAST;
	
	delete2DArray(scInputsGlob);
	delete2DArray(scOutputsGlob);
	
      }

    }

#ifdef HAVE_MPI
    MPI_Group_free(&fastMPIGroup);
    if (MPI_COMM_NULL != fastMPIComm) {
      MPI_Comm_free(&fastMPIComm);
    }
    MPI_Group_free(&worldMPIGroup);
#endif    

    if(scStatus) {

      destroy_SuperController(sc) ;

      if(scLibHandle != NULL) {
	// close the library
	std::cout << "Closing library...\n";
	dlclose(scLibHandle);
      }
      
    }

  }


void FAST_cInterface::loadSuperController(YAML::Node c) {

  if(c["superController"]) {
    scStatus = c["superController"].as<bool>();
    scLibFile = c["scLibFile"].as<std::string>();

    // open the library
    scLibHandle = dlopen(scLibFile.c_str(), RTLD_LAZY);
    if (!scLibHandle) {
      std::cerr << "Cannot open library: " << dlerror() << '\n';
    }
    
    create_SuperController = (create_sc_t*) dlsym(scLibHandle, "create_sc");
    // reset errors
    dlerror();
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
      std::cerr << "Cannot load symbol 'create_sc': " << dlsym_error << '\n';
      dlclose(scLibHandle);
    }

    destroy_SuperController = (destroy_sc_t*) dlsym(scLibHandle, "destroy_sc");
    // reset errors
    dlerror();
    const char *dlsym_error_us = dlerror();
    if (dlsym_error_us) {
      std::cerr << "Cannot load symbol 'destroy_sc': " << dlsym_error_us << '\n';
      dlclose(scLibHandle);
    }

    sc = create_SuperController() ;

    numScInputs = c["numScInputs"].as<int>();
    numScOutputs = c["numScOutputs"].as<int>();

    if ( (numScInputs > 0) && (numScOutputs > 0)) {
      scOutputsGlob = create2DArray<double>(nTurbinesGlob, numScOutputs);
      scInputsGlob = create2DArray<double>(nTurbinesGlob, numScInputs);
      for (int iTurb=0; iTurb < nTurbinesGlob; iTurb++) {
	for(int iInput=0; iInput < numScInputs; iInput++) {
	  scInputsGlob[iTurb][iInput] = 0.0 ; // Initialize to zero
	}
	for(int iOutput=0; iOutput < numScOutputs; iOutput++) {
	  scOutputsGlob[iTurb][iOutput] = 0.0 ; // Initialize to zero
	}
      }

    } else {
      std::cerr <<  "Make sure numScInputs and numScOutputs are greater than zero" << std::endl;
    }
    
   } else {
    scStatus = false;
    numScInputs = 0;
    numScOutputs = 0;
   }

}


void FAST_cInterface::fillScInputsGlob() {
  
  // Fills the global array containing inputs to the supercontroller from all turbines

  for(int iTurb=0; iTurb < nTurbinesGlob; iTurb++) {
    for(int iInput=0; iInput < numScInputs; iInput++) {
      scInputsGlob[iTurb][iInput] = 0.0; // Initialize to zero 
    }
  }
  
  for(int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
    for(int iInput=0; iInput < numScInputs; iInput++) {
      scInputsGlob[turbineMapProcToGlob[iTurb]][iInput] = cDriverSC_Input_from_FAST[iTurb]->toSC[iInput] ;
    }
  }
  
  
#ifdef HAVE_MPI
  if (MPI_COMM_NULL != fastMPIComm) {
    MPI_Allreduce(MPI_IN_PLACE, scInputsGlob[0], numScInputs*nTurbinesGlob, MPI_DOUBLE, MPI_SUM, fastMPIComm) ;
  }
#endif
  

}


void FAST_cInterface::fillScOutputsLoc() {
  
  // Fills the local array containing outputs from the supercontroller to each turbine
  
  for(int iTurb=0; iTurb < nTurbinesProc; iTurb++) {
    for(int iOutput=0; iOutput < numScOutputs; iOutput++) {
      cDriverSC_Output_to_FAST[iTurb]->fromSC[iOutput] = scOutputsGlob[turbineMapProcToGlob[iTurb]][iOutput] ;
    }
  }

}












