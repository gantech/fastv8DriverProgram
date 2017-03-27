
#include <sstream>
#include <iostream>
#include "hdf5.h"
#include <string>
#include "dlfcn.h"

class SuperController {

 private:
  
  int nTurbines;
  int nScInputsTurbine;
  int nScOutputsTurbine;

  int nGlobStates; // Global states like time 
  double * globStates;
  double * globStates_np1;

  int nTurbineStates; // States for each turbine
  double ** turbineStates ;
  double ** turbineStates_np1 ;

  double d2R = 0.01745329251 ; //Degrees to Radians

  //Supercontroller stuff
  std::string scLibFile;
  // Dynamic load stuff copied from 'C++ dlopen mini HOWTO' on tldp.org
  void *scLibHandle ; 
  typedef void sc_updateStates_t(double ** sc_inputsTurbine, double ** turbineStates_n, double ** turbineStates_np1, double * globStates_n, double * globStates_np1); 
  sc_updateStates_t * sc_updateStates;
  typedef void sc_calcOutputs_t(double ** sc_inputsTurbine, double ** turbineStates, double * globStates, double ** sc_outputsTurbine); 
  sc_updateStates_t * sc_updateStates;


 public:

  SuperController();

  ~SuperController() ;

  void init(int n, int numScInputs, int numScOutputs);
  
  void loadSuperController(YAML::Node c);

  void calcOutputs(double ** sc_inputsTurbine, double ** sc_outputsTurbine) ;

  void updateStates(double ** sc_inputsTurbine) ; //Make a prediction for 'n+1' based on 'n'

  void advanceStates() ; //Advance states to time step 'n+1'

  int writeRestartFile(int n_t_global);

  int readRestartFile(int n_t_global);
};

