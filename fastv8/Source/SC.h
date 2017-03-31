#include <sstream>
#include <iostream>
#include <string>
#include "yaml-cpp/yaml.h"
#include "hdf5.h"
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
  typedef void sc_updateStates_t(int nTurbines, int nScInputsTurbine, double ** sc_inputsTurbine, int nTurbineStates, double ** turbineStates_n, double ** turbineStates_np1, int nGlobStates, double * globStates_n, double * globStates_np1); 
  sc_updateStates_t * sc_updateStates;
  typedef void sc_calcOutputs_t(int nTurbines, int nScInputsTurbine, double ** sc_inputsTurbine, int nTurbineStates, double ** turbineStates, int nGlobStates, double * globStates, int nScOutputsTurbine, double ** sc_outputsTurbine); 
  sc_calcOutputs_t * sc_calcOutputs;


 public:

  SuperController();

  ~SuperController() ;

  void init(int nTurbinesGlob);
  
  void load(YAML::Node c);

  void calcOutputs(double ** sc_inputsTurbine, double ** sc_outputsTurbine) ;

  void updateStates(double ** sc_inputsTurbine) ; //Make a prediction for 'n+1' based on 'n'

  void advanceStates() ; //Advance states to time step 'n+1'

  int writeRestartFile(int n_t_global);

  int readRestartFile(int n_t_global);
};

