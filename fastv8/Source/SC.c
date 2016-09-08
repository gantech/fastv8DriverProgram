#include "SuperController_Types.h"
#include <iostream>

extern "C" void DISCON_SuperController_CalcOutputs(double ** scInputsGlob, double ** scOutputsGlob, int nTurbinesGlob, int numScInputs, int numScOutputs) {

  // Meaning of scInputs
  // 0 - Time
  // 1 - GenTorque

  // Meaning of scOutputs
  // 0 - Minimum Blade pitch
  

  // Turbine 0
    /*  Vary PC_MinPit as a function of time: */
    /*  0-20s: 0 degrees */
    /*  20-40s: 1.5 degrees */
    /*  40-60s: 3 degrees */

  // Turbine 1
    /*  Vary PC_MinPit as a function of time: */
    /*  0-20s: 0.5 degrees */
    /*  20-40s: 1 degrees */
    /*  40-60s: 2.5 degrees */

  double d2R = 0.01745329251 ;

  if (scInputsGlob[0][0] < 20.0) {
    scOutputsGlob[0][0] = 0.0 ;
  } else if(scInputsGlob[0][0] < 40.0) {
    scOutputsGlob[0][0] = 1.5 * d2R ;
  } else if(scInputsGlob[0][0] < 40.0) {
    scOutputsGlob[0][0] = 3 * d2R ;    
  }

  if (scInputsGlob[1][0] < 20.0) {
    scOutputsGlob[1][0] = 0.5 * d2R ;
  } else if(scInputsGlob[1][0] < 40.0) {
    scOutputsGlob[1][0] = 1.0 * d2R ;
  } else if(scInputsGlob[1][0] < 40.0) {
    scOutputsGlob[1][0] = 2.5 * d2R ;    
  }
  
#ifdef DEBUG

  std::cout << "nTurbinesGlob = " << nTurbinesGlob << std::endl ;
  std::cout << "numScInputs = "  << numScInputs  << std::endl ;
  std::cout << "numScOutputs = " << numScOutputs << std::endl ;

  for(int iTurb=0; iTurb < nTurbinesGlob; iTurb++) {
    for(int iInput=0; iInput < numScInputs; iInput++) {
      std::cout << "iTurb = " << iTurb << ", iInput = " << iInput << ",  " ;
      std::cout << scInputsGlob[iTurb][iInput] << std::endl ;
    }
  }

#endif

  
}


extern "C" void DISCON_SuperController_UpdateStates(double ** scInputsGlob, double ** scOutputsGlob, int nTurbinesGlob, int numScInputs, int numScOutputs) {

}
