#include "OpenFOAM_Types.h"
#include <iostream>

extern "C" void DISCON_SuperController(OpFM_InputType_t* turbine_controller, OpFM_OutputType_t* super_controller) {
  std::cout << "DISCON_SuperController" << '\n';
}
