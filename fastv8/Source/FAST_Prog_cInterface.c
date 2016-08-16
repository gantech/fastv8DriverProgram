#include "FAST_cInterface.h"
#include <iostream>

int main() {

  std::string cDriverInputFile="cDriver.i";
  FAST_cInterface FAST;
  try {
    FAST.readInputFile(cDriverInputFile);
  }
  catch( const std::runtime_error & ex) {
    std::cerr << ex.what() << std::endl ;
    std::cerr << "Program quitting now" << std::endl ;
    return 1;
  }


  for (int nt = FAST.get_ntStart(); nt <= FAST.get_ntEnd(); nt++) {
    FAST.step();
  }
  
  return 0;
    
}

