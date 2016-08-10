#include "FAST_cInterface.h"

int main()

{

  string cDriverInputFile="cDriver.inp";

  FAST_cInterface FAST(cDriverInputFile);

  int nt=-1;
  
  for (nt = FAST.get_ntStart(); nt <= FAST.get_ntEnd(); nt++) {
    
    FAST.cDriverStep();

  }

    
}




