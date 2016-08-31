#include "FAST_cInterface.h"
#include <iostream>
#include <mpi.h>

int main() {
  int iErr;
  int nProcs;
  int rank;

  iErr = MPI_Init(NULL, NULL);
  iErr = MPI_Comm_size( MPI_COMM_WORLD, &nProcs);
  iErr = MPI_Comm_rank( MPI_COMM_WORLD, &rank);

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

  if( !FAST.isDryRun() ) {
    for (int nt = FAST.get_ntStart(); nt <= FAST.get_ntEnd(); nt++) {
      FAST.step();
    }
  }

  FAST.end() ;
  MPI_Finalize() ;

  return 0;
    
}

