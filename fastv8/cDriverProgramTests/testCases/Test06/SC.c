#include "SC.h"

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

SuperController::SuperController():
nGlobStates(0),
nTurbineStates(0),
globStates(NULL),
turbineStates(NULL)
{
  
}

SuperController::~SuperController() {
  if (NULL != globStates) {
    delete [] globStates;
  }
  
  if (NULL != turbineStates) {
    delete2DArray(turbineStates);
  }
}

void SuperController::init(int n, int nI, int nO) {
  
  nTurbines = n;
  nScInputs = nI;
  nScOutputs = nO;

  nGlobStates = 4; //Arbitrary
  globStates = new double[nGlobStates]; 

  nTurbineStates = nI + nO ; //Arbitrary selection for this supercontroller
  turbineStates = create2DArray<double>(nTurbines, nTurbineStates);
  
  //Initialize the turbine outputs at time zero

  turbineStates[0][nScInputs] = 0;
  turbineStates[1][nScInputs] = 0.5 * d2R ;
 
}

void SuperController::calcOutputs(double ** scOutputsGlob) {

  for(int iTurb=0; iTurb < nTurbines; iTurb++) {
    for(int i=0; i < nScOutputs; i++) {
      scOutputsGlob[iTurb][i] = turbineStates[iTurb][i+nScInputs] ;
    }
  }

#ifdef DEBUG

  std::cout << "nTurbines = " << nTurbines << std::endl ;
  std::cout << "nScInputs = "  << nScInputs  << std::endl ;
  std::cout << "nScOutputs = " << nScOutputs << std::endl ;

  for(int iTurb=0; iTurb < nTurbines; iTurb++) {
    for(int iInput=0; iInput < nScInputs; iInput++) {
      std::cout << "iTurb = " << iTurb << ", iInput = " << iInput << ",  " ;
      std::cout << scInputsGlob[iTurb][iInput] << std::endl ;
    }
  }

#endif

  
}


void SuperController::updateStates(double ** scInputsGlob) {


  // Meaning of scInputs
  // 0 - Time
  // 1 - GenTorque

  // Meaning of scOutputs
  // 0 - Minimum Blade pitch
  

  // Turbine 0
    /*  Vary PC_MinPit as a function of time: */
    /*  0-1s: 0 degrees */
    /*  1-2s: 1.5 degrees */
    /*  2-4s: 3 degrees */

  // Turbine 1
    /*  Vary PC_MinPit as a function of time: */
    /*  0-1s: 0.5 degrees */
    /*  1-2s: 1 degrees */
    /*  2-4s: 2.5 degrees */

  //Copy inputs into states first
  for(int iTurb=0; iTurb < nTurbines; iTurb++) {
    for(int i=0; i < nScInputs; i++) {
      turbineStates[iTurb][i] = scInputsGlob[iTurb][i];
    }
  }

  if (scInputsGlob[0][0] < 1.001) {
    turbineStates[0][nScInputs] = 0.0 ;
  } else if(scInputsGlob[0][0] < 2.001) {
    turbineStates[0][nScInputs] = 1.5 * d2R ;
  } else if(scInputsGlob[0][0] < 4.001) {
    turbineStates[0][nScInputs] = 3 * d2R ;    
  }

  if (scInputsGlob[1][0] < 1.001) {
    turbineStates[1][nScInputs] = 0.5 * d2R ;
  } else if(scInputsGlob[1][0] < 2.001) {
    turbineStates[1][nScInputs] = 1.0 * d2R ;
  } else if(scInputsGlob[1][0] < 4.001) {
    turbineStates[1][nScInputs] = 2.5 * d2R ;    
  }

}

int SuperController::readRestartFile(int n_t_global) {

  hid_t restartFile = H5Fopen(("sc" + std::to_string(n_t_global) + ".chkp.h5").c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
  
  {
    hid_t attr = H5Aopen(restartFile, "nTurbines", H5P_DEFAULT);
    herr_t ret = H5Aread(attr, H5T_NATIVE_INT, &nTurbines) ;
    H5Aclose(attr);

    attr = H5Aopen(restartFile, "nScInputs", H5P_DEFAULT);
    ret = H5Aread(attr, H5T_NATIVE_INT, &nScInputs) ;
    H5Aclose(attr);

    attr = H5Aopen(restartFile, "nScOutputs", H5P_DEFAULT);
    ret = H5Aread(attr, H5T_NATIVE_INT, &nScOutputs) ;
    H5Aclose(attr);

    attr = H5Aopen(restartFile, "nGlobStates", H5P_DEFAULT);
    ret = H5Aread(attr, H5T_NATIVE_INT, &nGlobStates) ;
    H5Aclose(attr);

    globStates = new double[nGlobStates]; 

    attr = H5Aopen(restartFile, "nTurbineStates", H5P_DEFAULT);
    ret = H5Aread(attr, H5T_NATIVE_INT, &nTurbineStates) ;
    H5Aclose(attr);

    turbineStates = create2DArray<double>(nTurbines, nTurbineStates);

#ifdef DEBUG
    std::cout << "nTurbines = " << nTurbines << std::endl ;
    std::cout << "nScInputs = " << nScInputs << std::endl ;
    std::cout << "nScOutputs = " << nScOutputs << std::endl ;
    std::cout << "nGlobStates = " << nGlobStates << std::endl ;
    std::cout << "nTurbineStates = " << nTurbineStates << std::endl ;
#endif
   
  }

  if (nGlobStates > 0) {
    hid_t dataSet = H5Dopen2(restartFile, "/globStates", H5P_DEFAULT);
    herr_t status = H5Dread(dataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, globStates);
    status = H5Dclose(dataSet);
  }
  
  if (nTurbineStates > 0) {
    hid_t dataSet = H5Dopen2(restartFile, "turbineStates", H5P_DEFAULT);
    herr_t status = H5Dread(dataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, turbineStates[0]);
    status = H5Dclose(dataSet);
  }

#ifdef DEBUG
  for(int iTurb=0; iTurb < nTurbines; iTurb++) {
    for(int i=0; i < nTurbineStates; i++) {
      std::cout << "iTurb = " << iTurb << ", i = " << i << ",  " ;
      std::cout << turbineStates[iTurb][i] << std::endl ;
    }
  }
#endif
  herr_t status = H5Fclose(restartFile);
  
}


int SuperController::writeRestartFile(int n_t_global) {

  /* // HDF5 stuff to write states to restart file or read back from it */

  hid_t restartFile = H5Fcreate(("sc" + std::to_string(n_t_global) + ".chkp.h5").c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  {
    hsize_t dims[1];
    dims[0] = 1;
    hid_t dataSpace = H5Screate_simple(1, dims, NULL);
    hid_t attr = H5Acreate2(restartFile, "nTurbines", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    herr_t status = H5Awrite(attr, H5T_NATIVE_INT, &nTurbines);
    status = H5Aclose(attr);
    status = H5Sclose(dataSpace);

    dataSpace = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(restartFile, "nScInputs", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    status = H5Awrite(attr, H5T_NATIVE_INT, &nScInputs);
    status = H5Aclose(attr);
    status = H5Sclose(dataSpace);

    dataSpace = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(restartFile, "nScOutputs", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    status = H5Awrite(attr, H5T_NATIVE_INT, &nScOutputs);
    status = H5Aclose(attr);
    status = H5Sclose(dataSpace);

    dataSpace = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(restartFile, "nGlobStates", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    status = H5Awrite(attr, H5T_NATIVE_INT, &nGlobStates);
    status = H5Aclose(attr);
    status = H5Sclose(dataSpace);

    dataSpace = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(restartFile, "nTurbineStates", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    status = H5Awrite(attr, H5T_NATIVE_INT, &nTurbineStates);
    status = H5Aclose(attr);
    status = H5Sclose(dataSpace);
    
  }

  if (nGlobStates > 0) {
    hsize_t dims[1];
    dims[0] = nGlobStates;
    hid_t dataSpace = H5Screate_simple(1, dims, NULL);
    hid_t dataSet = H5Dcreate2(restartFile, "/globStates", H5T_NATIVE_DOUBLE, dataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);    
    herr_t status = H5Dwrite(dataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, globStates);
    
    status = H5Dclose(dataSet);
    status = H5Sclose(dataSpace);
  }
  
  if (nTurbineStates > 0) {

    for(int iTurb=0; iTurb < nTurbines; iTurb++) {
      for(int i=0; i < nTurbineStates; i++) {
	std::cout << "iTurb = " << iTurb << ", i = " << i << ",  " ;
	std::cout << turbineStates[iTurb][i] << std::endl ;
      }
    }

    hsize_t dims[2];
    dims[0] = nTurbines;
    dims[1] = nTurbineStates;
    hid_t dataSpace = H5Screate_simple(2, dims, NULL);
    hid_t dataSet = H5Dcreate2(restartFile, "turbineStates", H5T_NATIVE_DOUBLE, dataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);    
    herr_t status = H5Dwrite(dataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, turbineStates[0]);
    
    status = H5Dclose(dataSet);
    status = H5Sclose(dataSpace);
  }

  herr_t status = H5Fclose(restartFile);

  return 0;

}



extern "C" SuperController* create_sc() {
  return new SuperController;
}

extern "C" void destroy_sc(SuperController* p) {
  delete p;
}





