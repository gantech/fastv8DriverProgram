#include "SC.h"

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


SuperController::SuperController():
nGlobStates(0),
nTurbineStates(0),
globStates(NULL),
turbineStates(NULL)
{
  
}

SuperController::~SuperController() {

    
  if(scLibHandle != NULL) {
      // close the library
      std::cout << "Closing SC library..." << std::endl;
      dlclose(scLibHandle);
  }
  
  if (NULL != globStates) delete [] globStates;
  if (NULL != globStates_np1) delete [] globStates_np1;
  if (NULL != turbineStates) delete2DArray(turbineStates);
  if (NULL != turbineStates_np1) delete2DArray(turbineStates_np1);
  }

}

void SuperController::load(YAML::Node c) {

    nScInputsTurbine = c["numScInputs"].as<int>();
    nScOutputsTurbine = c["numScOutputs"].as<int>();

    if (c["numScGlobStates"]) {
        nGlobStates = c["numScGlobStates"].as<int>();
        if (nGlobStates < 0)
            std::cerr << "numScGlobStates has to be greater than zero" << std::endl ;
    } else {
        std::cerr << "numScGlobStates not present in input file" << std::endl ;
    }

    if (c["numScTurbineStates"]) {
        nTurbineStates = c["numScTurbineStates"].as<int>();
        if (nTurbineStates < 0)
            std::cerr << "numScTurbineStates has to be greater than zero" << std::endl ;
    } else {
        std::cerr << "numScTurbineStates not present in input file" << std::endl ;
    }

    if (c["scLibFile"]) {
        scLibFile = c["scLibFile"].as<std::string>();
        
        // open the library
        scLibHandle = dlopen(scLibFile.c_str(), RTLD_LAZY);
        if (!scLibHandle) {
            std::cerr << "Cannot open library: " << dlerror() << '\n';
        }
        
        sc_updateStates = (sc_updateStates_t*) dlsym(scLibHandle, "sc_updateStates");
        // reset errors
        dlerror();
        const char *dlsym_error_us = dlerror();
        if (dlsym_error) {
            std::cerr << "Cannot load symbol 'sc_updateStates': " << dlsym_error_us << '\n';
            dlclose(scLibHandle);
        }
        
        sc_calcOutput = (sc_calcOutput_t*) dlsym(scLibHandle, "sc_calcOutput");
        // reset errors
        dlerror();
        const char *dlsym_error_co = dlerror();
        if (dlsym_error_us) {
            std::cerr << "Cannot load symbol 'sc_calcOutput': " << dlsym_error_co << '\n';
            dlclose(scLibHandle);
        }
    } else {
        std::cerr << "scLibFile not present in input file" << std::endl ;
    }

}

void SuperController::init(int nTurbinesGlob) {
  
  nTurbines = nTurbinesGlob;

  globStates = new double[nGlobStates]; 
  globStates_np1 = new double[nGlobStates]; 

  turbineStates = create2DArray<double>(nTurbines, nTurbineStates);
  turbineStates_np1 = create2DArray<double>(nTurbines, nTurbineStates);
  
  // Initialize the turbine states at time zero - Not sure how to do this. May be call calcOut?
 
}

void SuperController::calcOutputs(double ** sc_inputsTurbine, double ** sc_outputsTurbine) {

    sc_calcOutputs(sc_inputsTurbine, turbineStates, globStates, sc_outputsTurbine);   

/*   for(int iTurb=0; iTurb < nTurbines; iTurb++) { */
/*     for(int i=0; i < nScOutputsTurbine; i++) { */
/*       sc_outputsTurbine[iTurb][i] = turbineStates[iTurb][i+nScInputsTurbine] ; */
/*     } */
/*   } */

/* #ifdef DEBUG */

/*   std::cout << "nTurbines = " << nTurbines << std::endl ; */
/*   std::cout << "nScInputsTurbine = "  << nScInputsTurbine  << std::endl ; */
/*   std::cout << "nScOutputsTurbine = " << nScOutputsTurbine << std::endl ; */

/*   for(int iTurb=0; iTurb < nTurbines; iTurb++) { */
/*     for(int iInput=0; iInput < nScInputsTurbine; iInput++) { */
/*       std::cout << "iTurb = " << iTurb << ", iInput = " << iInput << ",  " ; */
/*       std::cout << sc_inputsTurbine[iTurb][iInput] << std::endl ; */
/*     } */
/*   } */

/* #endif */

  
}

void SuperController::advanceStates() {

  for(int iState=0; iState<nGlobStates; iState++) {
    globStates[iState] = globStates_np1[iState];
  }
  
  for(int iTurb=0; iTurb < nTurbines; iTurb++) {
    for(int iState=0; iState < nTurbineStates; iState++) {
      turbineStates[iTurb][iState] = turbineStates_np1[iTurb][iState];
    }
  }


void SuperController::updateStates(double ** sc_inputsTurbine) {


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

  sc_updateStates(sc_inputsTurbine, turbineStates_n, turbineStates_np1, globStates_n, globStates_np1)

/*   //Copy inputs into states first */
/*   for(int iTurb=0; iTurb < nTurbines; iTurb++) { */
/*     for(int i=0; i < nScInputsTurbine; i++) { */
/*       turbineStates[iTurb][i] = sc_inputsTurbine[iTurb][i]; */
/*     } */
/*   } */

/*   if (sc_inputsTurbine[0][0] < 20.0) { */
/*     turbineStates[0][nScInputsTurbine] = 0.0 ; */
/*   } else if(sc_inputsTurbine[0][0] < 40.0) { */
/*     turbineStates[0][nScInputsTurbine] = 1.5 * d2R ; */
/*   } else if(sc_inputsTurbine[0][0] < 40.0) { */
/*     turbineStates[0][nScInputsTurbine] = 3 * d2R ;     */
/*   } */

/*   if (sc_inputsTurbine[1][0] < 20.0) { */
/*     turbineStates[1][nScInputsTurbine] = 0.5 * d2R ; */
/*   } else if(sc_inputsTurbine[1][0] < 40.0) { */
/*     turbineStates[1][nScInputsTurbine] = 1.0 * d2R ; */
/*   } else if(sc_inputsTurbine[1][0] < 40.0) { */
/*     turbineStates[1][nScInputsTurbine] = 2.5 * d2R ;     */
/*   } */

}

int SuperController::readRestartFile(int n_t_global) {

  hid_t restartFile = H5Fopen(("sc" + std::to_string(n_t_global) + ".chkp.h5").c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
  
  {
    hid_t attr = H5Aopen(restartFile, "nTurbines", H5P_DEFAULT);
    herr_t ret = H5Aread(attr, H5T_NATIVE_INT, &nTurbines) ;
    H5Aclose(attr);

    attr = H5Aopen(restartFile, "nScInputsTurbine", H5P_DEFAULT);
    ret = H5Aread(attr, H5T_NATIVE_INT, &nScInputsTurbine) ;
    H5Aclose(attr);

    attr = H5Aopen(restartFile, "nScOutputsTurbine", H5P_DEFAULT);
    ret = H5Aread(attr, H5T_NATIVE_INT, &nScOutputsTurbine) ;
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
    std::cout << "nScInputsTurbine = " << nScInputsTurbine << std::endl ;
    std::cout << "nScOutputsTurbine = " << nScOutputsTurbine << std::endl ;
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
    attr = H5Acreate2(restartFile, "nScInputsTurbine", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    status = H5Awrite(attr, H5T_NATIVE_INT, &nScInputsTurbine);
    status = H5Aclose(attr);
    status = H5Sclose(dataSpace);

    dataSpace = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(restartFile, "nScOutputsTurbine", H5T_NATIVE_INT, dataSpace, H5P_DEFAULT, H5P_DEFAULT) ;
    status = H5Awrite(attr, H5T_NATIVE_INT, &nScOutputsTurbine);
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


