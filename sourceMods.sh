module purge
module use /nopt/nrel/apps/modules/candidate/modulefiles/
module load impi-intel/5.1.3-16.0.2 lapack/3.4.2/intel-16.0.2 mkl/16.2.181 cmake/3.3.2
module unload epel
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FASTDIR/fastv8/lib/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FASTDIR/fastv8/CertTest/5MW_Baseline/ServoData/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FASTDIR/fastv8/Source/dependencies/yaml-cpp/lib/
