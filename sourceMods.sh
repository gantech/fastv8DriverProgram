module purge
module load gcc/4.8.1 lapack/3.4.2/gcc mkl cmake/3.3.2 openmpi-gcc/1.6.4-4.8.1   boost/1.55.0/openmpi-gcc hdf5/1.8.12/openmpi-gcc
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FASTDIR/fastv8/bin/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FASTDIR/fastv8/CertTest/5MW_Baseline/ServoData/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FASTDIR/fastv8/Source/dependencies/yaml-cpp/lib/
