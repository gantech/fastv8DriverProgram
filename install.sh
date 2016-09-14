#Prepare sourceMods.sh
echo $PWD > /tmp/fastDir
sed -e "s/FASTDIR/$(sed 's:/:\\/:g' /tmp/fastDir)/" sourceMods.sh > /tmp/updatedFile
cp /tmp/updatedFile sourceMods.sh
source sourceMods.sh

#Registry
cd Source/dependencies/Registry/
make

#map-plus-plus
cd ../map-plus-plus
make

#yaml-cpp
cd ../yaml-cpp
cmake ../yaml-cpp/ -DCMAKE_INSTALL_PREFIX=../
make
make install

#FAST 
cd ../../../Compiling
make #Basic FAST Library
make -f makefile_DISCON_DLL #Bladed style controller libraries
make FAST_driver=FAST_Prog #Fortran driver program
make -f makefileCDriverProg cppOnly #C driver program


