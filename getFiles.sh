#Get latest release of FASTv8 - 8.16.00a-bjj as of this writing
mkdir fastv8
cd fastv8
#wget https://nwtc.nrel.gov/system/files/FAST_v8.16.00a-bjj.tar.gz
#tar -zxf FAST_v8.16.00a-bjj.tar.gz
#rm FAST_v8.16.00a-bjj.tar.gz

#Get MAP
cd fastv8
git clone https://bitbucket.org/mmasciola/map-plus-plus.git
cd map-plus-plus
rm -rf .git
cd ../../


# #get Crunch for CertTests
# mkdir Crunch
# cd Crunch
# wget https://nwtc.nrel.gov/system/files/Crunch_v3.02.00c-mlb.tar.gz
# tar -zxf Crunch_v3.02.00c-mlb.tar.gz
# rm Crunch_v3.02.00c-mlb.tar.gz
# #Crunch needs an older version of the NWTC library
# mkdir NWTC_Library
# cd NWTC_Library
# wget https://nwtc.nrel.gov/system/files/NWTC_Lib_v1.07.02a-mlb.tar.gz
# tar -zxf NWTC_Lib_v1.07.02a-mlb.tar.gz
# rm NWTC_Lib_v1.07.02a-mlb.tar.gz





