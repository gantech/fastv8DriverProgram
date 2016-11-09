import numpy as np
import os, sys
import re
""" Generates a series of turbine controllers with varying parameters as follows
#pcKI  - Varying from 0.008068634 to 0.008965149
#pcKP  - Varying from 0.01882681 to 0.006275604
#pcMinPit - Varying from 0.0 to 0.05235987755982988
"""

pcKI_0 = 0.008068634
pcKI_1 = 0.008965149

pcKP_0 = 0.01882681
pcKP_1 = 0.006275604

pcMinPit_0 = 0.0
pcMinPit_1 = 0.05235987755982988

def generateTurbineController(pcKP, pcKI, pcMinPit, turbID):
    
    with open("DISCONsample.f90", "r") as source:
        lines = source.readlines()
        with open('DISCON'+str(turbID)+'.f90', "w") as sourceN:
            for line in lines:
                if(re.search('customPCKI',line)):
                    sourceN.write(re.sub(r'customPCKI', str(pcKI), line))
                elif(re.search('customPCKP',line)):
                    sourceN.write(re.sub(r'customPCKP', str(pcKP), line))
                elif(re.search('customPCMinPit',line)):
                    sourceN.write(re.sub(r'customPCMinPit', str(pcMinPit), line))
                else:
                    sourceN.write(line)

    with open("../../NRELOffshrBsline5MW_Onshore_ServoDynSample.dat", "r") as source:
        lines = source.readlines()
        with open('../../NRELOffshrBsline5MW_Onshore_ServoDyn'+str(turbID)+'.dat', "w") as sourceN:
             for line in lines:
                sourceN.write(re.sub(r'turbNumber', str(turbID), line))

    with open("../../../tSample_Test07.fst", "r") as source:
        lines = source.readlines()
        with open('../../../t'+str(turbID)+'_Test07.fst', "w") as sourceN:
             for line in lines:
                sourceN.write(re.sub(r'turbNumber', str(turbID), line))

    with open('../../../cDriver.i','a') as caseFile:
        caseFile.write('Turbine'+str(turbID)+':'+'\n')
        caseFile.write('  procNo: '+str(turbID)+'\n')
        caseFile.write('  TurbinePos: [ 0.0, 0.0, 0.0 ]'+'\n')
        caseFile.write('  restartFileName: "banana"'+'\n')
        caseFile.write('  FASTInputFileName: "t'+str(turbID)+'_Test07.fst"'+'\n')
        caseFile.write('  TurbID:  '+str(turbID)+'\n')
        caseFile.write(''+'\n')

    with open('../../../makefile_DISCON_DLL','a') as makeFile:
        makeFile.write('$(SOURCE_DIR)/DISCON'+str(turbID)+'$(OBJ_EXT): $(SOURCE_DIR)/DISCON'+str(turbID)+'.f90'+'\n')
        makeFile.write('	$(FC)  $(FFLAGS) -c $< -o $@ '+'\n')
        makeFile.write(''+'\n')
        makeFile.write('$(DEST_DIR)/libDISCON'+str(turbID)+'$(SOBJ_EXT): $(SOURCE_DIR)/DISCON'+str(turbID)+'$(OBJ_EXT) '+'\n')
        makeFile.write('	$(FC) $(LDFLAGS) -I $(SOURCE_DIR) -o $(DEST_DIR)/libDISCON'+str(turbID)+'$(SOBJ_EXT) $(SOURCE_DIR)/DISCON'+str(turbID)+'$(OBJ_EXT)'+'\n')
        makeFile.write(''+'\n')
        makeFile.write(''+'\n')





def generateTurbineControllers(nt):

    with open('../../../cDriver.i','w') as caseFile:
        caseFile.write('nTurbinesGlob: '+str(nt)+'\n')
        caseFile.write('dryRun:  False'+'\n')
        caseFile.write('restart: False'+'\n')
        caseFile.write('tStart:  0.0'+'\n')
        caseFile.write('tEnd:    60.0'+'\n')
        caseFile.write('tMax:    60.0'+'\n')
        caseFile.write('ntStart: 0'+'\n')
        caseFile.write('ntEnd:   9600'+'\n')
        caseFile.write('dtFAST:  0.00625'+'\n')
        caseFile.write('nEveryCheckPoint: 10000'+'\n')
        caseFile.write(''+'\n')

    makefileCaseSourceFilesString = 'SOURCE_FILES = '
    makefileCaseDefaultString = 'default:  '
    for i in range(nt):
        makefileCaseSourceFilesString = makefileCaseSourceFilesString + 'DISCON' + str(i)+'.f90 '
        makefileCaseDefaultString = makefileCaseDefaultString + '$(DEST_DIR)/libDISCON'+str(i)+'$(SOBJ_EXT)  '
    makefileCaseDefaultString = makefileCaseDefaultString + '\n'
    makefileCaseSourceFilesString = makefileCaseSourceFilesString + '\n'    

    with open("../../../makefile_DISCON_DLL_sample", "r") as source:
        lines = source.readlines()
        with open('../../../makefile_DISCON_DLL', "w") as sourceN:
             for line in lines:
                 if re.search('CASE_SOURCEFILES',line):
                     sourceN.write(re.sub(r'CASE_SOURCEFILES', makefileCaseSourceFilesString, line))
                 elif re.search('CASE_DEFAULT',line):
                     sourceN.write(re.sub(r'CASE_DEFAULT', makefileCaseDefaultString, line))
                 else:
                     sourceN.write(line)


    for i in range(nt):
        turbInterp = float(i)/float(nt-1)
        pcKI = pcKI_0 + turbInterp * (pcKI_1 - pcKI_0)
        pcKP = pcKP_0 + turbInterp * (pcKP_1 - pcKP_0)
        pcMinPit = pcMinPit_0 + turbInterp * (pcMinPit_1 - pcMinPit_0)
        generateTurbineController(pcKP, pcKI, pcMinPit, i)

    

if __name__=="__main__":
    
    nTurbines= 0
    try:
        nTurbines = int(sys.argv[1])
        if (nTurbines < 1):
            print "Number of turbines should be atleast 1"
            exit
    except:
        print "Can't find/process nTurbines in the first argument to the program"
        exit

    generateTurbineControllers(nTurbines)        
        

