nTurbinesGlob: 2
dryRun:  False
superController: True
scLibFile: "libScontroller.so"
restart: False
tStart:  0.0
tEnd:    5.0
tMax:    60.0
ntStart: 0
ntEnd:   1600
dtFAST:  0.00625
nEveryCheckPoint: 1600

Turbine0:
  procNo: 0
  TurbinePos: [ 0.0, 0.0, 0.0 ]
  numScOutputs: 2
  numScInputs: 4
  restartFileName: "banana"
  FASTInputFileName: "t1_test06.fst"
  TurbID:  1

Turbine1:
  procNo: 1
  TurbinePos: [ 0.0, 0.0, 0.0 ]
  numScOutputs: 2
  numScInputs: 4
  restartFileName: "banana"
  FASTInputFileName: "t2_test06.fst"
  TurbID:  2

