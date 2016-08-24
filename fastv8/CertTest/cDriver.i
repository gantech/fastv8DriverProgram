nTurbines: 2
restart: False
tStart:  0.0
tEnd:    5.0
tMax:    60.0
ntStart: 0
ntEnd:   800
dtFAST:  0.00625
nEveryCheckPoint: 800

Turbine1:
  TurbinePos: [ 0.0, 0.0, 0.0 ]
  numScOutputs: 0
  numScInputs: 0
  restartFileName: "blah"
  FASTInputFileName: "t1_test18.fst"
  TurbID:  1

Turbine2:
  TurbinePos: [ 0.0, 0.0, 0.0 ]
  numScOutputs: 0
  numScInputs: 0
  restartFileName: "blah"
  FASTInputFileName: "t2_test18.fst"
  TurbID:  2
