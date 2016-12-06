//!STARTOFREGISTRYGENERATEDFILE 'OpenFOAM_Types.h'
//!
//! WARNING This file is generated automatically by the FAST registry.
//! Do not edit.  Your changes to this file will be lost.
//!

#ifndef _OpenFOAM_TYPES_H
#define _OpenFOAM_TYPES_H


#ifdef _WIN32 //define something for Windows (32-bit)
#  include "stdbool.h"
#  define CALL __declspec( dllexport )
#elif _WIN64 //define something for Windows (64-bit)
#  include "stdbool.h"
#  define CALL __declspec( dllexport ) 
#else
#  include <stdbool.h>
#  define CALL 
#endif


  typedef struct OpFM_InitInputType {
    void * object ;
    int NumSC2Ctrl ;
    int NumCtrl2SC ;
    int NumActForcePtsBlade ;
    int NumActForcePtsTower ;
  } OpFM_InitInputType_t ;
  typedef struct OpFM_InitOutputType {
    void * object ;
    char * WriteOutputHdr ;     int WriteOutputHdr_Len ;
    char * WriteOutputUnt ;     int WriteOutputUnt_Len ;

  } OpFM_InitOutputType_t ;
  typedef struct OpFM_BladePropsType {
    void * object ;
    int NumBlNds ;
    float * BlSpn ;     int BlSpn_Len ;
    float * BlCrvAC ;     int BlCrvAC_Len ;
    float * BlSwpAC ;     int BlSwpAC_Len ;
    float * BlCrvAng ;     int BlCrvAng_Len ;
    float * BlTwist ;     int BlTwist_Len ;
    float * BlChord ;     int BlChord_Len ;
    int * BlAFID ;     int BlAFID_Len ;
  } OpFM_BladePropsType_t ;
  typedef struct OpFM_MiscVarType {
    void * object ;





  } OpFM_MiscVarType_t ;
  typedef struct OpFM_ParameterType {
    void * object ;
    float AirDens ;
    int NnodesVel ;
    int NnodesForce ;
  } OpFM_ParameterType_t ;
  typedef struct OpFM_InputType {
    void * object ;
    float * pxVel ;     int pxVel_Len ;
    float * pyVel ;     int pyVel_Len ;
    float * pzVel ;     int pzVel_Len ;
    float * pxForce ;     int pxForce_Len ;
    float * pyForce ;     int pyForce_Len ;
    float * pzForce ;     int pzForce_Len ;
    float * fx ;     int fx_Len ;
    float * fy ;     int fy_Len ;
    float * fz ;     int fz_Len ;
    float * SuperController ;     int SuperController_Len ;
  } OpFM_InputType_t ;
  typedef struct OpFM_OutputType {
    void * object ;
    float * u ;     int u_Len ;
    float * v ;     int v_Len ;
    float * w ;     int w_Len ;
    float * SuperController ;     int SuperController_Len ;
    float * WriteOutput ;     int WriteOutput_Len ;
  } OpFM_OutputType_t ;
  typedef struct OpFM_UserData {
    OpFM_InitInputType_t           OpFM_InitInput ;
    OpFM_InitOutputType_t          OpFM_InitOutput ;
    OpFM_MiscVarType_t             OpFM_Misc ;
    OpFM_ParameterType_t           OpFM_Param ;
    OpFM_InputType_t               OpFM_Input ;
    OpFM_OutputType_t              OpFM_Output ;
  } OpFM_t ;

#endif // _OpenFOAM_TYPES_H


//!ENDOFREGISTRYGENERATEDFILE
