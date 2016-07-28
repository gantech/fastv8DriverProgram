MODULE CrunchSubs

   ! This module contains some general, Crunch-related routines.

   USE                                NWTC_Library

   IMPLICIT                           NONE

CONTAINS

!=======================================================================
   SUBROUTINE Alloc


      ! This routine allocates many of the arrays.


   USE                                DataMod
   USE                                ProgGen


      ! Local declarations.

   INTEGER                         :: NF
   INTEGER                         :: Sttus



   !-------------------------------------------------------------------------------
      ! Set the number of files.

   IF ( Aggregate )  THEN
      NF = 1
   ELSE
      NF = NumFiles
   ENDIF


   !-------------------------------------------------------------------------------
      ! Allocate the data arrays.

   IF ( .NOT. FASTbin )  THEN

      ALLOCATE ( RawData(NumInCols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the raw data array.' )
      ENDIF

      ALLOCATE ( ConvData(TotCols,NumRecs,NumFiles) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the main data array.' )
      ENDIF

   END IF


   !-------------------------------------------------------------------------------
      ! Allocate the statistics arrays.

   ALLOCATE ( DataSums(TotCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataSums array.' )
   ENDIF


   ALLOCATE ( DataSum2(TotCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataSum2 array.' )
   ENDIF


   ALLOCATE ( DataSum3(TotCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataSum3 array.' )
   ENDIF


   ALLOCATE ( DataSum4(TotCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataSum4 array.' )
   ENDIF


   ALLOCATE ( DataMaxs(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataMaxs array.' )
   ENDIF


   ALLOCATE ( DataMeans(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataMeans array.' )
   ENDIF


   ALLOCATE ( DataMins(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DataMins array.' )
   ENDIF


   ALLOCATE ( NumCross(TotCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the NumCross array.' )
   ENDIF


   ALLOCATE ( StdDev(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the StdDev array.' )
   ENDIF


   ALLOCATE ( Skewness(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Skewness array.' )
   ENDIF


   ALLOCATE ( Kurtosis(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Kurtosis array.' )
   ENDIF


   ALLOCATE ( XFreq(TotCols,NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the XFreq array.' )
   ENDIF

   ALLOCATE ( MeanWS(NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MeanWS array.' )
   ENDIF


   ALLOCATE ( TrbInt(NF) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the TrbInt array.' )
   ENDIF


   !-------------------------------------------------------------------------------
      ! Allocate the azimuth averages and bin count arrays.

   IF ( NumAACols > 0 )  THEN

      ALLOCATE ( AziAver(NumAACols,NumAABins,NF) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for azimuth averages array.' )
      ENDIF


      ALLOCATE ( AziStDev(NumAACols,NumAABins,NF) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for azimuth standard deviations array.' )
      ENDIF


      ALLOCATE ( AziMean(NumAABins) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for azimuth means array.' )
      ENDIF


      ALLOCATE ( AziSum(NumAABins) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for azimuth sum array.' )
      ENDIF


      ALLOCATE ( Bin_Cnt(NumAABins) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the bin counts array.' )
      ENDIF

   ENDIF


   !-------------------------------------------------------------------------------
      ! Allocate the PMF arrays.

   IF ( NumPMF > 0 )  THEN

      ALLOCATE ( PMFBinVal(NumPMF) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the PMF bin values array.' )
      ENDIF


      ALLOCATE ( PMFCnt(NumPMF,NumPMFBins) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the PMF count array.' )
      ENDIF


      ALLOCATE ( PMFInvDel(NumPMF) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the PMF inverse delta array.' )
      ENDIF


      ALLOCATE ( PMFNorm(NumPMF) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the PMF normalization array.' )
      ENDIF


   ENDIF


   !-------------------------------------------------------------------------------
      ! Allocate the extreme-event arrays.

   IF ( NumEEGrps > 0 )  THEN


      ALLOCATE ( MaxEE(TotCols,TotCols,NumEEGrps) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the MaxEE array.' )
      ENDIF


      ALLOCATE ( MinEE(TotCols,TotCols,NumEEGrps) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the MinEE array.' )
      ENDIF


      IF ( Aggregate )  Then

         ALLOCATE ( MinEEFile(TotCols,NumEEGrps) , STAT=Sttus )

         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the MinEEFile array.' )
         ENDIF

         ALLOCATE ( MaxEEFile(TotCols,NumEEGrps) , STAT=Sttus )

         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the MaxEEFile array.' )
         ENDIF

      ENDIF

   ENDIF


   RETURN
   END SUBROUTINE Alloc
!=======================================================================
   SUBROUTINE PeakFind ( Fi )


      ! The logic in this routine was based upon the GPP Rainflow routine which
      ! was based upon the RF routines in LIFE2.  Paul Veers was probably
      ! responsible for the original logic.  There were bugs in the original
      ! LIFE2 code, so we fixed them here.

      ! This algorithm assumes a constant time step!


   USE                                DataMod
   USE                                ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Fi


      ! Local declarations.

   REAL(ReKi)                      :: DX1
   REAL(ReKi)                      :: DX2
   REAL(ReKi)                      :: DX_Old
   REAL(ReKi)                      :: X1
   REAL(ReKi)                      :: X2

   INTEGER                         :: IC
   INTEGER                         :: IR
   INTEGER                         :: PF_Col



      ! Tell the user why we are here.

   IF ( ( Fi == 1 ) .OR. ( .NOT. Aggregate ) )  CALL WrScr ( '  Finding peaks and valleys.' )


      ! Do this for all the channels.

   DO IC=1,NumPFCols


         ! Initialize the algorithm.

      PF_Col = PF_Cols(IC)
      X2     = ConvData(PF_Col,2,Fi)
      DX1    = X2 - ConvData(PF_Col,1,Fi)
      DX_Old = DX1

      DO IR=3,NumRecs

         X1  = X2
         X2  = ConvData(PF_Col,IR,Fi)
         DX2 = X2 - X1


            ! Check for a change in slope.

         IF ( DX_Old*DX2 < 0 )  THEN


               ! Calculate the three-point parabolic interpolation.  Save delta.

            ConvData(PF_Col,IR-1,Fi) = X1 - 0.125*( DX1 + DX2 )**2/( DX2 - DX1 )

            DX_Old = DX2

         ENDIF


            ! Update delta.

         DX1 = DX2

      ENDDO ! IR

   ENDDO ! IC


   RETURN
   END SUBROUTINE PeakFind ! ( Fi )
!=======================================================================
   SUBROUTINE RemoveXT ( Data1 , Data2 , XT )


      ! This routine does a matrix multiplication to remove crosstalk.  It
      ! replaces the incoming data with the results.


      ! Argument declarations.

   REAL(ReKi), INTENT(INOUT)       :: Data1
   REAL(ReKi), INTENT(INOUT)       :: Data2
   REAL(ReKi), INTENT(IN)          :: XT      (2,2)


      ! Local declarations.

   REAL(ReKi)                      :: DataVec (2)



   DataVec(1) = Data1
   DataVec(2) = Data2

   DataVec = MATMUL( XT , DataVec )

   Data1 = DataVec(1)
   Data2 = DataVec(2)


   RETURN
   END SUBROUTINE RemoveXT ! ( Data1 , Data2 , XT )
!=======================================================================

END MODULE CrunchSubs
