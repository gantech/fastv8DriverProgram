MODULE AASubs

   ! This module contains the routines needed to compute azimuth averages.

IMPLICIT NONE

CONTAINS

!=======================================================================
   SUBROUTINE AddAAcols


      ! This routine creates the column titles and units for the new
      ! azimuth-average channels.


   USE                             DataMod
   USE                             ProgGen


      ! Local declarations.

   INTEGER                      :: IC
   INTEGER                      :: NewCol
   INTEGER                      :: Sttus

   CHARACTER( 10), ALLOCATABLE  :: TmpTitles    (:)
   CHARACTER( 10), ALLOCATABLE  :: TmpUnits     (:)



   TotCols = NonAACols + 2*NumAACols


      ! Reallocate arrays for titles and units to make room for AA columns.

   ALLOCATE ( TmpTitles(NonAACols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for TmpTitles array.' )
   ENDIF


   IF ( HaveUnits )  THEN

      ALLOCATE ( TmpUnits(NonAACols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for TmpUnits array.' )
      ENDIF

   ENDIF


   DO IC=1,NonAACols

      TmpTitles(IC) = Titles(IC)

      IF ( HaveUnits )  TmpUnits (IC) = Units (IC)

   ENDDO ! IC


   DEALLOCATE ( Titles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for Titles array.' )
   ENDIF

   ALLOCATE ( Titles(TotCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for enlarged Titles array.' )
   ENDIF


   IF ( HaveUnits )  THEN

      DEALLOCATE ( Units , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for Units array.' )
      ENDIF

      ALLOCATE ( Units(TotCols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for enlarged Units array.' )
      ENDIF

   ENDIF

   DO IC=1,NonAACols

      Titles(IC) = TmpTitles(IC)

      IF ( HaveUnits )  Units (IC) = TmpUnits (IC)

   ENDDO ! IC


   DO IC=1,NumAACols

      IF ( ( AA_Cols(IC) < 1 ) .OR. ( AA_Cols(IC) > TotCols ) )  THEN
         CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' AA column must be between 1 and ' &
                    //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
      ENDIF

      NewCol = NonAACols + IC

      Titles(NewCol) = Titles(AA_Cols(IC))

      IF ( HaveUnits )  Units (NewCol) = Units (AA_Cols(IC))

      Titles(NewCol+NumAACols) = Titles(AA_Cols(IC))

      IF ( HaveUnits )  Units (NewCol+NumAACols) = Units (AA_Cols(IC))

   ENDDO ! IC


   RETURN
   END SUBROUTINE AddAAcols
!=======================================================================
   FUNCTION AzimAver( Fi , IC , IB_Lo , AzimuthS )


      ! This function returns the azimuth-average value for the ICth column at
      ! the current Azimuth.


   USE                                DataMod


      ! Argument declarations.

   REAL(ReKi), INTENT(IN)          :: AzimuthS

   INTEGER, INTENT(IN)             :: Fi
   INTEGER, INTENT(IN)             :: IB_Lo
   INTEGER, INTENT(IN)             :: IC


      ! Local declarations.

   REAL(ReKi)                      :: AzimAver
   REAL(ReKi)                      :: AzimLow

   INTEGER                         :: IB_Hi



      ! Find the next bin.

   IF (IB_Lo == NumAABins) THEN
      IB_Hi = 1
   ELSE
      IB_Hi = IB_Lo + 1
   ENDIF

      ! Interpolate the azimuth averages for the correct value.

   AzimLow  = ( IB_Lo - 1 )*DelAzim
   AzimAver = ( AziAver(IC,IB_Hi,Fi) - AziAver(IC,IB_Lo,Fi) )*( AzimuthS - AzimLow )/DelAzim + AziAver(IC,IB_Lo,Fi)


   RETURN
   END FUNCTION AzimAver ! ( Fi , IC , IR , IB_Lo , AzimuthS )
!=======================================================================
   FUNCTION AzimBin( Fi , IR , AzimuthS )


      ! This function returns the azimuth bin for the current Azimuth.


   USE                                DataMod


      ! Argument declarations.

   REAL(ReKi), INTENT(OUT)         :: AzimuthS

   INTEGER, INTENT(IN)             :: Fi
   INTEGER, INTENT(IN)             :: IR


      ! Local declarations.

   INTEGER                         :: AzimBin



      ! Find the azimuth.  Shift it by half the bin width.

   AzimuthS = ConvData(AzimCol,IR,Fi) - DelAzim2

   IF ( AzimuthS .LT. 0 )  THEN
      AzimuthS = 360.0 - AMOD( -AzimuthS , 360.0 )
      IF ( AzimuthS .EQ. 360.0 )  AzimuthS = 0.0
   ELSE
      AzimuthS = AMOD( AzimuthS , 360.0 )
   ENDIF


      ! Find the nearest bin.

   AzimBin = INT( AzimuthS*NumAABins/360.0 ) + 1


   RETURN
   END FUNCTION AzimBin ! ( Fi , IR , AzimuthS )
!=======================================================================
   SUBROUTINE AzimCalc ( Fi )


      ! This routine calculates the azimuth averages and standard deviations
      !  of all requested channels.


   USE                                DataMod
   USE                                ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Fi


      ! Local declarations.

   REAL(DbKi), ALLOCATABLE         :: AverSum  (:,:)
   REAL(DbKi), ALLOCATABLE         :: StDevSum (:,:)

   REAL(DbKi)                      :: Delta

   REAL(ReKi)                      :: Azimuth

   INTEGER                         :: Fil
   INTEGER                         :: IB
   INTEGER                         :: IC
   INTEGER                         :: IFi
   INTEGER                         :: IR
   INTEGER                         :: Sttus



      ! Let folks know what we're doing.

   CALL WrScr ( '  Computing azimuth averages.' )


      ! Allocate some array space.

   ALLOCATE ( AverSum(NumAACols, NumAABins) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the azimuth averaging AverSum array.' )
   ENDIF

   ALLOCATE ( StDevSum(NumAACols, NumAABins) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the azimuth averaging StDevSum array.' )
   ENDIF

      ! Initialize the arrays.

   IF ( Fi == 0 )  THEN
      IFi = 1
   ELSE
      IFi = Fi
   ENDIF

   DO IB=1,NumAABins

       Bin_Cnt (IB) = 0
       AziSum  (IB) = 0.0d0

      DO IC=1,NumAACols
         AverSum (IC,IB)     = 0.0d0
         StDevSum(IC,IB)     = 0.0d0
         AziAver (IC,IB,IFi) = 0.0
         AziStDev(IC,IB,IFi) = 0.0
      ENDDO ! IC

   ENDDO ! IB


      ! Scan through the data.  Determine the appropriate bin.
      ! Increment counts for that bin.  Add data values to the
      ! appropriate bin sums.

   IF ( Aggregate )  THEN

      DO Fil=1,GoodFiles

         DO IR=1,NumRecs

            IF ( ConvData(AzimCol,IR,Fil) .LT. 0 )  THEN
               Azimuth = 360.0 - AMOD( -ConvData(AzimCol,IR,Fil) , 360.0 )
               IF ( Azimuth .EQ. 360.0 )  Azimuth = 0.0
            ELSE
               Azimuth = AMOD( ConvData(AzimCol,IR,Fil) , 360.0 )
            ENDIF

            IB = INT( Azimuth*NumAABins/360.0 ) + 1

            Bin_Cnt(IB) = Bin_Cnt(IB) + 1
            AziSum (IB) = AziSum (IB) + Azimuth

            DO IC=1,NumAACols

               AverSum(IC,IB) = AverSum(IC,IB) + ConvData(AA_Cols(IC),IR,Fil)

            ENDDO ! IC

         ENDDO ! IR

      ENDDO ! Fil

   ELSE

      DO IR=1,NumRecs

         IF ( ConvData(AzimCol,IR,Fi) .LT. 0 )  THEN
            Azimuth = 360.0 - AMOD( -ConvData(AzimCol,IR,Fi) , 360.0 )
            IF ( Azimuth .EQ. 360.0 )  Azimuth = 0.0
         ELSE
            Azimuth = AMOD( ConvData(AzimCol,IR,Fi) , 360.0 )
         ENDIF

         IB = INT( Azimuth*NumAABins/360.0 ) + 1

         Bin_Cnt(IB) = Bin_Cnt(IB) + 1
         AziSum (IB) = AziSum (IB) + Azimuth

         DO IC=1,NumAACols

            AverSum(IC,IB) = AverSum(IC,IB) + ConvData(AA_Cols(IC),IR,Fi)

         ENDDO ! IC

      ENDDO ! IR

   ENDIF


      ! Compute bin averages.

   DO IB=1,NumAABins

      IF ( Bin_Cnt(IB) .GT. 0 )  THEN

         AziMean(IB) = AziSum(IB)/Bin_Cnt(IB)

         DO IC=1,NumAACols
              AziAver(IC,IB,IFi) = AverSum(IC,IB)/Bin_Cnt(IB)
         ENDDO ! IC

      ELSE

         AziMean(IB) = ( IB - 0.5 )*360.0/NumAABins

         DO IC=1,NumAACols
            AziAver(IC,IB,IFi) = 0.0
         ENDDO ! IC

      ENDIF

   ENDDO ! IB

      ! Do sums for standard deviation.

   IF ( Aggregate )  THEN

      DO Fil=1,GoodFiles

         DO IR=1,NumRecs

            IF ( ConvData(AzimCol,IR,Fil) .LT. 0 )  THEN
               Azimuth = 360.0 - AMOD( -ConvData(AzimCol,IR,Fil) , 360.0 )
               IF ( Azimuth .EQ. 360.0 )  Azimuth = 0.0
            ELSE
               Azimuth = AMOD( ConvData(AzimCol,IR,Fil) , 360.0 )
            ENDIF

            IB = INT( Azimuth*NumAABins/360.0 ) + 1

            DO IC=1,NumAACols

              Delta           = ConvData(AA_Cols(IC),IR,Fil) -  AziAver(IC,IB,IFi)
              StDevSum(IC,IB) = StDevSum(IC,IB) + Delta*Delta

            ENDDO ! IC

         ENDDO ! IR

      ENDDO ! Fil

   ELSE

      DO IR=1,NumRecs

         IF ( ConvData(AzimCol,IR,Fi) .LT. 0 )  THEN
            Azimuth = 360.0 - AMOD( -ConvData(AzimCol,IR,Fi) , 360.0 )
            IF ( Azimuth .EQ. 360.0 )  Azimuth = 0.0
         ELSE
            Azimuth = AMOD( ConvData(AzimCol,IR,Fi) , 360.0 )
         ENDIF

         IB = INT( Azimuth*NumAABins/360.0 ) + 1

         DO IC=1,NumAACols

           Delta           = ConvData(AA_Cols(IC),IR,Fi) -  AziAver(IC,IB,IFi)
           StDevSum(IC,IB) = StDevSum(IC,IB) + Delta*Delta

         ENDDO ! IC

      ENDDO ! IR

   ENDIF

      ! Compute standard deviations.

   DO IB=1,NumAABins

      IF ( Bin_Cnt(IB) .GT. 1 )  THEN

         DO IC=1,NumAACols
            AziStDev(IC,IB,IFi) = DSQRT( StDevSum(IC,IB)/(Bin_Cnt(IB) - 1.0) )
         ENDDO ! IC

      ELSE

         DO IC=1,NumAACols
            AziStDev(IC,IB,IFi) = 0.0
         ENDDO ! IC

      ENDIF

   ENDDO ! IB

   DEALLOCATE ( AverSum  )
   DEALLOCATE ( StDevSum )

   RETURN
   END SUBROUTINE AzimCalc ! ( Fi )
!=======================================================================
   SUBROUTINE Write_AA ( Fi )


      ! This routine creates the azimuth averages file.


   USE                             CrunchIO
   USE                             DataMod
   USE                             ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Fi


      ! Local declarations.

   INTEGER                      :: Fil
   INTEGER                      :: IB
   INTEGER                      :: IC
   INTEGER                      :: IOS

   CHARACTER(100)               :: AA_File
   CHARACTER(200)               :: Frmt



      ! Get date and time for azimuth averages file.

   DateStr = CurDate()
   TimeStr = CurTime()


      ! Create the name for the azimuth averages file.

   IF ( Aggregate )  THEN
      Fil     = 1
      AA_File = TRIM( AggRoot )//'.azi'
   ELSE
      Fil     = Fi
      AA_File = TRIM( RootName(Fi) )//'.azi'
   ENDIF


      ! Open the azimuth averages file.

   CALL GetNewUnit ( AU )
   OPEN ( AU , FILE=Trim( AA_File ) , STATUS='UNKNOWN' , FORM='FORMATTED' , IOSTAT=IOS )

   IF ( IOS /= 0 )  THEN
      CALL ProgAbort ( ' Warning.  The azimuth averages file "'//Trim( AA_File )//'" could not be opened.')
   ENDIF


      ! Write the appropriate header for aggregate or individual azimuth averages.

   Frmt = "( / 'Azimuth averages generated by ' , A , A , ' on ' , A , ' at ' , A , '.' )"
   WRITE (AU,Frmt)  TRIM( ProgName ), TRIM( ProgVer ), DateStr, TimeStr

   IF ( Aggregate )  THEN
      IF ( BadFiles == 0 )  THEN
         Frmt = "( / 'These aggregate azimuth averages were based upon ' , A , ' records from ' , A" &
               //" , ' input files.' )"
         WRITE (AU,Frmt)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) )
      ELSE
         Frmt = "( / 'These aggregate azimuth averages were based upon ' , A , ' records from ' , A" &
               //" , ' of the ' , A , ' specified input files.' )"
         WRITE (AU,Frmt)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) ), TRIM( Int2LStr( NumFiles ) )
      ENDIF
   ELSE
      Frmt = "( / 'These azimuth averages for ""' , A , '"" were based upon ' , A , ' records.' )"
      WRITE (AU,Frmt)  TRIM( FileName(Fi) ), TRIM( Int2LStr( AnalRecs ) )
   ENDIF


      ! Write out mean wind speed and turbulence intensity.

   IF ( WS_Col > 0 )  THEN
      IF ( HaveUnits )  THEN
         WRITE (AU,'(/,A,F5.1,1X,A,/,A,F5.1,A)')  'Mean wind speed      =' , MeanWs(Fil) , Units(WS_Col) , &
                                                  'Turbulence intensity =' , TrbInt(Fil) , '%'
      ELSE
         WRITE (AU,'(/,A,F5.1,/,A,F5.1,A)')  'Mean wind speed      =' , MeanWs(Fil) , &
                                             'Turbulence intensity =' , TrbInt(Fil) , '%'
      ENDIF
   ENDIF


      ! Write the column headings.

   IF ( TabDelim )  THEN
      Frmt = "(/,'Azimuth',   (:,'"//Tab//"',A))"
      WRITE (Frmt(14:16),'(I3)')  NumAACols
      WRITE (AU,Frmt)  ( TRIM( Titles(AA_Cols(IC)) ), IC=1,NumAACols )
      IF ( HaveUnits )  THEN
         Frmt = "('(deg)',   (:,'"//Tab//"',A))"
         WRITE (Frmt(10:12),'(I3)')  NumAACols
         WRITE (AU,Frmt)  ( TRIM( Units (AA_Cols(IC)) ), IC=1,NumAACols )
      ENDIF
   ELSE
      WRITE (AU,'()')
      Frmt = "(   "//TextFmt//")"
      WRITE (Frmt(2:4),'(I3)')  NumAACols + 1
      WRITE (AU,Frmt)  '   Azimuth', ( ADJUSTR( TRIM( Titles(AA_Cols(IC)) ) ), IC=1,NumAACols )
      IF ( HaveUnits )  THEN
         WRITE (AU,Frmt)  '     (deg)', ( ADJUSTR( TRIM( Units(AA_Cols(IC)) ) ), IC=1,NumAACols )
      ENDIF
      WRITE (Frmt(2:4),'(I3)')  NumAACols + 1
      WRITE (AU,Frmt)  ( '----------', IC=1,NumAACols+1 )
   ENDIF


      ! Create the output format.

   IF ( TabDelim )  THEN
      Frmt = "(   ("//RealFmt//",:,'"//Tab//"'))"
   ELSE
      Frmt = "(   ("//RealFmt//"))"
   ENDIF

   WRITE (Frmt(2:4),'(I3)')  NumAACols + 1


      ! Write out the azimuth averages.

   DO IB=1,NumAABins

      WRITE (AU,Frmt)  AziMean(IB), ( AziAver(IC,IB,Fil), IC=1,NumAACols )

   ENDDO ! IB


      ! If all of the requested input files could not be read for aggregate analyses,
      ! append the list of bad files to the output.

   IF ( Aggregate .AND. ( BadFiles > 0 ) )  CALL WrBadList ( AU )


      ! Close the azimuth averages file.

   CLOSE ( AU )


   RETURN
   END SUBROUTINE Write_AA ! ( Fi )
!=======================================================================

END MODULE AASubs
