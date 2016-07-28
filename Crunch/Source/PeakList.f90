MODULE PLSubs

   ! This module contains routines to list peaks.

IMPLICIT NONE

CONTAINS

!=======================================================================
   SUBROUTINE PeakList ( Fi )


      ! This routine finds the peaks and valleys of the data.


   USE                             CrunchIO
   USE                             DataMod
   USE                             ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Fi


      ! Local declarations.

   INTEGER                      :: FIL
   INTEGER                      :: ISC
   INTEGER                      :: NGC
   INTEGER                      :: PLC



      ! Tell the user we are listing peaks and/or valleys.

   CALL WrScr ( '  Listing peaks and/or valleys.' )


      ! Eliminate any selected columns that are constant.

   NGC = 0

   DO ISC=1,NumPLCh

      PLC = PL_Cols(ISC)

      IF ( StdDev(PLC,Fi) /= 0.0 )  THEN

         NGC = NGC + 1

         PL_Cols(NGC) = PLC

      ENDIF

   ENDDO ! ISC


      ! If there are no good columns, exit this routine.

   IF ( NGC .EQ. 0 )  THEN

      CALL USRALARM
      CALL WrScr ( ' All selected PL channels had constant data.  No PL file generated.' )
      RETURN

   ENDIF


      ! Reset the number of valid PL channels.

   NumPLCh = NGC


      ! Create the peak-list file.  Initialize the count array.

   CALL PL_Init ( Fi )


      ! What algorithm will we use to find the peaks and valleys?

   SELECT CASE ( PL_Meth )

   CASE ( 1 )


      IF ( Aggregate )  THEN


            ! Let's process all the data files.

         DO Fil=1,GoodFiles
            CALL PL_Slope ( Fil )
         ENDDO ! Fil

      ELSE  ! Process individual files.

         CALL PL_Slope ( Fi )

      ENDIF ! Aggregate


   CASE ( 2 )


      IF ( Aggregate )  THEN


            ! Let's process all the data files.

         DO Fil=1,GoodFiles
            CALL PL_Thresh ( Fil )
         ENDDO ! Fil

      ELSE  ! Process individual files.

         CALL PL_Thresh ( Fi )

      ENDIF ! Aggregate


   ENDSELECT


      ! If all of the requested input files could not be read for aggregate analyses,
      ! append the list of bad files to the output.

   IF ( Aggregate .AND. ( BadFiles > 0 ) )  CALL WrBadList ( PU )


      ! Close the peak-list file.

   CLOSE ( PU )


   RETURN
   END SUBROUTINE PeakList ! ( Fi )
!=======================================================================
   SUBROUTINE PL_Init ( Fi )


      ! This routine creates the peak-list file and generates their headers.


   USE                             DataMod
   USE                             ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Fi


      ! Local declarations.

   INTEGER(IntKi)               :: IC
   INTEGER(IntKi)               :: IOS
   INTEGER(IntKi)               :: PLC
   INTEGER(IntKi)               :: Sttus

   CHARACTER( 28)               :: Frmt
   CHARACTER(100)               :: PL_File
   CHARACTER( 10)               :: PTStr
   CHARACTER( 17)               :: PTStrH
   CHARACTER( 10)               :: PTUnd
   CHARACTER(200)               :: FormStr



      ! Get date and time for peak-list file.

   DateStr = CurDate()
   TimeStr = CurTime()


      ! Allocate the array of peak-list I/O units if it has not already been allocated.

   IF ( .NOT. ALLOCATED( PUC ) )  THEN
      ALLOCATE ( PUC(NumPLCh) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the peak-list I/O units array.' )
      ENDIF
   ENDIF


      ! Create files for each output channel.

   DO IC=1,NumPLCh


         ! Which column is this?

      PLC = PL_Cols(IC)


         ! Create the name for this peak-list file.

      IF ( Aggregate )  THEN
         PL_File = TRIM( AggRoot )//'_'//TRIM( Titles(PLC ) )//'.pek'
      ELSE
         PL_File = TRIM( RootName(Fi) )//'_'//TRIM( Titles(PLC ) )//'.pek'
      ENDIF


         ! Open the peak-list file.

      CALL GetNewUnit ( PUC(IC) )
      OPEN ( PUC(IC) , FILE=Trim( PL_File ) , STATUS='UNKNOWN' , FORM='FORMATTED' , IOSTAT=IOS )

      IF ( IOS /= 0 )  THEN
         CALL ProgAbort ( ' Warning.  The peak-list file "'//Trim( PL_File )//'" could not be opened.')
      ENDIF


         ! Are we generating peaks, valleys, or both?

      IF ( .NOT. PLWrPk(IC) )  THEN
         PTStrH = 'troughs'
         PTStr  = '   Troughs'
         PTUnd  = '   -------'
      ELSEIF ( .NOT. PLWrTr(IC) )  THEN
         PTStrH = 'peaks'
         PTStr  = '     Peaks'
         PTUnd  = '     -----'
      ELSE
         PTStrH = 'peaks and troughs'
         PTStr  = '   Pks/Trs'
         PTUnd  = '   -------'
      ENDIF


         ! Write out the header.

      IF ( HaveUnits )  THEN
         FormStr = "( / 'These "//TRIM( PTStrH )//" ""' , 2A , '"" were generated by ' , A , A "&
                 //" , ' on ' , A , ' at ' , A , '.' )"
         WRITE (PUC(IC),FormStr)   TRIM( Titles(PLC) ), TRIM( Units(PLC) ), TRIM( ProgName ), TRIM( ProgVer ), DateStr, TimeStr
      ELSE
         FormStr = "( / 'These "//TRIM( PTStrH )//" for ""' , A , '"" were generated by ' , A , A "&
                 //" , ' on ' , A , ' at ' , A , '.' )"
         WRITE (PUC(IC),FormStr)   TRIM( Titles(PLC) ), TRIM( ProgName ), TRIM( ProgVer ), DateStr, TimeStr
      ENDIF

      IF ( Aggregate )  THEN

         IF ( BadFiles == 0 )  THEN
            FormStr = "( / 'These aggregate "//TRIM( PTStrH )//" were based upon ' , A , ' records from ' , A "&
                    //" , ' input files.' )"
            WRITE (PUC(IC),FormStr)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) )
         ELSE
            FormStr = "( / 'These aggregate "//TRIM( PTStrH )//" were based upon ' , A , ' records from ' , A "&
                    //" , ' of the ' , A , ' specified input files.' )"
            WRITE (PUC(IC),FormStr)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) ), TRIM( Int2LStr( NumFiles ) )
         ENDIF

      ELSE
         FormStr = "( / 'These "//TRIM( PTStrH )//" for file ""' , A , '"" were based upon ' , A , ' records.' )"
         WRITE (PUC(IC),FormStr)  TRIM( FileName(Fi) ), TRIM( Int2LStr( AnalRecs ) )
      ENDIF


        ! Let folks know if we used the peak finder.

      IF ( Do_PF )  THEN
        WRITE (PUC(IC),'(A)')  'The peak-finding algorithm was used.'
      ELSE
        WRITE (PUC(IC),'(A)')  'The peak-finding algorithm was not used.'
      ENDIF


         ! Write out mean wind speed and turbulence intensity.

      IF ( WS_Col > 0 )  THEN
         IF ( HaveUnits )  THEN
            WRITE (PUC(IC),'(/,A,F5.1,1X,A,/,A,F5.1,A)')  'Mean wind speed      =', MeanWs(Fi), Units(WS_Col), &
                                                     'Turbulence intensity =', TrbInt(Fi), '%'
         ELSE
            WRITE (PUC(IC),'(/,A,F5.1,/,A,F5.1,A)')  'Mean wind speed      =', MeanWs(Fi), &
                                                     'Turbulence intensity =', TrbInt(Fi), '%'
         ENDIF
      ENDIF


         ! Say what method was used.  Output thresholds if that method is being used.

      IF ( PL_Meth == 1 )  THEN

         WRITE (PUC(IC), '(/A)')  'The change-of-slope method was used.'

      ELSE

         IF ( PLMeanThld(IC) )  THEN
            PLNegThld(IC) = DataMeans(PLC,Fi)
            PLPosThld(IC) = DataMeans(PLC,Fi)
         ENDIF
         IF ( .NOT. PLWrTr(IC) )  THEN
            WRITE (PUC(IC), '(/A)')  'The threshold method was used with a peak threshold of ' &
                                   //TRIM( Flt2LStr( PLPosThld(IC) ) )//'.'
         ELSEIF ( .NOT. PLWrPk(IC) )  THEN
            WRITE (PUC(IC), '(/A)')  'The threshold method was used with a trough threshold of ' &
                                   //TRIM( Flt2LStr( PLNegThld(IC) ) )//'.'
         ELSE
            WRITE (PUC(IC), '(/A)')  'The threshold method was used with thresholds of '//TRIM( Flt2LStr( PLNegThld(IC) ) ) &
                                   //' and '//TRIM( Flt2LStr( PLPosThld(IC) ) )//'.'
         ENDIF

      ENDIF


         ! Write out column heading(s).

      IF ( TabDelim )  THEN
         IF ( WrPLtime )  THEN
            WRITE (PUC(IC),'(/,A)')  'Time'//Tab//PTStr
         ELSE
            WRITE (PUC(IC),'(/,A)')  PTStr
         ENDIF
      ELSE
         IF ( WrPLtime )  THEN
            Frmt = "(2(/,2"//TextFmt//"))"
            WRITE (PUC(IC),Frmt)  '      Time', PTStr , &
                             '      ----', PTUnd
         ELSE
            Frmt = "(2(/,"//TextFmt//"))"
            WRITE (PUC(IC),Frmt)  PTStr, PTUnd
         ENDIF
      ENDIF


   ENDDO ! IC



   RETURN
   END SUBROUTINE PL_Init ! ( Fi )
!=======================================================================
   SUBROUTINE PL_Slope ( Fi )


      ! This routine finds the peaks and valleys of the data using the slope-change method.


   USE                            DataMod
   USE                            ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)         :: Fi


      ! Local declarations.

   REAL                           DX1
   REAL                           DX2
   REAL                           X1
   REAL                           X2

   INTEGER                        IC
   INTEGER                        IR
   INTEGER                        PLC



      ! Find all peaks and valleys using the slope-change method.

   DO IC=1,NumPLCh


         ! Initialize the algorithm.

      PLC = PL_Cols(IC)
      X2  = ConvData(PLC,2,Fi)
      DX1 = X2 - ConvData(PLC,1,Fi)

      DO IR=3,NumRecs

         X1  = X2
         X2  = ConvData(PLC,IR,Fi)
         DX2 = X2 - X1


            ! Check for a change in slope.

         IF ( DX1*DX2 < 0.0 )  THEN


               ! Do we have a peak or valley.  Write out the ones we want.

            IF ( ( DX1 > 0.0 ) .AND. PLWrPk(IC) )  THEN

               IF ( WrPLtime )  THEN
                  WRITE (PUC(IC),PLFrmt)  ConvData(TimeCol,IR-1,Fi), X1
               ELSE
                  WRITE (PUC(IC),PLFrmt)  X1
               ENDIF

            ELSEIF ( ( DX1 < 0.0 ) .AND. PLWrTr(IC) )  THEN

               IF ( WrPLtime )  THEN
                  WRITE (PUC(IC),PLFrmt)  ConvData(TimeCol,IR-1,Fi), X1
               ELSE
                  WRITE (PUC(IC),PLFrmt)  X1
               ENDIF

            ENDIF

            DX1 = DX2

         ENDIF

      ENDDO ! IR

   ENDDO ! IC


   RETURN
   END SUBROUTINE PL_Slope ! ( Fi )
!=======================================================================
   SUBROUTINE PL_Thresh ( Fi )


      ! This routine finds the peaks and valleys of the data using the
      ! peak-between-mean-crossings method.


   USE                                DataMod
   USE                                ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Fi


      ! Local declarations.

   REAL                               Maximum
   REAL                               Minimum
   REAL                               PkTime
   REAL                               TrTime
   REAL                               Val

   INTEGER                            IC
   INTEGER                            IR
   INTEGER                            PLC
   INTEGER                            OldState



      ! Find all peaks and valleys using the threshold method.

   DO IC=1,NumPLCh

      PLC = PL_Cols(IC)


         ! Initialize the algorithm.

      OldState = 1                     ! 0: BelowNegTh, 1: BetweenTh, 2: AbovePosTh


         ! Find the peaks and valleys.

      DO IR=1,NumRecs

         Val = ConvData(PLC,IR,Fi)


            ! Find the current state.

         IF ( Val < PLNegThld(IC) )  THEN


               ! We are below the negative threshold.

            SELECT CASE ( OldState )

            CASE ( 0 )

               IF ( Val < Minimum )  THEN
                  Minimum = Val
                  TrTime  = ConvData(TimeCol,IR,Fi)
               ENDIF

            CASE ( 1 )

               Minimum = Val
               TrTime  = ConvData(TimeCol,IR,Fi)

            CASE ( 2 )

               Minimum = Val
               TrTime  = ConvData(TimeCol,IR,Fi)

               IF ( PLWrPk(IC) )  THEN
                  IF ( WrPLtime )  THEN
                     WRITE (PUC(IC),PLFrmt)  PkTime, Maximum
                  ELSE
                     WRITE (PUC(IC),PLFrmt)  Maximum
                  ENDIF
               ENDIF

            ENDSELECT ! OldState

            OldState = 0

         ELSEIF ( Val > PLPosThld(IC) )  THEN


               ! We are above the positive threshold.

            SELECT CASE ( OldState )

            CASE ( 0 )

               Maximum = Val
               PkTime  = ConvData(TimeCol,IR,Fi)

               IF ( PLWrTr(IC) )  THEN
                  IF ( WrPLtime )  THEN
                     WRITE (PUC(IC),PLFrmt)  TrTime, Minimum
                  ELSE
                     WRITE (PUC(IC),PLFrmt)  Minimum
                  ENDIF
               ENDIF

            CASE ( 1 )

               Maximum = Val
               PkTime  = ConvData(TimeCol,IR,Fi)

            CASE ( 2 )

               IF ( Val > Maximum )  THEN
                  Maximum = Val
                  PkTime  = ConvData(TimeCol,IR,Fi)
               ENDIF

            ENDSELECT ! OldState

            OldState = 2

         ELSE


               ! We are between the thresholds.

            SELECT CASE ( OldState )

            CASE ( 0 )

               IF ( PLWrTr(IC) )  THEN
                  IF ( WrPLtime )  THEN
                     WRITE (PUC(IC),PLFrmt)  TrTime, Minimum
                  ELSE
                     WRITE (PUC(IC),PLFrmt)  Minimum
                  ENDIF
               ENDIF

            CASE ( 2 )

               IF ( PLWrPk(IC) )  THEN
                  IF ( WrPLtime )  THEN
                     WRITE (PUC(IC),PLFrmt)  PkTime, Maximum
                  ELSE
                     WRITE (PUC(IC),PLFrmt)  Maximum
                  ENDIF
               ENDIF

            ENDSELECT ! OldState

            OldState = 1

         ENDIF

      ENDDO ! IR


         ! Output the last maximum or minimum in the time series.

      SELECT CASE ( OldState )

      CASE ( 0 )

      IF ( PLWrTr(IC) )  THEN
         IF ( WrPLtime )  THEN
            WRITE (PUC(IC),PLFrmt)  TrTime, Minimum
         ELSE
            WRITE (PUC(IC),PLFrmt)  Minimum
         ENDIF
      ENDIF

      CASE ( 2 )

      IF ( PLWrPk(IC) )  THEN
         IF ( WrPLtime )  THEN
            WRITE (PUC(IC),PLFrmt)  PkTime, Maximum
         ELSE
            WRITE (PUC(IC),PLFrmt)  Maximum
         ENDIF
      ENDIF

      ENDSELECT ! OldState


   ENDDO ! IC


   RETURN
   END SUBROUTINE PL_Thresh ! ( Fi )
!=======================================================================

END MODULE PLSubs
