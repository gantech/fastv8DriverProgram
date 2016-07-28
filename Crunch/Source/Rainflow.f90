MODULE RFSubs

   ! This module contains routines to calculate extreme events.

IMPLICIT NONE

CONTAINS

!=======================================================================
   SUBROUTINE BinCount1 ( NCyc, IC )

   !    This subroutine does the 1-D binning of the cycle-counted data.
   !    It bins the cycles over their min-to-max range for each column.


   USE                                DataMod


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: IC
   INTEGER, INTENT(IN)             :: NCyc


      ! Local declarations.

   INTEGER                         :: IBR
   INTEGER                         :: IR



      ! Zero out the RF_Counts array for this column.

   DO IBR=1,NumRFRBins
      RF_Counts(IC,IBR,1) = 0
   ENDDO ! IBR


      ! Find the maximum range if automatic.

   IF ( AutoRange(IC) )  THEN

      MaxRng(IC) = Smallest

      DO IR=1,NCyc
         IF ( MaxRng(IC) < CycRange(IR) )  MaxRng(IC) = CycRange(IR)
      ENDDO ! IR

   ENDIF


      ! Loop through the data.  Determine the appropriate bin.
      ! Increment counts for that bin.

   DO IR=1,NCyc

      IBR = MIN0( INT( NumRFRBins*CycRange(IR)/MaxRng(IC) ) + 1 , NumRFRBins )

      RF_Counts(IC,IBR,1) = RF_Counts(IC,IBR,1) + CycMult(IR)

   ENDDO ! IR


   RETURN
   END SUBROUTINE BinCount1 ! ( NCyc, IC )
!=======================================================================
   SUBROUTINE BinCount2 ( NCyc, IC )

   !    This subroutine does the 2-D binning of the cycle-counted data.  It
   !    bins the cycles over their min-to-max range for each column.


   USE                                DataMod


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: IC
   INTEGER, INTENT(IN)             :: NCyc


      ! Local declarations.

   REAL(ReKi)                      :: InvDelta
   REAL(ReKi)                      :: InvMaxR

   INTEGER                         :: IBM
   INTEGER                         :: IBR
   INTEGER                         :: IR



      ! Zero out the RF_Counts array for this column.

   DO IBR=1,NumRFRBins
      DO IBM=1,NumRFMBins
         RF_Counts(IC,IBR,IBM) = 0
      ENDDO ! IBM
   ENDDO ! IBR


      ! Find the maximum range if automatic.

   IF ( AutoRange(IC) )  THEN

      MaxRng(IC) = Smallest

      DO IR=1,NCyc
         IF ( MaxRng(IC) < CycRange(IR) )  MaxRng(IC) = CycRange(IR)
      ENDDO ! IR

   ENDIF

   InvMaxR = 1/MaxRng(IC)


      ! Find the smallest and largest cycle mean values if automatic.

   IF ( AutoMeans(IC) )  THEN

      MinMean(IC) = Biggest
      MaxMean(IC) = Smallest

      DO IR=1,NCyc
         IF ( CycMean(IR) > MaxMean(IC) )  MaxMean(IC) = CycMean(IR)
         IF ( CycMean(IR) < MinMean(IC) )  MinMean(IC) = CycMean(IR)
      ENDDO

   ENDIF

   InvDelta = 1.0/( MaxMean(IC) - MinMean(IC) )


      ! Loop through the data.  Determine the appropriate bin.
      ! Increment counts for that bin.

   DO IR=1,NCyc

      IBR = MAX( MIN( INT( NumRFRBins*CycRange(IR)*InvMaxR )+ 1 , NumRFRBins ), 1 )
      IBM = MAX( MIN( INT( NumRFMBins*InvDelta*( CycMean(IR) - MinMean(IC) ) ) + 1 , NumRFMBins ), 1 )

      RF_Counts(IC,IBR,IBM) = RF_Counts(IC,IBR,IBM) + CycMult (IR)

   ENDDO ! IR


   RETURN
   END SUBROUTINE BinCount2 ! ( NCyc, IC )
!=======================================================================
   SUBROUTINE Rainflow ( Fi )


      !  This routine performs a one-pass rainflow counting of the
      !  primary data.  It then takes the counts and bins them over
      !  the range from the min to the max.  It employs rainflow
      !  counting routines derived from the LIFE2 software
      !  developed by Sandia National Labs.


   USE                             CrunchIO
   USE                             DataMod
   USE                             ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Fi


      ! Local declarations.

   REAL(ReKi)                   :: CR_Val
   REAL(ReKi)                   :: DelTime
   REAL(ReKi)                   :: TimeUnit

   INTEGER                      :: IBM
   INTEGER                      :: IBR
   INTEGER                      :: IC
   INTEGER                      :: IR
   INTEGER                      :: ISC
   INTEGER                      :: NGC
   INTEGER                      :: RFC
   INTEGER                      :: Sttus

   CHARACTER( 30), ALLOCATABLE  :: CR_Str    (:)
   CHARACTER( 30)               :: Fmt
   CHARACTER(200)               :: Frmt
   CHARACTER( 30)               :: PeriodStr



      ! Tell the user we are computing Rainflow Cycle Counts.

   CALL WrScr ( '  Computing rainflow cycle counts.' )


      ! Eliminate any selected columns that are constant.

   NGC = 0

   DO ISC=1,NumRFCols

      IC = RF_Cols(ISC)

      IF ( StdDev(IC,Fi) /= 0.0 )  THEN

         NGC = NGC + 1


            ! If NGC/=ISC (constant channel found), we'll have to shift.

         RF_Cols  (NGC) = IC
         MaxMean  (NGC) = MaxMean  (ISC)
         MaxRng   (NGC) = MaxRng   (ISC)
         MinMean  (NGC) = MinMean  (ISC)
         AutoRange(NGC) = AutoRange(ISC)
         AutoMeans(NGC) = AutoMeans(ISC)

!mlb   ELSE
!mlb      Print warning about the column being skipped.

      ENDIF

   ENDDO ! ISC


      ! If there are no good columns, exit this routine.

   IF ( NGC .EQ. 0 )  THEN

      CALL USRALARM
      CALL WrScr ( ' All selected RF columns had constant data.  No RF file generated.' )
      RETURN

   ENDIF


      ! Reset the number of valid RF columns.

   NumRFCols = NGC


      ! Create the rainflow file.  Initialize the count array.

   CALL RF_Init ( Fi , PeriodStr )


      ! Loop through the columns.  Cycle count, then bin the cycles.

   DO IC=1,NumRFCols

      RFC = RF_Cols(IC)


         ! Call Sandia rainflow routines.

      CALL SRain ( Fi , RFC , RF_Cycles(IC), HalfCycMult(IC) )

         ! Shall we bin the cycles?

      IF ( NumRFRBins > 0 )  THEN


            ! Do 1-D or 2-D binning?

         IF ( NumRFMBins == 1 )  THEN
            CALL BinCount1 ( RF_Cycles(IC) , IC )
         ELSE
            CALL BinCount2 ( RF_Cycles(IC) , IC )
         ENDIF

      ELSE


            ! We are not binning.  Let's output the cycles themselves.

         DO IR=1,RF_Cycles(IC)

            IF ( TabDelim )  THEN
               IF ( NumRFMBins == 1 )  THEN
                  Frmt = "(2("//RealFmt//"))"
                  WRITE (RUC(IC),Frmt)  CycMult(IR), CycRange(IR)
               ELSE
                  Frmt = "("//RealFmt//",'"//Tab//"',"//RealFmt//",'"//Tab//"',"//RealFmt//")"
                  WRITE (RUC(IC),Frmt)  CycMult(IR), CycRange(IR) , CycMean(IR)
               ENDIF
            ELSE
               IF ( NumRFMBins == 1 )  THEN
                  Frmt = "(2("//RealFmt//"))"
                  WRITE (RUC(IC),Frmt)  CycMult(IR), CycRange(IR)
               ELSE
                  Frmt = "(3("//RealFmt//"))"
                  WRITE (RUC(IC),Frmt)  CycMult(IR), CycRange(IR) , CycMean(IR)
               ENDIF
            ENDIF

         ENDDO ! IR


            ! If all of the requested input files could not be read for aggregate analyses,
            ! append the list of bad files to the output.

         IF ( Aggregate .AND. ( BadFiles > 0 ) )  CALL WrBadList ( RUC(IC) )


            ! Close the output file.

         CLOSE ( RUC(IC) )

      ENDIF ! ( NumRFRBins = 0 )

   ENDDO ! IC


      ! Shall we bin the cycles?

   IF ( NumRFRBins > 0 )  THEN


         ! Normalize the binned cycle counts with run time and rainflow
         ! count period.

      IF ( Fi == 0 )  THEN
         DelTime = ( AnalRecs - 1 )*( ConvData(TimeCol,2,1 ) - ConvData(TimeCol,1,1 ) )
      ELSE
         DelTime = ( AnalRecs - 1 )*( ConvData(TimeCol,2,Fi) - ConvData(TimeCol,1,Fi) )
      ENDIF

      TimeUnit = RF_Per/DelTime


         ! Are we doing 1-D or 2-D bins?

      IF ( NumRFMBins == 1 )  THEN


               ! Add first part of normalization info to the header.

            IF ( RF_Norm )  THEN
               WRITE (RU,'(/,A)') 'To normalize the count rates so they are comparable to others that used'
               WRITE (RU,'(  A)') 'different bin widths, the rates were divided by the following factors:'
            ENDIF

            ! Let's do 1-D binning.

         DO IC=1,NumRFCols


               ! Compute the range bin width.

            RngBinWid(IC) = MaxRng(IC)/NumRFRBins


               ! Write out normalization bin width, if appropriate.

            IF ( RF_Norm )  THEN

               IF ( TabDelim )  THEN
                  IF ( HaveUnits )  THEN
                     WRITE (RU,'(A)')  Tab//TRIM( Titles(RF_Cols(IC)) )//Tab//TRIM( Flt2LStr( RngBinWid(IC) ) )// &
                                       Tab//TRIM( Units (RF_Cols(IC)) )
                  ELSE
                     WRITE (RU,'(A)')  Tab//TRIM( Titles(RF_Cols(IC)) )//Tab//TRIM( Flt2LStr( RngBinWid(IC) ) )
                  ENDIF
               ELSE
                  IF ( HaveUnits )  THEN
                     WRITE (RU,'(A)')  '  '//Titles(RF_Cols(IC))//' = '//Flt2LStr( RngBinWid(IC) )//' '//TRIM( Units(RF_Cols(IC)) )
                  ELSE
                     WRITE (RU,'(A)')  '  '//Titles(RF_Cols(IC))//' = '//Flt2LStr( RngBinWid(IC) )
                  ENDIF
               ENDIF

            ENDIF

         ENDDO ! IC


            ! Write the column headings.

         IF ( TabDelim )  THEN

            WRITE (RU,'()')
            Frmt = "(   (A,'[X]"//Tab//"',A,'[Y]"//Tab//"'))"
            WRITE (Frmt(2:4),'(I3)')  NumRFCols

            WRITE (RU,Frmt)  ( TRIM( Titles(RF_Cols(IC)) ), TRIM( Titles(RF_Cols(IC)) ), IC=1,NumRFCols )

            Frmt = "(   (A,'"//Tab//"',A,'"//Tab//"'))"
            WRITE (Frmt(2:4),'(I3)')  NumRFCols

            IF ( HaveUnits )  THEN
               WRITE (RU,Frmt)  ( TRIM( Units(RF_Cols(IC)) ), '(cyc/time)', IC=1,NumRFCols )
            ELSE
               WRITE (RU,Frmt)  ( '(?)', '(cyc/time)', IC=1,NumRFCols )
            ENDIF

         ELSE

            WRITE (RU,'()')
            Frmt = "(   "//TextFmt//")"
            WRITE (Frmt(2:4),'(I3)')  2*NumRFCols

            WRITE (RU,Frmt)  ( ADJUSTR( Titles(RF_Cols(IC)) ), &
                               ADJUSTR( Titles(RF_Cols(IC)) ), IC=1,NumRFCols )

            IF ( HaveUnits )  THEN
               WRITE (RU,Frmt)  ( ADJUSTR( Units(RF_Cols(IC)) ), '(cyc/time)', IC=1,NumRFCols )
            ELSE
               WRITE (RU,Frmt)  ( '       (?)', '(cyc/time)', IC=1,NumRFCols )
            ENDIF

            WRITE (RU,Frmt)  ( '----------', '----------', IC=1,NumRFCols )

         ENDIF


            ! Generate a format for the 1-D data output.

         IF ( TabDelim )  THEN


               ! Allocate an array to hold the string values of the count rates.

            ALLOCATE ( CR_Str(NumRFCols) , STAT=Sttus )

            IF ( Sttus /= 0 )  THEN
               CALL ProgAbort ( ' Error allocating memory for the rainflow CR_Str array.' )
            ENDIF

            Frmt = "(    ("//RealFmt//",'"//Tab//"',A))"

         ELSE

            Frmt = "(    (2("//RealFmt//")))"

         ENDIF

         WRITE (Frmt(2:5),'(I4)')  NumRFCols


            ! Output the results.

         DO IBR=1,NumRFRBins


               ! Create the strings of the count rates so that when we have zeros, are
               ! writing to tab-delimited files, and if the user asked for them, we
               ! output a space instead of a zero.  This will make it easier to plot on
               ! log scales.

            DO IC=1,NumRFCols

               IF ( TabDelim )  THEN


                     ! Output the appropriate string depending on whether or not there are counts in this bin.

                  IF ( RFZC_Blank .AND. ( RF_Counts(IC,IBR,1) < 0.000001 ) )  THEN

                     CR_Str(IC) = Tab

                  ELSE

                        ! Divide by bin width if we are normalizing.

                     IF ( RF_Norm ) THEN
                        CR_Val = RF_Counts(IC,IBR,1)*TimeUnit/RngBinWid(IC)
                     ELSE
                        CR_Val = RF_Counts(IC,IBR,1)*TimeUnit
                     ENDIF


                        ! We don't need a tab at the end of the line.

                     IF ( IC == NumRFCols )  THEN
                        Fmt = "("//RealFmt//")"
                     ELSE
                        Fmt = "("//RealFmt//",'"//Tab//"')"
                     ENDIF

                     WRITE(CR_Str(IC),Fmt)  CR_Val


                        ! Shift the string to the left.

                     CR_Str(IC) = ADJUSTL( CR_Str(IC) )

                  ENDIF ! RF_Counts(IC,IBR,1) == 0

               ENDIF ! TabDelim

            ENDDO ! IC

            IF ( TabDelim )  THEN
               WRITE (RU,Frmt)  ( (IBR-0.5)*RngBinWid(IC), TRIM( CR_Str(IC) ), IC=1,NumRFCols )
            ELSE
               IF ( RF_Norm )  THEN
                  WRITE (RU,Frmt)  ( (IBR-0.5)*RngBinWid(IC), RF_Counts(IC,IBR,1)*TimeUnit/RngBinWid(IC), IC=1,NumRFCols )
               ELSE
                  WRITE (RU,Frmt)  ( (IBR-0.5)*RngBinWid(IC), RF_Counts(IC,IBR,1)*TimeUnit, IC=1,NumRFCols )
               ENDIF
            ENDIF

         ENDDO ! IBR


      ELSE


            ! Let's output a header and matrix for each column.

         DO IC=1,NumRFCols


            RFC = RF_Cols(IC)


               ! Explain the matrices

            WRITE (RU,'(//,A)')  '=========================================================================='
            IF ( HaveUnits )  THEN
               WRITE (RU,'( /,A)')  'For '//TRIM( Titles(RFC) )//' '//TRIM( Units(RFC) )//':'
            ELSE
               WRITE (RU,'( /,A)')  'For '//TRIM( Titles(RFC) )//':'
            ENDIF
            WRITE (RU,'(   A)')  ' Row values are the cycle peak-to-peak magnitudes.'
            WRITE (RU,'(   A)')  ' Column values are the cycle means.'
            WRITE (RU,'(   A)')  ' Table values are the rainflow cycles per '//TRIM( PeriodStr )//'.'


               ! Calculate the range and mean bin widths.

            RngBinWid (IC) = MaxRng(IC)/NumRFRBins
            MeanBinWid(IC) = ( MaxMean(IC) - MinMean(IC) )/NumRFMBins


               ! Write out normalization bin area, if appropriate.

            IF ( RF_Norm )  THEN

               IF ( TabDelim )  THEN
                  WRITE (RU,'(/,A)')  ' To normalize the counts so they are comparable to others with different'
                  WRITE (RU,'(  A)')  ' bin areas, the counts were divided by:'
                  IF ( HaveUnits )  THEN
                     WRITE (RU,'(A)')  Tab//TRIM( Flt2LStr( RngBinWid(IC)*MeanBinWid(IC) ) )//Tab//TRIM( Units(RFC) )//'^2'
                  ELSE
                     WRITE (RU,'(A)')  Tab//TRIM( Flt2LStr( RngBinWid(IC)*MeanBinWid(IC) ) )
                  ENDIF
               ELSE
                  WRITE (RU,'(/,A)')  ' To normalize the counts so they are comparable to others with different'
                  IF ( HaveUnits )  THEN
                     WRITE (RU,'(A)')  ' bin areas, the counts were divided by '// &
                                         TRIM( Flt2LStr( RngBinWid(IC)*MeanBinWid(IC) ) )//' '// &
                                         TRIM( Units(RFC) )//'^2.'
                  ELSE
                     WRITE (RU,'(A)')  ' bin areas, the counts were divided by '// &
                                         TRIM( Flt2LStr( RngBinWid(IC)*MeanBinWid(IC) ) )
                  ENDIF
               ENDIF

            ENDIF


               ! Write the column headings.

            IF ( TabDelim )  THEN
               Frmt = "(/,"//TextFmt//",   ('"//Tab//"',"//RealFmt//"))"
               WRITE (Frmt(14:16),'(I3)')  NumRFMBins
            ELSE
               Frmt = "(/,"//TextFmt//",' |',    ("//RealFmt//"))"
               WRITE (Frmt(19:21),'(I3)')  NumRFMBins
            ENDIF

            WRITE (RU,Frmt)  ' ', ( MinMean(IC) + ( IBM - 0.5 )*MeanBinWid(IC), IBM=1,NumRFMBins )

            IF ( .NOT. TabDelim )  THEN
               Frmt = "("//TextFmt//",' +',    "//TextFmt//")"
               WRITE (Frmt(17:19),'(I3)')  NumRFMBins + 1
               WRITE (RU,Frmt)  ( '----------', IBM=1,NumRFMBins+1 )
            ENDIF



               ! Set up the format for the regular data.

            IF ( TabDelim )  THEN
               Frmt = "(    ("//RealFmt//",:,'"//Tab//"'))"
               WRITE (Frmt(2:5),'(I4)')  NumRFMBins + 1
            ELSE
               Frmt = "("//RealFmt//",' |',   ("//RealFmt//"))"
               WRITE (Frmt(21:23),'(I3)')  NumRFMBins
            ENDIF


               ! Loop through all the range bins.

            DO IBR=1,NumRFRBins

               IF ( RF_Norm ) THEN
                  WRITE (RU,Frmt)  (IBR-0.5)*RngBinWid(IC), ( RF_Counts(IC,IBR,IBM)*TimeUnit/(RngBinWid(IC)*MeanBinWid(IC)) &
                                   , IBM=1,NumRFMBins )
               ELSE
                  WRITE (RU,Frmt)  (IBR-0.5)*RngBinWid(IC), ( RF_Counts(IC,IBR,IBM)*TimeUnit, IBM=1,NumRFMBins )
               ENDIF

            ENDDO ! IBR

         ENDDO ! IC

      ENDIF


         ! If all of the requested input files could not be read for aggregate analyses,
         ! append the list of bad files to the output.

      IF ( Aggregate .AND. ( BadFiles > 0 ) )  CALL WrBadList ( RU )


         ! Close results file.

      CLOSE ( RU )


         ! Deallocate space for bin arrays.

      DEALLOCATE ( RF_Counts  )
      DEALLOCATE ( MeanBinWid )
      DEALLOCATE ( RngBinWid  )


   ENDIF ! ( NumRFRBins > 0 )


      ! Deallocate space for cycle arrays.

   DEALLOCATE ( CycMult   )
   DEALLOCATE ( CycRange  )
   DEALLOCATE ( CycMean   )
   DEALLOCATE ( RF_Cycles )


   RETURN
   END SUBROUTINE Rainflow ! ( Fi )
!=======================================================================
   SUBROUTINE RF_Init ( Fi, PeriodStr )


      ! This routine creates the rainflow file and initializes the rainflow arrays.


   USE                             DataMod
   USE                             ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Fi

   CHARACTER(*), INTENT(OUT)    :: PeriodStr


      ! Local declarations.

   INTEGER                      :: IBM
   INTEGER                      :: IBR
   INTEGER                      :: IC
   INTEGER                      :: IOS
   INTEGER                      :: RFC
   INTEGER                      :: Sttus

   CHARACTER( 28)               :: Frmt
   CHARACTER(100)               :: RF_File
   CHARACTER(200)               :: FormStr



      ! Get date and time for rainflow file.

   DateStr = CurDate()
   TimeStr = CurTime()


      ! Allocate the array of rainflow I/O units if it has not already been allocated.

   IF ( .NOT. ALLOCATED( RUC ) )  THEN
      ALLOCATE ( RUC(NumRFCols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the rainflow I/O units array.' )
      ENDIF
   ENDIF


      ! Are we binning the cycles?

   IF ( NumRFRBins == 0 )  THEN


         ! For no binning, we have to create files for each output channel.

      DO IC=1,NumRFCols


            ! Which column is this?

         RFC = RF_Cols(IC)


            ! Create the name for this rainflow file.

         IF ( Aggregate )  THEN
            RF_File = TRIM( AggRoot )//'_'//TRIM( Titles(RFC) )//'.rcc'
         ELSE
            RF_File = TRIM( RootName(Fi) )//'_'//TRIM( Titles(RFC) )//'.rcc'
         ENDIF


            ! Open the rainflow file.

         CALL GetNewUnit ( RUC(IC) )

         OPEN ( RUC(IC) , FILE=Trim( RF_File ) , STATUS='UNKNOWN' , FORM='FORMATTED' , IOSTAT=IOS )

         IF ( IOS /= 0 )  THEN
            CALL ProgAbort ( ' Warning.  The rainflow file "'//Trim( RF_File )//'" could not be opened.')
         ENDIF


            ! Write out the header.

         IF ( HaveUnits )  THEN
            FormStr = "( / 'These rainflow cycles for ""' , 2A , '"" were generated by ' , A , A " &
                    //" , ' on ' , A , ' at ' , A , '.' )"
            WRITE (RUC(IC),FormStr)   TRIM( Titles(RFC) ), TRIM( Units(RFC) ), TRIM( ProgName ), TRIM( ProgVer ), DateStr, TimeStr
         ELSE
            FormStr = "( / 'These rainflow cycles for ""' , A , '"" were generated by ' , A , A " &
                    //" , ' on ' , A , ' at ' , A , '.' )"
            WRITE (RUC(IC),FormStr)   TRIM( Titles(RFC) ), TRIM( ProgName ), TRIM( ProgVer ), DateStr, TimeStr
         ENDIF

         IF ( Aggregate )  THEN

            IF ( BadFiles == 0 )  THEN
               FormStr = "( / 'These aggregate cycles were based upon ' , A , ' records from ' , A " &
                       //" , ' input files.' )"
               WRITE (RUC(IC),FormStr)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) )
            ELSE
               FormStr = "( / 'These aggregate cycles were based upon ' , A , ' records from ' , A " &
                       //" , ' of the ' , A , ' specified input files.' )"
               WRITE (RUC(IC),FormStr)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) ), TRIM( Int2LStr( NumFiles ) )
            ENDIF

         ELSE
            FormStr = "( / 'These cycles for file ""' , A , '"" were based upon ' , A , ' records.' )"
            WRITE (RUC(IC),FormStr)  TRIM( FileName(Fi) ), TRIM( Int2LStr( AnalRecs ) )
         ENDIF


           ! Let folks know if we used the peak finder.

         IF ( Do_PF )  THEN
           WRITE (RUC(IC),'(A)')  'The peak-finding algorithm was used.'
         ELSE
           WRITE (RUC(IC),'(A)')  'The peak-finding algorithm was not used.'
         ENDIF


            ! Write out mean wind speed and turbulence intensity.

         IF ( WS_Col > 0 )  THEN
            IF ( HaveUnits )  THEN
               WRITE (RUC(IC),'(/,A,F5.1,1X,A,/,A,F5.1,A)')  'Mean wind speed      =', MeanWs(Fi), Units(WS_Col), &
                                                             'Turbulence intensity =', TrbInt(Fi), '%'
            ELSE
               WRITE (RUC(IC),'(/,A,F5.1,/,A,F5.1,A)')  'Mean wind speed      =', MeanWs(Fi), &
                                                        'Turbulence intensity =', TrbInt(Fi), '%'
            ENDIF
         ENDIF


            ! Write out column heading(s).

         IF ( NumRFMBins == 1 )  THEN
            IF ( TabDelim )  THEN
               WRITE (RUC(IC),'(/,A)')  'NCycles'//Tab//'Ranges'
            ELSE
               Frmt = "(2(/,"//TextFmt//"))"
               WRITE (RUC(IC),Frmt)  '    NCycles','    Ranges' , '    ------','    ------'
            ENDIF
         ELSE
            IF ( TabDelim )  THEN
               WRITE (RUC(IC),'(/,A)')  'NCycles'//Tab//'Ranges'//Tab//'Means'
            ELSE
               Frmt = "(2(/,3"//TextFmt//"))"
               WRITE (RUC(IC),Frmt)  '    NCycles','    Ranges', '     Means','    ------', '    ------', '     -----'
            ENDIF
         ENDIF


      ENDDO ! IC

   ELSE


         ! Create the name for the rainflow file.

      IF ( Aggregate )  THEN
         RF_File = TRIM( AggRoot )//'.rcc'
      ELSE
         RF_File = TRIM( RootName(Fi) )//'.rcc'
      ENDIF


         ! Open the rainflow file.

      CALL GetNewUnit ( RU )

      OPEN ( RU , FILE=Trim( RF_File ) , STATUS='UNKNOWN' , FORM='FORMATTED' , IOSTAT=IOS )

      IF ( IOS /= 0 )  THEN
         CALL ProgAbort ( ' Warning.  The rainflow file "'//Trim( RF_File )//'" could not be opened.')
      ENDIF


         ! We're binning the cycles.  Write the appropriate header for aggregate or
         ! individual rainflow cycle counts.

      FormStr = "( / 'These binned, rainflow cycle counts were generated by ' , A , A , ' on ' , A , ' at ' , A , '.' )"
      WRITE (RU,FormStr)   TRIM( ProgName ), TRIM( ProgVer ), DateStr, TimeStr

      IF ( Aggregate )  THEN
         IF ( BadFiles == 0 )  THEN
            FormStr = "( / 'These aggregate counts were based upon ' , A , ' records from ' , A , ' files.' )"
            WRITE (RU,FormStr)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) )
         ELSE
            FormStr = "( / 'These aggregate counts were based upon ' , A , ' records from ' , A " &
                    //" , ' of the ' , A , ' specified input files.' )"
            WRITE (RU,FormStr)  TRIM( Int2LStr( AnalRecs ) ), TRIM( Int2LStr( GoodFiles ) ), TRIM( Int2LStr( NumFiles ) )
         ENDIF
      ELSE
         FormStr = "( / 'These counts for file ""' , A , '"" were based upon ' , A , ' records.' )"
         WRITE (RU,FormStr)  TRIM( FileName(Fi) ), TRIM( Int2LStr( AnalRecs ) )
      ENDIF


         ! Let folks know if we used the peak finder.

      IF ( Do_PF )  THEN
        WRITE (RU,'(A)')  'The peak-finding algorithm was used.'
      ELSE
        WRITE (RU,'(A)')  'The peak-finding algorithm was not used.'
      ENDIF


         ! Write out mean wind speed and turbulence intensity.

      IF ( WS_Col > 0 )  THEN
         IF ( HaveUnits )  THEN
            WRITE (RU,'(/,A,F5.1,1X,A,/,A,F5.1,A)')  'Mean wind speed      =', MeanWs(Fi), Units(WS_Col), &
                                                     'Turbulence intensity =', TrbInt(Fi), '%'
         ELSE
            WRITE (RU,'(/,A,F5.1,/,A,F5.1,A)')  'Mean wind speed      =', MeanWs(Fi), &
                                                'Turbulence intensity =', TrbInt(Fi), '%'
         ENDIF
      ENDIF


         ! Report rainflow options in output file.

      IF ( NumRFMBins == 1 )  THEN
         WRITE (RU,'(/,A,/)')  'Counts were generated for ranges only.'
      ELSE
         WRITE (RU,'(/,A,/)')  'Counts were generated for ranges and means.'
      ENDIF


         ! Determine correct string label for rainflow counting period.

      IF ( RF_Per == 1 )  THEN

         PeriodStr = 'second'

      ELSEIF ( RF_Per < 60)  THEN

         PeriodStr = TRIM( Int2LStr( RF_Per ) )//' seconds'

      ELSEIF ( MOD( RF_Per , 86400 ) == 0 )  THEN

         IF ( RF_Per == 86400 )  THEN
            PeriodStr = 'day'
         ELSE
            PeriodStr = TRIM( Int2LStr( RF_Per/86400 ) )//' days'
         ENDIF

      ELSEIF ( MOD( RF_Per , 3600 ) == 0 )  THEN

         IF ( RF_Per == 3600 )  THEN
            PeriodStr = 'hour'
         ELSE
            PeriodStr = TRIM( Int2LStr( RF_Per/3600 ) )//' hours'
         ENDIF

      ELSEIF ( MOD( RF_Per , 60 ) == 0 )  THEN

         IF ( RF_Per == 60 )  THEN
            PeriodStr = 'minute'
         ELSE
            PeriodStr = TRIM( Int2LStr( RF_Per/60 ) )//' minutes'
         ENDIF

      ELSE

         PeriodStr = TRIM( Int2LStr( RF_Per ) )//' seconds'

      ENDIF

      IF ( NumRFMBins == 1 )  THEN
         WRITE (RU,'(A)')  'The [X]-values are the peak-to-peak magnitudes'//' of the rainflow cycles.'

         WRITE (RU,'(A)')  'The [Y]-values are the rainflow cycles per '//TRIM( PeriodStr )//' for those peak-to-peak magnitudes.'
      ENDIF


         ! Allocate the RF binning arrays.

      ALLOCATE ( RF_Counts(NumRFCols,NumRFRBins,NumRFMBins) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the rainflow count array.' )
      ENDIF


      ALLOCATE ( MeanBinWid(NumRFCols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the rainflow MeanBinWid array.' )
      ENDIF


      ALLOCATE ( RngBinWid(NumRFCols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the rainflow RngBinWid array.' )
      ENDIF


         ! Initialize bin counts.

      DO IBM=1,NumRFMBins

         DO IBR=1,NumRFRBins

            DO IC=1,NumRFCols
               RF_Counts(IC,IBR,IBM) = 0
            ENDDO ! IC

         ENDDO ! IBR

      ENDDO ! IBM


   ENDIF


      ! Allocate the RF cycle arrays.

   ALLOCATE ( CycMult(AnalRecs) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rainflow CycMult array.' )
   ENDIF


   ALLOCATE ( CycRange(AnalRecs) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rainflow CycRange array.' )
   ENDIF


   ALLOCATE ( CycMean(AnalRecs) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rainflow CycMean array.' )
   ENDIF


   ALLOCATE ( RF_Cycles(NumRFCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rainflow RF_Cycles array.' )
   ENDIF


   RETURN
   END SUBROUTINE RF_Init ! ( Fi, PeriodStr )
!=======================================================================
   SUBROUTINE SRain ( Fi, RFC, NCyc, HCyc )


   !**************************************************************************
   !                                                                         *
   !                       >>>>> SRAIN <<<<<                                 *
   !                                                                         *
   !                                                                         *
   ! AUTHOR:   Larry Schluter                                                *
   !           Sandia National Laboratories                                  *
   !           Division 6225                                                 *
   !                                                                         *
   ! DATE:     February 2, 1989                                              *
   !                                                                         *
   ! DESCRIPTION: This routine counts a history as it occurs and identifies  *
   !              the same cycles as rainflow algorithms which require that  *
   !              the history be rearranged.  The steps listed correspond    *
   !              to the steps in the rainflow counting rules.               *
   !                                                                         *
   !              The algorithm was taken from "Simple Rainflow Counting     *
   !              Algorithms", by  S. D. Downing and D. F. Socie.  Paul      *
   !              Veers originally coded the algorithm and Larry Schluter    *
   !              modified the code to work with the LIFE2 code.             *
   !                                                                         *
   ! MODIFICATIONS:  This routine was heavily modified for Crunch, 7/1998.   *
   !                                                                         *
   !**************************************************************************


   USE                                DataMod
   USE                                ProgGen


      ! Argument declarations.

   REAL(ReKi), INTENT(IN)          :: HCyc

   INTEGER, INTENT(IN)             :: Fi
   INTEGER, INTENT(OUT)            :: Ncyc
   INTEGER, INTENT(IN)             :: RFC


      ! Local declarations.

   REAL(ReKi), ALLOCATABLE         :: Harm      (:)
   REAL(ReKi), ALLOCATABLE         :: Scratch   (:)
   REAL(ReKi)                      :: Slope
   REAL(ReKi)                      :: X
   REAL(ReKi)                      :: Y

   INTEGER                         :: IFi
   INTEGER                         :: IR
   INTEGER                         :: IRec
   INTEGER                         :: Istart
   INTEGER                         :: J
   INTEGER                         :: NPts
   INTEGER                         :: Sttus



      ! Allocate the Scratch array.

   ALLOCATE ( Scratch(AnalRecs), STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rainflow Scratch array.' )
   ENDIF


      ! Copy data for this column into the Scratch array.

   IF ( Fi == 0 )  THEN


         ! Use data from all files when Fi is 0.

      IRec = 0

      DO IFi=1,GoodFiles
         DO IR=1,NumRecs
            IRec          = IRec + 1
            Scratch(IRec) = ConvData(RFC,IR,IFi)
         ENDDO ! IR
      ENDDO ! IFi

   ELSE

      IF ( Aggregate )  THEN


            ! Use data from all files when an aggregate analysis.

         IRec = 0

         DO IFi=1,GoodFiles
            DO IR=1,NumRecs
               IRec          = IRec + 1
               Scratch(IRec) = ConvData(RFC,IR,IFi)
            ENDDO ! IR
         ENDDO ! IFi

      ELSE


            ! Use data from one file when not an aggregate analysis.

         DO IR=1,AnalRecs
            Scratch(IR) = ConvData(RFC,IR,Fi)
         ENDDO ! IR

      ENDIF ! ( Aggregate )

   ENDIF


      ! Allocate Harm array.

   ALLOCATE ( Harm(AnalRecs), STAT = Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rainflow Harm array.' )
   ENDIF


      ! Initialize the routine's variables.

   DO IR=1,AnalRecs
      Harm(IR) = 0.0
   ENDDO ! IR

   Harm(1) = Scratch(1)
   Harm(2) = Scratch(2)
   IR      = 2
   IStart  = 1
   J       = 0
   NCyc    = 0
   NPts    = 2

   IF ( Harm(1) <= Harm(2) )  THEN
      Slope = 1
   ELSE
      Slope = -1
   ENDIF


      ! Step 1.

   10 CONTINUE

   IR = IR + 1

   IF ( IR > AnalRecs )  GOTO 60

   NPts       = NPts + 1
   Slope      = -Slope
   Harm(NPts) = Scratch(IR)


      ! Step 2.

   20 CONTINUE

   IF ( NPts < IStart+1 )  GOTO 10

   X = Slope*( Harm(NPts) - Harm(NPts-1) )

   IF ( X    <= Smallest )  GOTO 200
   IF ( NPts <  IStart+2 )  GOTO 10

   Y = Slope*( Harm(NPts-2) - Harm(NPts-1) )


      ! Step 3.

   30 CONTINUE

   IF (X < Y )  GOTO 10

   IF ( ( X == Y ) .AND. ( IStart == NPts-2 ) )  GOTO 10
   IF ( ( X >  Y ) .AND. ( IStart == NPts-2 ) )  GOTO 40
   IF ( ( X >= Y ) .AND. ( IStart /= NPts-2 ) )  GOTO 50


      ! Step 4.

   40 CONTINUE

   IStart = IStart + 1

   GOTO 10


      ! Step 5.

   50 CONTINUE

   NCyc           = NCyc + 1
   CycMult (NCyc) = 1.0
   CycRange(NCyc) = Y
   CycMean (NCyc) = 0.5*( Harm(NPts-1) + Harm(NPts-2) )
   NPts           = NPts - 2
   Harm(NPts)     = Harm(NPts+2)
   GOTO 20


      ! Step 6.

   60 CONTINUE

   IF (HCyc < 0.999999) THEN
      GOTO 400
   ENDIF

      ! Counts half cycles as full cycles.

   J = J + 1

   IF ( J > IStart )  GOTO 900

   NPts       = NPts + 1
   Slope      = -Slope
   Harm(NPts) = Harm(J)


      ! Step 7.

   70 CONTINUE

   IF ( NPts <  IStart+1 )  GOTO 60

   X = Slope*( Harm(NPts) - Harm(NPts-1) )

   IF ( X    <= Smallest )  GOTO 300
   IF ( NPts <  IStart+2 )  GOTO 60

   Y = Slope*( Harm(NPts-2) - Harm(NPts-1) )


      ! Step 8

   80 CONTINUE

   IF ( X < Y )  GOTO 60


      ! Step 9

   90 CONTINUE

   NCyc           = NCyc + 1
   CycMult (NCyc) = 1.0
   CycRange(NCyc) = Y
   CycMean (NCyc) = 0.5*( Harm(NPts-1) + Harm(NPts-2) )
   NPts           = NPts - 2
   Harm(NPts)     = Harm(NPts+2)

   GOTO 70


   200 CONTINUE

   NPts       = NPts - 1
   Harm(NPts) = Harm(NPts+1)
   Slope      = -Slope

   GOTO 20


   300 CONTINUE

   NPts       = NPts - 1
   Harm(NPts) = Harm(NPts+1)
   Slope      = -Slope

   GOTO 70


      ! Counts half cycles as a user-specified value.

   400 CONTINUE

   DO J=2,NPts
      NCyc           = NCyc + 1
      CycMult (NCyc) = HCyc
      CycRange(NCyc) = ABS( Harm(J-1) - Harm(J) )
      CycMean (NCyc) = 0.5*( Harm(J-1) + Harm(J) )
   ENDDO ! J


      ! We're done.  Deallocate the temporary arrays.

   900   CONTINUE

   DEALLOCATE ( Harm )
   DEALLOCATE ( Scratch )


   RETURN
   END SUBROUTINE SRain ! ( Fi, RFC, NCyc, HCyc )
!=======================================================================

END MODULE RFSubs
