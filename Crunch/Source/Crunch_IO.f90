MODULE CrunchIO

   ! This module contains some I/O routines for Crunch.

   ! It contains the following routines:

   !  SUBROUTINE Check_Args
   !  SUBROUTINE CheckInp
   !  SUBROUTINE DetectCols
   !  SUBROUTINE DetectRecs
   !  SUBROUTINE GetFASTbinData ( InFile, GoodFile, Error )
   !  SUBROUTINE GetParams
   !  SUBROUTINE ReadFData ( InFile, GoodFile, Error )
   !  SUBROUTINE WrBadList ( Unit )


   USE                                NWTC_Library

   IMPLICIT                           NONE

CONTAINS

!=======================================================================
   SUBROUTINE Check_Args


      ! This subroutine is used to check for command-line arguments.


   USE                                ProgGen



      ! Get the name of the primary input file from the command line.

   CALL CheckArgs ( InpFile )

   IF ( LEN_TRIM( InpFile ) == 0 )  THEN
      CALL WrScr1    ( ' Syntax is:' )
      CALL WrScr     ( '    '//TRIM( ProgName )//' ['//SwChar//'h] <infile>' )
      CALL WrScr     ( ' where:' )
      CALL WrScr     ( '    '//SwChar//'h generates this help message.' )
      CALL ProgAbort ( '    <infile> is the name of the required primary input file.', TimeWait=-1.0, ErrLevel=1 )
   ENDIF


   RETURN
   END SUBROUTINE Check_Args
!=======================================================================
   SUBROUTINE CheckInp


      ! This subroutine is used to check input parameters for validity.


   USE                                DataMod


      ! Local declarations.

   INTEGER                         :: IC
   INTEGER                         :: IG



      ! Check filtering parameters.

   IF ( NumFilt /= 0 )  THEN

      IF ( NumFilt > NumInCols )  THEN
         CALL ProgAbort ( ' The number of filtered columns must not exceed ' &
                    //TRIM( Int2LStr( NumInCols ) )//'.')
      ENDIF

      DO IC=1,NumFilt

         IF ( ( FiltCols(IC) < 1 ) .OR. ( FiltCols(IC) > NumInCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' filter column must be between 1 and ' &
                       //TRIM( Int2LStr( NumInCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


      ! Check AA parameters.

   IF ( NumAACols /= 0 )  THEN

      IF ( NumAACols > NonAACols )  THEN
         CALL ProgAbort ( ' The number of AA columns must not exceed ' &
                    //TRIM( Int2LStr( NonAACols ) )//'.')
      ENDIF

      DO IC=1,NumAACols

         IF ( ( AA_Cols(IC) < 1 ) .OR. ( AA_Cols(IC) > NonAACols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' AA column must be between 1 and ' &
                       //TRIM( Int2LStr( NonAACols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

      IF ( AzimCol > NonAACols )  THEN
         CALL ProgAbort ( ' The azimuth column must not exceed '//TRIM( Int2LStr( NonAACols ) )//'.')
      ENDIF

      TotCols = NonAACols + 2*NumAACols

   ENDIF


      ! Check Crosstalk columns.

   IF ( NumXT /= 0 )  THEN

      DO IC=1,NumXT

         IF ( ( XT_Cols(1,IC) < 0 ) .OR. ( XT_Cols(1,IC) > NonAACols ) )  THEN
            CALL ProgAbort ( ' The (1,'//TRIM( Int2LStr( IC ) )//') crosstalk channel must be between 1 and ' &
                       //TRIM( Int2LStr( NonAACols ) )//' (inclusive).')
         ENDIF

         IF ( ( XT_Cols(2,IC) < 0 ) .OR. ( XT_Cols(2,IC) > NonAACols ) )  THEN
            CALL ProgAbort ( ' The (2,'//TRIM( Int2LStr( IC ) )//') crosstalk channel must be between 1 and ' &
                       //TRIM( Int2LStr( NonAACols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


      ! Check the list of peak-finder columns.

   IF ( NumPFCols /= 0 )  THEN

      DO IC=1,NumPFCols

         IF ( ( PF_Cols(IC) < 1 ) .OR. ( PF_Cols(IC) > TotCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' PF column must be between 1 and ' &
                       //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


      ! Check the peak-list info.

   IF ( NumPLCh /= 0 )  THEN

      IF ( ( NumPLCh < 0 ) .OR. ( NumPLCh > TotCols ) )  THEN
         CALL ProgAbort ( ' The number of peak-list channels must be between 0 and ' &
                    //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
      ENDIF

      DO IC=1,NumPLCh

         IF ( ( PL_Cols(IC) < 1 ) .OR. ( PL_Cols(IC) > TotCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' peak-list column must be between 1 and ' &
                       //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


      ! Check the PMF info.

   IF ( NumPMF /= 0 )  THEN

      IF ( ( NumPMF < 0 ) .OR. ( NumPMF > TotCols ) )  THEN
         CALL ProgAbort ( ' The number of PMF columns must be between 0 and ' &
                    //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
      ENDIF

      DO IC=1,NumPMF

         IF ( ( PMFCols(IC) < 1 ) .OR. ( PMFCols(IC) > TotCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' PMF column must be between 1 and ' &
                       //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


      ! Check the rainflow info.

   IF ( NumRFCols /= 0 )  THEN


      IF ( NumRFCols > TotCols )  THEN
         CALL ProgAbort ( ' The number of rainflow columns must not exceed '//TRIM( Int2LStr( TotCols ) )//'.')
      ENDIF

      DO IC=1,NumRFCols
         IF ( ( RF_Cols(IC) < 1 ) .OR. ( RF_Cols(IC) > TotCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' rainflow column must be between 1 and ' &
                           //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF
      ENDDO ! IC

   ENDIF


      ! Check extreme-event info.

   IF ( NumEEGrps /= 0 )  THEN

      DO IG=1,NumEEGrps

         IF ( TotEECols(IG) > TotCols )  THEN
            CALL ProgAbort ( ' The total number of columns in extreme-event group #'//TRIM( Int2LStr( IG ) ) &
                       //' must be between 1 and '//TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF

         DO IC=1,TotEECols(IG)

            IF ( ( EE_Cols(IC,IG) < 0 ) .OR. ( EE_Cols(IC,IG) > TotCols ) )  THEN
               CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' column in extreme-event group #'//TRIM( Int2LStr( IG ) ) &
                          //' must be between 1 and '//TRIM( Int2LStr( TotCols ) )//' (inclusive).')
            ENDIF

         ENDDO ! IC

      ENDDO ! IG

   ENDIF


      ! Check summary-statistics info.

   IF ( NumSFCols /= 0 )  THEN

      DO IC=1,NumSFCols

         IF ( ( SF_Cols(IC) < 1 ) .OR. ( SF_Cols(IC) > TotCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' summary-statistics column must be between 1 and ' &
                      //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


      ! Check info for extreme-statistics extrapolation.

   IF ( NumESCols /= 0 )  THEN

      DO IC=1,NumESCols

         IF ( ( ES_Cols(IC) < 1 ) .OR. ( ES_Cols(IC) > TotCols ) )  THEN
            CALL ProgAbort ( ' The #'//TRIM( Int2LStr( IC ) )//' statistical-extrapolation column must be between 1 and ' &
                       //TRIM( Int2LStr( TotCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

   ENDIF


   RETURN
   END SUBROUTINE CheckInp
!=======================================================================
   SUBROUTINE DetectCols


      ! This routine autodetects the number of columns in the first file.


   USE                                DataMod
   USE                                ProgGen


      ! Local declarations.

   INTEGER                         :: IC
   INTEGER                         :: IOS
   INTEGER                         :: IR
   INTEGER                         :: ND
   INTEGER                         :: Sttus

   LOGICAL                         :: Exists

   CHARACTER(  12)                 :: Fmt
   CHARACTER(9999)                 :: Line



      !  Open the first data file.

   INQUIRE ( FILE=TRIM( FileName(1) ) , EXIST=Exists )

   IF ( .NOT. Exists )  THEN
      CALL ProgAbort ( ' Warning.  The input file "'//TRIM( FileName(1) )//'" does not exist.' )
   ENDIF

   CALL GetNewUnit ( DU )
   OPEN ( DU , FILE=TRIM( FileName(1) ) , STATUS='OLD' , FORM='FORMATTED' )


      ! Tell the user what's up.

   IF ( NumFiles == 1 )  THEN
      CALL WrScr1 ( ' Autodetecting the number of columns in the data file.' )
   ELSE
      CALL WrScr1 ( ' Autodetecting the number of columns in the data files.' )
   ENDIF


      ! Are we autoparsing for column titles?

   IF ( CTRow > 0 )  THEN


         ! Skip down to channel-titles line and read it.

      DO IR=1,CTRow-1

         READ (DU,'()',IOSTAT=IOS)

         IF ( IOS < 0 )  THEN
            CALL PremEOF ( FileName(1), ' The error occurred while skipping to the channel-titles row.' )
         ENDIF

      ENDDO ! IR

      READ (DU,'(A)',IOSTAT=IOS)  Line

      IF ( IOS < 0 )  THEN
         CALL PremEOF ( FileName(1), ' The error occurred while trying to read the channel-titles line.' )
      ENDIF


         ! If there is only "whitespace" on the line, ProgAbort.

      NumInCols = CountWords( Line )

      IF ( NumInCols == 0 )  THEN
         CALL ProgAbort ( ' If Crunch is trying to autodetect the number of columns by scanning the' &
                    //' row of channel titles, that line must contain at least one character' &
                    //' that is not a space, tab, comma, single quote, or double quote.' )
      ENDIF


         ! Allocate array for titles.

      ALLOCATE ( Titles(NumInCols), STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for channel-titles array.' )
      ENDIF


         ! Parse channel titles from the titles line.

      CALL GetWords ( Line, Titles, NumInCols )


         ! Shall we parse for units?

      IF ( HaveUnits )  THEN

         IF ( CURow > CTRow+1 )  THEN


               ! Skip to the units row.

            DO IR=CTRow,CURow-2

               READ (DU,'()',IOSTAT=IOS)

               IF ( IOS < 0 )  THEN
                  CALL PremEOF ( FileName(1), ' The error occurred while skipping to the channel-units row.' )
               ENDIF

            ENDDO ! IR

         ELSE


               ! Rewind file and skip to the units row.

            REWIND DU

            DO IR=1,CURow-1

               READ (DU,'()',IOSTAT=IOS)

               IF ( IOS < 0 )  THEN
                  CALL PremEOF ( FileName(1), ' The error occurred while skipping to the channel-units row.' )
               ENDIF

            ENDDO ! IR

         ENDIF


            ! Allocate array for units.

         ALLOCATE ( Units(NumInCols) , STAT=Sttus )

         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for channel-units array.' )
         ENDIF


            ! Read units row and parse it.

         READ (DU,'(A)',IOSTAT=IOS)  Line

         IF ( IOS < 0 )  THEN
            CALL PremEOF ( FileName(1), ' The error occurred while trying to read the channel-units line.' )
         ENDIF

         CALL GetWords ( Line, Units, NumInCols )

      ENDIF

   ELSE


         ! Because there are no column titles in this file, let's scan
         ! the first data record to determine the number of columns.

         ! Skip down to first data record and read it.

      DO IR=1,FDRow-1

         READ (DU,'()',IOSTAT=IOS)

         IF ( IOS < 0 )  THEN
            CALL PremEOF ( FileName(1), ' The error occurred while skipping to the first data record.' )
         ENDIF

      ENDDO ! IR

      READ (DU,'(A)',IOSTAT=IOS)  Line

      IF ( IOS < 0 )  THEN
         CALL PremEOF ( FileName(1), ' The error occurred while trying to read the first data record.' )
      ENDIF

      NumInCols = CountWords( Line )


         ! Allocate array for titles.

      ALLOCATE ( Titles(NumInCols), STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for channel-titles array.' )
      ENDIF


         ! Create dummy column titles.

      DO IC=1,NumInCols

         ND  = INT( LOG10( REAL( IC ) ) + 1.0E-20 ) + 1
         Fmt = "('Chan',Ix.x)"

         WRITE (Fmt(10:10),'(I1)')  ND
         WRITE (Fmt(12:12),'(I1)')  ND
         WRITE (Titles(IC),'(A,I3.3)')  'Chan', IC

      ENDDO ! IC


   ENDIF


      ! Let's create the column list.

   NumCols = NumInCols

   ALLOCATE ( ColList(NumCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for column-list array.' )
   ENDIF

   DO IC=1,NumCols
      ColList(IC) = IC
   ENDDO ! IC


      ! Let's create the conversion-constants arrays.

   ALLOCATE ( Scal(NumCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for conversion-scales array.' )
   ENDIF


   ALLOCATE ( Offset(NumCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for conversion-offset array.' )
   ENDIF


   DO IC=1,NumCols
      Scal  (IC) = 1.0
      Offset(IC) = 0.0
   ENDDO ! IC


      ! Close the data file

   CLOSE ( DU )


   RETURN
   END SUBROUTINE DetectCols
!=======================================================================
   SUBROUTINE DetectRecs


      ! This routine autodetects the range of records in the data files.


   USE                                DataMod
   USE                                ProgGen


      ! Local declarations.

   REAL                             :: TFirst                        ! The scaled time of the first kept record.

   INTEGER                          :: IC
   INTEGER                          :: IOS
   INTEGER                          :: IR
   INTEGER                          :: Sttus
   INTEGER                          :: TimeColIn

   LOGICAL                          :: Exists



      !  Open data file.

   INQUIRE ( FILE=TRIM( FileName(1) ) , EXIST=Exists )

   IF ( .NOT. Exists )  THEN
      CALL ProgAbort ( ' Warning.  The input file "'//TRIM( FileName(1) )//'" does not exist.' )
   ENDIF

   CALL GetNewUnit ( DU )
   OPEN ( DU , FILE=TRIM( FileName(1) ) , STATUS='OLD' , FORM='FORMATTED' )


      ! Process the requested data records of this file.

   IF ( NumFiles == 1 )  THEN
      CALL WrScr1 ( ' Autodetecting the range of records in the data file.' )
   ELSE
      CALL WrScr1 ( ' Autodetecting the range of records in the data files.' )
   ENDIF


      ! Skip over the header records of this file.

   DO IR=1,FDRow-1

      READ (DU,'()',IOSTAT=IOS)

      IF ( IOS < 0 )  THEN
         CALL PremEOF ( FileName(1), ' The error occurred while skipping the #'//TRIM( Int2LStr( IR ) )//' header record.' )
      ENDIF

   ENDDO ! IR


      ! Allocate the raw data array.

   ALLOCATE ( RawData(NumInCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the raw data array.' )
   ENDIF


      ! Determine the time column if we are using it to limit the scope of the data.
      ! Check to make sure the time column is in the input file.

   IF ( UseTime )  THEN

      IF ( TimeCol == 0 )  THEN
         CALL ProgAbort ( ' When using time to determine what data to read, you must specify the time channel.' )
      ELSE IF ( TimeCol > NumInCols )  THEN
         CALL ProgAbort ( ' When using time to determine what data to read, the time channel' &
                     //' must be one of channels in the input file.  You specified ' &
                     //TRIM( Int2LStr( TimeCol ) )//' as the time channel, yet there are only ' &
                     //TRIM( Int2LStr( NumInCols ) )//' channels in the data file.' )
      ENDIF

      TimeColIn = ColList( TimeCol )


         ! Determine how many rows to skip to get to TStart and how many rows to read to get to TEnd.

      READ (DU,*,IOSTAT=IOS)  ( RawData(IC), IC=1,NumInCols )
      IF ( IOS > 0 )  THEN
         CALL ProgAbort ( ' Data error reading line '//Int2LStr( FDRow )//' of file "'//TRIM( FileName(1) )//'".' )
      ELSEIF ( IOS < 0 )  THEN
         CALL ProgAbort ( ' THere is no data on the first data row of file "'//TRIM( FileName(1) )//'".' )
      ENDIF

      TFirst = Scal(TimeCol)*RawData(TimeColIn) + Offset(TimeCol) + Smallest

      READ (DU,*,IOSTAT=IOS)  ( RawData(IC), IC=1,NumInCols )
      IF ( IOS > 0 )  THEN
         CALL ProgAbort ( ' Data error reading line '//Int2LStr( FDRow+1 )//' of file "'//TRIM( FileName(1) )//'".' )
      ELSEIF ( IOS < 0 )  THEN
         CALL ProgAbort ( ' There is no data on the first data row of file "'//TRIM( FileName(1) )//'".' )
      ENDIF

      TimeStep = Scal(TimeCol)*RawData(TimeColIn) + Offset(TimeCol) + Smallest - TFirst

      FirstRec = NINT( ( REAL( TStart, R8Ki ) - REAL( TFirst, R8Ki ) )/TimeStep ) + FDRow
      NumRecs  = NINT( ( REAL( TEnd  , R8Ki ) - REAL( TStart, R8Ki ) )/TimeStep ) + 1


         ! We will return here after deallocating the raw data array, as we know how what lines we want to keep.

      DEALLOCATE ( RawData , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the raw data array.' )
      ENDIF

      CLOSE ( DU )
      RETURN

   ENDIF


      ! Let's scan through the data,

   FirstRec = FDRow
   NumRecs  = 0

   DO

      READ (DU,*,IOSTAT=IOS)  ( RawData(IC), IC=1,NumInCols )

      IF ( IOS > 0 )  THEN
                                                    ! Invalid data in line.
         CALL ProgAbort ( ' Data error reading line '//TRIM( Int2LStr( FDRow + NumRecs ) ) &
                    //' of file "'//TRIM( FileName(1) )//'".' )

      ELSEIF ( IOS < 0 )  THEN                                                         ! We hit the end of file.

         IF ( NumRecs == 0 )  THEN
            CALL ProgAbort ( ' No valid data records were found in file "'//TRIM( FileName(1) )//'".' )
         ELSE
            EXIT
         ENDIF

      ENDIF


         ! Increment the number of kept file records.

      NumRecs = NumRecs + 1

   ENDDO


      ! Deallocate the raw data array.

   DEALLOCATE ( RawData , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for the raw data array.' )
   ENDIF


      ! Close the file and return.

   CLOSE ( DU )


   RETURN
   END SUBROUTINE DetectRecs
!=======================================================================
   SUBROUTINE GetFASTbinData ( InFile, GoodFile, Error )


      ! This routine calls the NWTC Library's ReadFASTbinand puts the data in file GoodFile.


   USE                          :: CrunchSubs
   USE                          :: DataMod
   USE                          :: ProgGen

   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER,          INTENT(IN) :: GoodFile
   INTEGER,          INTENT(IN) :: InFile

   LOGICAL, INTENT(OUT)         :: Error


      ! Local declarations.

   REAL(R8Ki), PARAMETER        :: MaxTSErr = 0.0001_R8Ki   ! The fractional error between the timestep in the first file and the current file.
   REAL(R8Ki)                   :: TSError                  ! The fractional error between the timestep in the first file and the current file.

   REAL(ReKi)                   :: TFirst                   ! The time at the beginning of the binary file.
   REAL(ReKi)                   :: TLast                    ! The time at the end of the binary file.

   INTEGER(IntKi)               :: ErrStat                  ! An error status to be returned by some NWTC Library routines.
   INTEGER(IntKi)               :: ICol                     ! Column index.
   INTEGER(IntKi)               :: IEnd                     ! Last record to keep.
   INTEGER(IntKi)               :: IStart                   ! First record to keep.
   INTEGER(IntKi)               :: IRec                     ! Record index.
   INTEGER(IntKi)               :: IXT                      ! Crosstalk index.
   INTEGER(IntKi)               :: KeepRec                  ! The index of kept records in the ConvData array.

   CHARACTER(200)               :: ErrMsg                   ! An error message to be returned by some NWTC Library routines.

   TYPE (FASTdataType)          :: FASTdata                 ! The derived type for holding FAST output data.


      !  Get FAST data from a binary output file.

   FASTdata%File = FileName(InFile)

   CALL ReadFASTbin ( DU, .FALSE., FASTdata, ErrStat, ErrMsg )

   IF ( ErrStat /= 0 )  THEN
      Error = .TRUE.
      RETURN
   END IF ! ( ErrStat /= 0 )


      ! Will the input data satisfy the start and end times if the end time is not zero?
      ! If so, skip this file.

   TFirst = Scal(1)*FASTdata%Data(1,1) + Offset(1)
   TLast  = Scal(1)*FASTdata%Data(FASTdata%NumRecs,1) + Offset(1)

   IF ( TEnd > 0.0 )  THEN
      IF ( ( TStart < TFirst ) .OR. ( TEnd > TLast ) )  THEN
         Error = .TRUE.
         RETURN
      ENDIF
      IStart = NINT( ( REAL( TStart, R8Ki ) - REAL( TFirst, R8Ki ) )/FASTdata%TimeStep ) + 1
      IEnd   = NINT( ( REAL( TEnd  , R8Ki ) - REAL( TFirst, R8Ki ) )/FASTdata%TimeStep ) + 1
   ELSE
      IStart = 1
      IEnd   = FASTdata%NumRecs
   ENDIF


      ! If this is the first file, set NumRecs and compute indices to limit time.
      ! Skip file if it doesn't have the same time step and include the specified start and end times.

   IF ( GoodFile == 1 )  THEN


         ! Find start and end records if limiting by time.
         ! Would anyone ever scale time?  Maybe to force it to zero at the beginning.  Just in case...

      TimeStep = FASTdata%TimeStep
      NumRecs  = IEnd - IStart + 1


         ! Allocate the ConvData array if it has not already been done.
         ! Don't waste storage on files we previously skipped (NumFiles-Infile+1).

      ALLOCATE ( ConvData(NumCols+NumCChan,NumRecs,NumFiles-Infile+1) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( '>>Error allocating memory for ConvData array in GetFASTbinData.' )
      ENDIF

   ELSE

         ! Is the time step for this file very close to that of the first good file?

      TSError = ABS( TimeStep/FASTdata%TimeStep - 1.0_R8Ki )

      IF ( TSError > MaxTSErr )  THEN
         Error = .TRUE.
         RETURN
      ENDIF

   END IF


      ! Copy the data from the FASTdata structure to the ConvData array.  Keep only the selected channels for the chosen time range.
      ! Apply scales and offsets.  Remove crosstalk.

   DO IRec=IStart,IEnd

      KeepRec = IRec - IStart + 1

      DO ICol=1,NumCols

         ConvData(ICol,KeepRec,GoodFile) = Scal(ICol)*FASTdata%Data(IRec,ColList(ICol)) + Offset(ICol)

      END DO ! ICol


         ! Remove crosstalk for requested pairs.

      DO IXT=1,NumXT

         CALL RemoveXT ( ConvData(XT_Cols(1,IXT),KeepRec,GoodFile) &
                       , ConvData(XT_Cols(2,IXT),KeepRec,GoodFile) , XT(1,1,IXT) )

     ENDDO ! IXT

   ENDDO

   CALL WrOver ( '  '//TRIM( Int2LStr( NumRecs ) )//' records were read.   ' )

   Error = .FALSE.


   RETURN
   END SUBROUTINE GetFASTbinData ! ( InFile, GoodFile, Error )
!=======================================================================
   SUBROUTINE GetParams


      ! This subroutine is used to open the input file and read it.


   USE                             DataMod
   USE                             ProgGen


      ! Local declarations.

   REAL(ReKi)                   :: TempAry      (2)                           ! A temporary array for reading in multiple real values.

   INTEGER                      :: Fi
   INTEGER                      :: IC
   INTEGER                      :: IG
   INTEGER                      :: IOS
   INTEGER                      :: NameLen
   INTEGER                      :: Sttus

   INTEGER(IntKi)               :: ErrStat                                    ! An error status to be returned by some NWTC Library routines.
   INTEGER(IntKi)               :: FmtWidth                                   ! The number of characters that will result from writes using RealFmt.

   CHARACTER(200)               :: ErrMsg                                     ! An error message to be returned by some NWTC Library routines.
   CHARACTER(100)               :: Line
   CHARACTER(  4)               :: NegThStr
   CHARACTER(  4)               :: PosThStr
   CHARACTER(200)               :: TmpAry       (3)                           ! A temporary array for reading in multiple character values.
   CHARACTER(MaxChrLen)         :: TmpTitle                                   ! A temporary string to hold the channel title for FAST binary files.
   CHARACTER(MaxChrLen)         :: TmpUnits                                   ! A temporary string to hold the channel units for FAST binary files.

   TYPE (FASTdataType)          :: FASTdata                                   ! The derived type for holding FAST output data.



      ! Open the parameter information file.

   CALL GetNewUnit   ( IU )
   CALL OpenFInpFile ( IU, InpFile )

   !-------------------------------------------------------------------------------

      ! Read the file header.

   CALL ReadCom ( IU, InpFile,                       'the Crunch title line' )
   CALL ReadVar ( IU, InpFile, JobTitle, 'JobTitle', 'The job title line,'   )


      !-----  Job Options  ------------------------------------------------------------

   CALL ReadCom ( IU, InpFile, 'the Job Options title line' )

   READ (IU,*,IOSTAT=IOS)  Echo

   CALL CheckIOS ( IOS, InpFile, 'the Echo flag', FlgType )

   IF ( Echo )  THEN
      CALL GetRoot ( InpFile, ParRoot )
      CALL OpenFOutFile ( UnEc, TRIM( ParRoot )//'.ech' )
      WRITE (UnEc,'(A)')                        'Echo of WT_Perf Input File:'
      WRITE (UnEc,'(A)')                        ' "'//TRIM( InpFile )//'"'
      WRITE (UnEc,'(A)')                        'Generated on: '//CurDate()//' at '//CurTime()//'.'
      WRITE (UnEc,'(A)')                        JobTitle
      WRITE (UnEc,"(2X,L11,2X,A,T27,' - ',A)")  Echo, 'Echo', 'Echo input parameters to "<root>.ech"?'
   ENDIF

   CALL ReadVar ( IU, InpFile, Out_Stats , 'Out_Stats' , 'the flag for outputting statistics'      )
   CALL ReadVar ( IU, InpFile, Out_Data  , 'Out_Data'  , 'the flag for outputting modified data'   )
   CALL ReadVar ( IU, InpFile, TabDelim  , 'TabDelim'  , 'the flag for using tab-delimited output' )
   CALL ReadVar ( IU, InpFile, RealFmt   , 'RealFmt'   , 'the real format specifier'               )
   CALL ReadVar ( IU, InpFile, Aggregate , 'Aggregate' , 'the flag for doing aggregate analyses'   )

   IF ( Aggregate )  THEN
      CALL ReadVar ( IU, InpFile, AggRoot, 'AggRoot', 'the root name for the aggregate-analysis files' )
   ELSE
      CALL ReadCom ( IU, InpFile, 'the unused root name for the aggregate-analysis files.' )
   ENDIF


      ! Test to make sure we have a valid format string.

   CALL ChkRealFmtStr ( RealFmt, 'RealFmt', FmtWidth, ErrStat, ErrMsg )

   IF ( ErrStat == ErrID_Fatal )  THEN
      CALL ProgAbort ( ErrMsg )
   ELSEIF ( ErrStat == ErrID_Severe )  THEN
      CALL ProgWarn ( ErrMsg )
   ENDIF


      ! If not tab-delimited output, generate the text format specifier.

   IF ( .NOT. TabDelim )  THEN

      IF ( ( FmtWidth < 11 ) .OR. ( FmtWidth > 99 ) )  THEN
         CALL ProgAbort ( ' The field width for the numerical-output format specifier must be between 11 and 99 characters' &
                    //' (inclusive).  The format string is "'//TRIM( RealFmt )//'".')
      ENDIF

      WRITE (TextFmt (2:3),'(I2)')  FmtWidth - 10
      WRITE (TextFmtI(2:3),'(I2)')  FmtWidth - 10

   ENDIF


      !-----  Input-Data Layout  ------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Input-Data Layout title line'                 )
   CALL ReadVar ( IU, InpFile, FASTbin,     'FASTbin', 'the flag indicating input is in FAST binary form' )
   CALL ReadVar ( IU, InpFile, CTRow  ,     'CTRow'  , 'the row number for channel titles'                )
   CALL ReadVar ( IU, InpFile, CURow  ,     'CURow'  , 'the row number for channel units'                 )
   CALL ReadVar ( IU, InpFile, FDRow  ,     'FDRow'  , 'the number of the first row of numerical data'    )
   CALL ReadVar ( IU, InpFile, NumRecs,     'NumRecs', 'the number of data records'                       )
   CALL ReadAry ( IU, InpFile, TempAry,  2, 'TempAry', 'the start and end times'                          )

   IF ( CTRow < 0 )  THEN
      CALL ProgAbort ( ' The row number for channel titles must not be negative.')
   ENDIF

   IF ( CURow < 0 )  THEN
      CALL ProgAbort ( ' The row number for channel units must not be negative.')
   ELSEIF ( ( CTRow /= 0 ) .AND. ( CURow == CTRow ) )  THEN
      CALL ProgAbort ( ' The row number for channel units must not the same as the row number of the channel titles.')
   ELSEIF ( CURow == 0 )  THEN
      HaveUnits = .FALSE.
   ELSE
      HaveUnits = .TRUE.
   ENDIF

   IF ( .NOT. FASTbin )  THEN
      IF ( FDRow <= 0 )  THEN
         CALL ProgAbort ( ' The number of the first row of numerical data must be greater than zero.')
      ELSE IF ( FDRow <= CTRow )  THEN
         CALL ProgAbort ( ' The first data row must be greater than the row containing the channel titles.')
      ELSE IF ( FDRow <= CURow )  THEN
         CALL ProgAbort ( ' The first data row must be greater than the row containing the channel units.')
      END IF
   END IF

   IF ( NumRecs < 0 )  THEN
      CALL ProgAbort ( ' The number of data records must be not be negative.')
   ELSEIF ( NumRecs > 0 )  THEN
      FirstRec = FDRow
   ENDIF

   TStart = TempAry(1)
   TEnd   = TempAry(2)

   IF ( ( TStart == 0.0 ) .AND. ( TEnd == 0.0 ) )  THEN
      UseTime = .FALSE.
   ELSEIF ( TStart >= TEnd )  THEN
      CALL ProgAbort ( ' The start time must be less than the end time unless both are zero.')
   ELSE
      UseTime = .TRUE.
   ENDIF


      !-----  Channel Information  ----------------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Channel-Information title line'      )
   CALL ReadVar ( IU, InpFile, NumInCols, 'NumInCols', 'the number of channels in the data file' )

   IF ( NumInCols < 0 )  THEN

      CALL ProgAbort ( ' The number of channels in the data file must not be negative.')

   ELSEIF ( NumInCols > 0 )  THEN


          ! We're going to specify column information.

      AutoCols = .FALSE.


         ! Read number of used columns.

      CALL ReadVar ( IU, InpFile, NumCols, 'NumCols', 'the number of channels to use')

      IF ( NumCols <= 0 )  THEN
         CALL ProgAbort ( ' xThe number of used input channels must be greater than zero.')
      ENDIF


         ! Allocate arrays for column info.

      CALL AllocAry ( Titles , NumCols, 'Titles'  )
      CALL AllocAry ( Units  , NumCols, 'Units'   )
      CALL AllocAry ( ColList, NumCols, 'ColList' )
      CALL AllocAry ( Scal   , NumCols, 'Scal'    )
      CALL AllocAry ( Offset , NumCols, 'Offset'  )


         ! Get info for each channel.

      CALL ReadCom ( IU, InpFile, 'the channel information line'      )

      DO IC=1,NumCols

         READ (IU,'(A)',IOSTAT=IOS)  Line


         IF ( IOS < 0 )  THEN
            CALL PremEOF ( InpFile , ' The error occurred while trying to read information for the #' &
                                     //TRIM( Int2LStr( IC ) )//' channel.' )
         ELSEIF ( LEN_TRIM( Line ) == 0 )  THEN
            CALL ProgAbort ( ' The line for the #'//TRIM( Int2LStr( IC ) )//' column is blank.')
         ENDIF

         READ (Line,*,IOSTAT=IOS)  Titles(IC), Units(IC), ColList(IC), Scal(IC), Offset(IC)

         CALL CheckIOS ( IOS, InpFile, 'the information for input channel #'//TRIM( Int2LStr( IC ) ), NumType )

         IF ( ( ColList(IC) < 1 ) .OR. ( ColList(IC) > NumInCols ) )  THEN
            CALL ProgAbort ( ' The original "'//TRIM( Titles(IC) )  //'" channel number must be between 1 and ' &
                       //TRIM( Int2LStr( NumInCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

      HaveUnits = .TRUE.

   ELSEIF ( FASTbin )  THEN


         ! Read number of used columns.

      CALL ReadVar ( IU, InpFile, NumCols, 'NumCols', 'the number of channels to use' )

      IF ( NumCols < 0 )  THEN
         CALL WrScr ( '' )
         CALL ProgAbort ( ' The number of used input channels must not be negative.' )
      ENDIF


         ! Allocate arrays for column info.

      IF ( NumCols > 0 )  THEN
         CALL AllocAry ( Titles , NumCols, 'Titles'  )
         CALL AllocAry ( Units  , NumCols, 'Units'   )
         CALL AllocAry ( ColList, NumCols, 'ColList' )
         CALL AllocAry ( Scal   , NumCols, 'Scal'    )
         CALL AllocAry ( Offset , NumCols, 'Offset'  )
      ENDIF


         ! Get info for each channel.

      CALL ReadCom ( IU, InpFile, 'the channel information line'      )

      DO IC=1,NumCols

         READ (IU,'(A)',IOSTAT=IOS)  Line


         IF ( IOS < 0 )  THEN
            CALL PremEOF ( InpFile , ' The error occurred while trying to read information for the #' &
                                     //TRIM( Int2LStr( IC ) )//' channel.' )
         ELSEIF ( LEN_TRIM( Line ) == 0 )  THEN
            CALL ProgAbort ( ' The line for the #'//TRIM( Int2LStr( IC ) )//' column is blank.')
         ENDIF

         READ (Line,*,IOSTAT=IOS)  TmpTitle, TmpUnits, ColList(IC), Scal(IC), Offset(IC)                                ! We will get names and units from the FAST binary file.

         CALL CheckIOS ( IOS, InpFile, 'the information for input channel #'//TRIM( Int2LStr( IC ) ), NumType )

         IF ( ( ColList(IC) < 1 ) .OR. ( ColList(IC) > NumInCols ) )  THEN
            CALL ProgAbort ( ' The original "'//TRIM( TmpTitle )  //'" channel number must be between 1 and ' &
                       //TRIM( Int2LStr( NumInCols ) )//' (inclusive).')
         ENDIF

      ENDDO ! IC

      HaveUnits = .TRUE.

   ELSE


         ! We're going to autodetect the channels.

      AutoCols = .TRUE.

      CALL ReadCom ( IU, InpFile, 'the unused number of channels to use' )
      CALL ReadCom ( IU, InpFile, 'the channel-information comment line' )

   ENDIF


      !-----  Filtering  --------------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                     'the Filter-Information title line' )
   CALL ReadVar ( IU, InpFile, NumFilt, 'NumFilt', 'the number of channels to filter'  )

   IF ( NumFilt == 0 )  THEN

      CALL ReadCom ( IU, InpFile, 'the unused line containing the list of columns to filter')
      CALL ReadCom ( IU, InpFile, 'the unused line containing the filter type')
      CALL ReadCom ( IU, InpFile, 'the unused line containing the low cut-off frequency')
      CALL ReadCom ( IU, InpFile, 'the unused line containing the high cut-off frequency')

   ELSE

      IF ( NumFilt < 0 ) THEN
         CALL ProgAbort ( ' The number of filtered columns must not be negative.' )
      ENDIF


         ! Allocate array for filter column list.

      CALL AllocAry ( FiltCols, NumFilt, 'FiltCols' )


         ! Read in the rest of the filter information.

      CALL ReadAry ( IU, InpFile, FiltCols,  NumFilt, 'FiltCols', 'the list columns for filtering' )
      CALL ReadVar ( IU, InpFile, FiltType,           'FiltType', 'the filter type'                )
      CALL ReadVar ( IU, InpFile, LoCut   ,           'LoCut'   , 'the low-pass cutoff frequency'  )
      CALL ReadVar ( IU, InpFile, HiCut   ,           'HiCut'   , 'the high-pass cutoff frequency' )

   ENDIF


      !-----  Calculated Channels  ----------------------------------------------------

   CALL ReadCom ( IU, InpFile,                       'the Calculated-Channels title line'       )
   CALL ReadVar ( IU, InpFile, NumCChan, 'NumCChan', 'the number of calculated channels'        )
   CALL ReadVar ( IU, InpFile, Seed    , 'Seed'    , 'the seed for the random-number generator' )
   CALL ReadCom ( IU, InpFile,                       'the calculated-channels information line' )

   IF ( NumCChan > 0 )  THEN


         ! Allocate space to store calclulated-channel information.

      CALL AllocAry ( CCTitles, NumCChan, 'CCTitles' )
      CALL AllocAry ( EqnStr  , NumCChan, 'EqnStr'   )

      IF ( HaveUnits )  CALL AllocAry ( CCUnits, NumCChan, 'CCUnits' )

      ALLOCATE ( Equation(NumCChan) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the Equation array.' )
      ENDIF


          ! Read in the equations.

      DO IC=1,NumCChan

         IF ( HaveUnits )  THEN
            CALL ReadAry ( IU, InpFile, TmpAry,  3, 'TmpAry', 'the information for calculated channel #'//TRIM( Int2LStr( IC ) ) )
            CCTitles(IC) = TmpAry(1)
            CCUnits (IC) = TmpAry(2)
            EqnStr  (IC) = TmpAry(3)
         ELSE
            CALL ReadAry ( IU, InpFile, TmpAry,  2, 'TmpAry', 'the information for calculated channel #'//TRIM( Int2LStr( IC ) ) )
            CCTitles(IC) = TmpAry(1)
            EqnStr  (IC) = TmpAry(2)
         ENDIF

      ENDDO ! IC

   ELSEIF ( NumCChan < 0 )  THEN

      CALL ProgAbort ( ' The number of calculated channels must be greater than or equal to zero.')

   ENDIF


      !-----  Moving Avergaes  --------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                 'the Moving-Averages title line'             )
   CALL ReadVar ( IU, InpFile, NumMA, 'NumMA', 'the number of channels for moving averages' )
   CALL ReadCom ( IU, InpFile,                 'the moving-averages information line'       )

   IF ( NumMA > 0 )  THEN


         ! Allocate arrays for moving-average information.

      CALL AllocAry ( MATitles, NumMA, 'MATitles' )
      CALL AllocAry ( MAChans , NumMA, 'MAChans'  )
      CALL AllocAry ( MAPeriod, NumMA, 'MAPeriod' )


          ! Read in the moving-average information.

      DO IC=1,NumMA

         READ (IU,*,IOSTAT=IOS)  MATitles(IC), MAChans(IC), MAPeriod(IC)

         CALL CheckIOS ( IOS, InpFile, 'the information for moving-average channel #'//TRIM( Int2LStr( IC ) ), NumType )

      ENDDO ! IC

   ELSEIF ( NumMA < 0 )  THEN

      CALL ProgAbort ( ' The number of moving-average channels must be greater than or equal to zero.')

   ENDIF


      !-----  Time and Wind Speed  ----------------------------------------------------

   CALL ReadCom ( IU, InpFile,                     'the Time and Wind Speed title line' )
   CALL ReadVar ( IU, InpFile, TimeCol, 'TimeCol', 'the time channel number'            )
   CALL ReadVar ( IU, InpFile, WS_Col , 'WS_Col' , 'the wind-speed channel number'      )

   IF ( TimeCol < 0 )  THEN
      CALL ProgAbort ( ' The specified time column must not be negative.' )
   ENDIF

   IF ( WS_Col < 0 )  THEN
      CALL ProgAbort ( ' The specified wind-speed column must not be negative.' )
   ENDIF


      !-----  Load Roses  -------------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                       'the Load Rose title line'       )
   CALL ReadVar ( IU, InpFile, NumRoses, 'NumRoses', 'the number of load roses'       )
   CALL ReadCom ( IU, InpFile,                       'the load-rose information line' )

   IF ( NumRoses > 0 )  THEN


         ! Allocate arrays for rose information.

      CALL AllocAry ( RoseName , NumRoses, 'RoseName'  )
      CALL AllocAry ( RoseCh00 , NumRoses, 'RoseCh00'  )
      CALL AllocAry ( RoseCh90 , NumRoses, 'RoseCh90'  )
      CALL AllocAry ( RoseSects, NumRoses, 'RoseSects' )


          ! Read in the rose information.

      DO IC=1,NumRoses

         READ (IU,*,IOSTAT=IOS)  RoseName(IC), RoseCh00(IC), RoseCh90(IC), RoseSects(IC)

         CALL CheckIOS ( IOS, InpFile, 'the information for load rose #'//TRIM( Int2LStr( IC ) ), NumType )


            ! Check the validity of the input data.

         IF ( LEN_TRIM( RoseName(IC) ) > 8 )  THEN
            CALL UsrAlarm
            CALL WrScr1 ( 'Warning!  The length of the name for load rose #'//TRIM( Int2LStr( IC ) ) &
                     //' is longer than 8 characters.  Name truncated to "'//RoseName(IC)//'".' )
         ENDIF


            ! Is the number of rose sectors less than 100?

         IF ( ( RoseSects(IC) > 99 ) .OR. ( RoseSects(IC) < 1 ) )  THEN
            CALL ProgAbort ( ' The number of sectors for load rose #'//TRIM( Int2LStr( IC ) ) &
                       //' must be between 1 and 99 (inclusive).')
         ENDIF

      ENDDO ! IC


            ! Calculate the number of new channels for the roses.

      NumRoseCh = 0

      DO IC=1,NumRoses
         NumRoseCh = NumRoseCh + RoseSects(IC)
      END DO ! IC


   ELSEIF ( NumRoses < 0 )  THEN

      CALL ProgAbort ( ' The number of load roses must be greater than or equal to zero.')

   ENDIF


      !-----  Azimuth Averages  -------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                       'the Azimuth Average title line'       )
   CALL ReadVar ( IU, InpFile, NumAACols, 'NumAACols', 'the number of load roses'       )

   IF ( NumAACols == 0 )  THEN
      CALL ReadCom ( IU, InpFile, 'the Azimuth Average title line' )
      CALL ReadCom ( IU, InpFile, 'the Azimuth Average title line' )
      CALL ReadCom ( IU, InpFile, 'the Azimuth Average title line' )
      CALL ReadCom ( IU, InpFile, 'the Azimuth Average title line' )
   ELSEIF ( NumAACols < 0 )  THEN
      CALL ProgAbort ( ' The number of AA columns must not be negative.')
   ELSE


         ! Allocate array for AA column list.

      CALL AllocAry ( AA_Cols, NumAACols, 'AA_Cols' )


         ! Read in the rest of the AA information.

      CALL ReadAry ( IU, InpFile, AA_Cols  , NumAACols, 'AA_Cols'  , 'the list of azimuth-average channels' )
      CALL ReadVar ( IU, InpFile, NumAABins           , 'NumAABins', 'the number of azimuth-average bins'   )
      CALL ReadVar ( IU, InpFile, AzimCol             , 'AzimCol'  , 'the azimuth channel'                  )
      CALL ReadVar ( IU, InpFile, Out_AA              , 'Out_AA'   , 'the output-azimuth-averages flag'     )

      IF ( NumAABins <= 0 )  THEN
         CALL ProgAbort ( ' Warning.  The number of AA bins must be greater than zero.')
      ENDIF

      DelAzim  = 360.0/NumAABins
      DelAzim2 = 0.5*DelAzim

   ENDIF


      !-----  Crosstalk  --------------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                 'the Crosstalk title line'              )
   CALL ReadVar ( IU, InpFile, NumXT, 'NumXT', 'the number of crosstalk matrices'      )
   CALL ReadCom ( IU, InpFile,                 'the crosstalk channel information line' )

   IF ( NumXT > 0 )  THEN


         ! Allocate array for crosstalk matrices.

      CALL AllocAry ( XT     , 2, 2, NumXT, 'XT'      )
      CALL AllocAry ( XT_Cols, 2,    NumXT, 'XT_Cols' )


         ! Read in the matrices.

      DO IC=1,NumXT

         READ (IU,*,IOSTAT=IOS)  XT_Cols(1,IC), XT_Cols(2,IC), XT(1,1,IC), XT(1,2,IC), XT(2,1,IC), XT(2,2,IC)

         CALL CheckIOS ( IOS, InpFile, 'the information for crosstalk matrix #'//TRIM( Int2LStr( IC ) ), NumType )

      ENDDO ! IXT

   ELSEIF ( NumXT < 0 )  THEN

      CALL ProgAbort ( ' The number of crosstalk pairs cannot be negative.' )

   ENDIF


      !-----  Peak Finder  ------------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Peak Finder title line'              )
   CALL ReadVar ( IU, InpFile, NumPFCols, 'NumPFCols', 'the number of channels for peak finding' )

   IF ( NumPFCols == 0 )  THEN

      Do_PF = .FALSE.

      CALL ReadCom ( IU, InpFile, 'the peak channel list line' )

   ELSE


         ! Allocate array for PF column list and read it.

      CALL AllocAry ( PF_Cols, NumPFCols, 'PF_Cols' )

      CALL ReadAry ( IU, InpFile, PF_Cols, NumPFCols, 'PF_Cols', 'the list of channels for which we will find peaks' )

      Do_PF = .TRUE.

   ENDIF


      !-----  Peak/Trough Listing  ----------------------------------------------------

   CALL ReadCom ( IU, InpFile,                     'the Peak/Trough Listing title line' )
   CALL ReadVar ( IU, InpFile, NumPLCh, 'NumPLCh', 'the number of peak/trough channels' )


   IF ( NumPLCh == 0 )  THEN

      CALL ReadCom ( IU, InpFile, 'the method of identifying peaks line'   )
      CALL ReadCom ( IU, InpFile, 'the peak-list time flag line'           )
      CALL ReadCom ( IU, InpFile, 'the peak-list channel information line' )

   ELSE

         ! Read in the method of identifying peaks.

      CALL ReadVar ( IU, InpFile, PL_Meth, 'PL_Meth', 'the method of identifying peaks' )

      IF ( ( PL_Meth /= 1 ) .AND. ( PL_Meth /= 2 ) )  THEN
         CALL ProgAbort ( ' Warning.  The method of identifying peaks must be 1 or 2.')
      ENDIF


         ! Allocate arrays for peak/trough column information.

      CALL AllocAry ( PL_Cols   , NumPLCh, 'PL_Cols'    )
      CALL AllocAry ( PLNegThld , NumPLCh, 'PLNegThld'  )
      CALL AllocAry ( PLPosThld , NumPLCh, 'PLPosThld'  )
      CALL AllocAry ( PLWrTr    , NumPLCh, 'PLWrTr'     )
      CALL AllocAry ( PLWrPk    , NumPLCh, 'PLWrPk'     )
      CALL AllocAry ( PLMeanThld, NumPLCh, 'PLMeanThld' )


         ! Read more of the Peak/Trough information.

      CALL ReadVar ( IU, InpFile, WrPLtime, 'WrPLtime', 'the peak-list time flag'                )
      CALL ReadCom ( IU, InpFile,                       'the peak-list channel information line' )


         ! Ensure that the time column was specified if the user requested times to be output for the peak listing.

      IF ( WrPLtime )  THEN
         IF ( TimeCol == 0 )  THEN
            CALL WrScr     ( '' )
            CALL ProgAbort ( ' For peak/valley listing, TimeCol must be > 0 if WrPLtime is true.' )
         ENDIF
      ENDIF


         ! Create the format statement.

      IF ( WrPLtime )  THEN
         IF ( TabDelim )  THEN
            PLFrmt = "("//TRIM( RealFmt )//",'"//Tab//"',"//TRIM( RealFmt )//")"
         ELSE
            PLFrmt = "(2("//TRIM( RealFmt )//"))"
         ENDIF
      ELSE
         PLFrmt = "("//TRIM( RealFmt )//")"
      ENDIF


         ! Read in the peak-list column data.

      DO IC=1,NumPLCh

         READ (IU,'(A)',IOSTAT=IOS)  Line

         IF ( IOS < 0 )  THEN
            CALL PremEOF ( InpFile , ' The error occurred while trying to read the column information for peak listing.' )
         ENDIF

         READ (Line,*,IOSTAT=IOS)  PL_Cols(IC), PLWrTr(IC), PLNegThld(IC), PLWrPk(IC), PLPosThld(IC)

         IF ( IOS < 0 )  THEN

            CALL PremEOF ( InpFile , ' The The error occurred while trying to read the peak-listing column information for the #' &
                                   //TRIM( Int2LStr( IC ) )//' column.' )

         ELSEIF ( IOS > 0 )  THEN

            PosThStr = ' '

            READ (Line,*,IOSTAT=IOS)  PL_Cols(IC), PLWrTr(IC), NegThStr, PLWrPk(IC), PosThStr

            IF ( IOS > 0 )  THEN
               CALL ProgAbort ( ' Invalid input.  The error occurred while trying to read the peak-listing column information'  &
                              //' for the #'//TRIM( Int2LStr( IC ) )//' column.' )
            ENDIF

            CALL Conv2UC( NegThStr )
            CALL Conv2UC( PosThStr )

            IF ( ( NegThStr == 'MEAN' ) .AND. ( PosThStr == 'MEAN' ) )  THEN
               PLMeanThld(IC) = .TRUE.
            ELSEIF ( ( ( NegThStr == 'MEAN' ) .AND. ( PosThStr /= 'MEAN' ) ) .OR. &
                     ( ( NegThStr /= 'MEAN' ) .AND. ( PosThStr == 'MEAN' ) ) )  THEN
               CALL ProgAbort ( ' If one of the threshold values for the #' &
                          //TRIM( Int2LStr( IC ) )//' peak-list column is set to "MEAN," then so must the other.')
            ELSE
               PLMeanThld(IC) = .FALSE.
            ENDIF

         ELSE
            PLMeanThld(IC) = .FALSE.
         ENDIF

         IF ( ( .NOT. PLWrTr(IC) ) .AND. ( .NOT. PLWrPk(IC) ) )  THEN
            CALL ProgAbort ( ' For peak/trough listing, you must not turn off both the peaks and troughs for the #' &
                       //TRIM( Int2LStr( IC ) )//' column.' )
         ENDIF

         IF ( ( PLNegThld(IC) > PLPosThld(IC) ) .AND. ( .NOT. PLMeanThld(IC) ) )  THEN
            CALL ProgAbort ( ' For peak/trough listing, the positive threshold is not >= the negative threshold for the #' &
                       //TRIM( Int2LStr( IC ) )//' column.' )
         ENDIF

      ENDDO ! IC

   ENDIF


      !-----  Probability Mass  -------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                   'the Probability Mass title line' )
   CALL ReadVar ( IU, InpFile, NumPMF, 'NumPMF', 'the number of PMF channels'      )

   IF ( NumPMF == 0 )  THEN

      CALL ReadCom ( IU, InpFile, 'the number of PMF-bins line'      )
      CALL ReadCom ( IU, InpFile, 'the PMF channel-information line' )

   ELSE

         ! Read in the number of PMF bins and skip the following comment.

      CALL ReadVar ( IU, InpFile, NumPMFBins, 'NumPMFBins', 'the number of PMF bins'           )
      CALL ReadCom ( IU, InpFile,                           'the PMF channel information line' )

      IF ( NumPMFBins < 2 )  THEN
         CALL ProgAbort ( ' Warning.  The number of PMF bins must be greater than 1.')
      ENDIF


         ! Allocate arrays for PMF column data.

      CALL AllocAry ( PMFCols,    NumPMF, 'PMFCols'    )
      CALL AllocAry ( PMFMins,    NumPMF, 'PMFMins'    )
      CALL AllocAry ( PMFMaxs,    NumPMF, 'PMFMaxs'    )
      CALL AllocAry ( PMFAutoScl, NumPMF, 'PMFAutoScl' )


         ! Read in the PMF column data.

      DO IC=1,NumPMF

         READ (IU,*,IOSTAT=IOS)  PMFCols(IC), PMFMins(IC), PMFMaxs(IC)

         CALL CheckIOS ( IOS, InpFile, 'the information for PMF Channel #'//TRIM( Int2LStr( IC ) ), NumType )

         IF ( ( PMFMins(IC)  > PMFMaxs(IC) ) .OR. &
            ( ( PMFMins(IC) == PMFMaxs(IC) ) .AND. ( PMFMins(IC) /= 0.0 ) ) )  THEN
            CALL ProgAbort ( ' The PMF minimum must be less than the PMF maximum for the #' &
                       //TRIM( Int2LStr( IC ) )//' PMF column.')
         ENDIF
         IF ( ( PMFMins(IC) == 0.0 ) .AND. ( PMFMaxs(IC) == 0.0 ) )  THEN               ! Auto-calculate min and max.
            PMFAutoScl(IC) = .TRUE.
         ELSE
            PMFAutoScl(IC) = .FALSE.
         ENDIF

      ENDDO ! IC

   ENDIF


      !-----  Rainflow Cycles  --------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Rainflow Cycles title line'  )
   CALL ReadVar ( IU, InpFile, NumRFCols, 'NumRFCols', 'the number of rainflow channels' )

   IF ( NumRFCols == 0 )  THEN

      CALL ReadCom ( IU, InpFile, 'the rainflow counting period'      )
      CALL ReadCom ( IU, InpFile, 'the rainflow normalization flag'   )
      CALL ReadCom ( IU, InpFile, 'the rainflow zero-count flag'      )
      CALL ReadCom ( IU, InpFile, 'the number of rainflow range bins' )
      CALL ReadCom ( IU, InpFile, 'the number of rainflow means bins' )
      CALL ReadCom ( IU, InpFile, 'the RF channel-information line'   )

   ELSEIF ( NumRFCols < 0 )  THEN

      CALL ProgAbort ( ' The number of rainflow channels must not be negative.')

   ELSE


         ! Allocate arrays for rainflow information.

      CALL AllocAry ( RF_Cols    , NumRFCols, 'RF_Cols'     )
      CALL AllocAry ( HalfCycMult, NumRFCols, 'HalfCycMult' )
      CALL AllocAry ( MaxMean    , NumRFCols, 'MaxMean'     )
      CALL AllocAry ( MaxRng     , NumRFCols, 'MaxRng'      )
      CALL AllocAry ( MinMean    , NumRFCols, 'MinMean'     )
      CALL AllocAry ( AutoRange  , NumRFCols, 'AutoRange'   )
      CALL AllocAry ( AutoMeans  , NumRFCols, 'AutoMeans'   )


         ! Read in more of the RF data.

      CALL ReadVar ( IU, InpFile, RF_Per    , 'RF_Per'    , 'the rainflow counting period'      )
      CALL ReadVar ( IU, InpFile, RF_Norm   , 'RF_Norm'   , 'the rainflow normalization flag'   )
      CALL ReadVar ( IU, InpFile, RFZC_Blank, 'RFZC_Blank', 'the rainflow zero-count flag'      )
      CALL ReadVar ( IU, InpFile, NumRFRBins, 'NumRFRBins', 'the number of rainflow range bins' )
      CALL ReadVar ( IU, InpFile, NumRFMBins, 'NumRFMBins', 'the number of rainflow means bins' )
      CALL ReadCom ( IU, InpFile,                           'the RF channel-information line'   )

      IF ( RF_Per <= 0 ) THEN
         CALL ProgAbort ( ' Warning: Rainflow cycle counting period must not be zero or negative.' )
      ENDIF


         ! Read in and check rainflow column list.

      DO IC=1,NumRFCols

         IF ( NumRFRBins == 0 )  THEN


               ! Because we are not binning cycles, we only need the channel numbers
               ! and half-cycle multipliers.

            READ (IU,*,IOSTAT=IOS)  RF_Cols(IC), HalfCycMult(IC)


         ELSEIF ( NumRFMBins == 1 )  THEN


               ! We are doing 1-D rainflow cycle counting, so we do not need info about the means.

            READ (IU,*,IOSTAT=IOS)  RF_Cols(IC), HalfCycMult(IC), MaxRng(IC)


         ELSE


               ! We are doing 2-D rainflow cycle counting, we need info about the means.

            READ (IU,*,IOSTAT=IOS)  RF_Cols(IC), HalfCycMult(IC), MaxRng(IC), MinMean(IC), MaxMean(IC)

         ENDIF

         CALL CheckIOS ( IOS, InpFile, 'the information for Rainflow Channel #'//TRIM( Int2LStr( IC ) ), NumType )


            ! Check for valid values.

         IF ( NumRFRBins > 0 )  THEN

            AutoRange(IC) = .FALSE.

            IF ( MaxRng(IC) == 0.0 )  THEN
               AutoRange(IC) = .TRUE.
            ELSEIF ( MaxRng(IC) < 0.0 )  THEN
               CALL ProgAbort ( ' The max range must be >= 0 for rainflow column #'//TRIM( Int2LStr( IC ) )//'.')
            ENDIF

         ENDIF


         IF ( NumRFMBins > 1 )  THEN

            AutoMeans(IC) = .FALSE.

            IF ( ( MinMean(IC) == 0.0 ) .AND. ( MaxMean(IC) == 0.0 ) )  THEN
               AutoMeans(IC) = .TRUE.
            ELSEIF ( MinMean(IC) >= MaxMean(IC) )  THEN
               CALL ProgAbort ( ' The min mean must be < the max mean for rainflow column #'//TRIM( Int2LStr( IC ) )//'.')
            ENDIF

         ENDIF

      ENDDO ! IC


   ENDIF


      !-----  Extreme Events  ---------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Extreme Events title line'               )
   CALL ReadVar ( IU, InpFile, NumEEGrps, 'NumEEGrps', 'the number of extreme-event groups'          )
   CALL ReadCom ( IU, InpFile,                         'the extreme-events channel-information line' )

   IF ( NumEEGrps < 0 )  THEN

      CALL ProgAbort ( ' The number of extreme-event groups must be >= 0.')

   ELSEIF ( NumEEGrps > 0 )  THEN


         ! Allocate arrays for extreme-event information.

      CALL AllocAry ( EE_Names  , NumEEGrps, 'EE_Names'   )
      CALL AllocAry ( NumEECols , NumEEGrps, 'NumEECols'  )
      CALL AllocAry ( NumEEICols, NumEEGrps, 'NumEEICols' )
      CALL AllocAry ( TotEECols , NumEEGrps, 'TotEECols'  )


         ! We're going to waste a little space with this next allocation, but
         ! this should be easier to read and understand than using pointers.
         ! The reason the space is wasted is because not all groups will have
         ! the same number of columns in them.

      CALL AllocAry ( EE_Cols, MaxEECols, NumEEGrps, 'EE_Cols' )


         ! Read in and check extreme-event information list.

      DO IG=1,NumEEGrps

         READ (IU,*,IOSTAT=IOS)  EE_Names(IG), NumEECols (IG), ( EE_Cols(IC,IG), IC=              1,NumEECols(IG) ) &
                                             , NumEEICols(IG), ( EE_Cols(IC,IG), IC=NumEECols(IG)+1,NumEECols(IG)+NumEEICols(IG) )

         CALL CheckIOS ( IOS, InpFile, 'the information for extreme-event group #'//TRIM( Int2LStr( IC ) ), NumType )

         TotEECols(IG) = NumEECols(IG) + NumEEICols(IG)


            ! Check for valid values.

         IF ( NumEECols(IG) < 1 )  THEN
            CALL ProgAbort ( ' The number of columns in extreme-event group #'//TRIM( Int2LStr( IG ) ) &
                       //' must not be negative.')
         ENDIF

         IF ( NumEEICols(IG) < 0 )  THEN
            CALL ProgAbort ( ' The number of information columns in extreme-event group #'//TRIM( Int2LStr( IG ) ) &
                       //' must not be negative.')
         ENDIF

      ENDDO ! IG

   ENDIF


      !-----  Summary Files  ----------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Summary Files title line'       )
   CALL ReadVar ( IU, InpFile, NumSFCols, 'NumSFCols', 'the number of summary-file channels' )

   IF ( NumSFCols == 0 )  THEN

      Out_Sumry = .FALSE.

      CALL ReadCom ( IU, InpFile, 'the list of summary-file channels'       )

   ELSEIF ( NumSFCols > 0 )  THEN


         ! Make sure we're not doing aggregate statistics.  These two features are incompatible.

      IF ( Aggregate )  THEN
         CALL ProgAbort ( ' You cannot choose both aggregate analyses and summary statistics.' )
      ENDIF


         ! Allocate array for SF column list.

      CALL AllocAry ( SF_Cols, NumSFCols, 'SF_Cols' )


         ! Read in SF columns.

      Out_Sumry = .TRUE.

      CALL ReadAry ( IU, InpFile, SF_Cols,  NumSFCols, 'SF_Cols', 'the list of summary-file channels' )

   ELSEIF ( NumSFCols < 0 )  THEN

      CALL ProgAbort ( ' The number of columns for summary files cannot be negative.' )

   ENDIF


      !-----  Statistical Extrapolation  ----------------------------------------------

   CALL ReadCom ( IU, InpFile,                         'the Statistical Extrapolation title line'               )
   CALL ReadVar ( IU, InpFile, NumESCols, 'NumESCols', 'the number of statistical-extrapolation channels'       )
   CALL ReadCom ( IU, InpFile,                         'the statistical-extrapolation channel-information line' )

   IF ( NumESCols == 0 )  THEN

      ExtStats = .FALSE.

   ELSEIF ( NumESCols > 0 )  THEN


         ! Make sure we're not doing aggregate statistics.  These two features are incompatible.

      IF ( Aggregate )  THEN
         CALL ProgAbort ( ' You cannot choose both aggregate analyses and statistical extrapolation.' )
      ENDIF


         ! Allocate arrays for statistical extrapolation.

      CALL AllocAry ( ES_Cols, NumESCols, 'ES_Cols' )
      CALL AllocAry ( ExtHrs , NumESCols, 'ExtHrs'  )
      CALL AllocAry ( Quant  , NumESCols, 'Quant'   )


         ! Read in and check ES columns and extrapolation hours list.

      ExtStats = .TRUE.

      DO IC=1,NumESCols

         READ (IU,*,IOSTAT=IOS)  ES_Cols(IC), ExtHrs(IC), Quant(IC)

         CALL CheckIOS ( IOS, InpFile, 'the information for statistical-extrapolation channel #'//TRIM( Int2LStr( IC ) ), NumType )

         IF ( ExtHrs(IC) < 0.0 )  THEN
            CALL ProgAbort ( ' Extrapolation hours for the #'//TRIM( Int2LStr( IC ) ) &
                       //' summary-file column must not be negative. ')
         ENDIF

         IF ( ( Quant(IC) <= 0.0 ) .OR. ( Quant(IC) >= 1.0) )  THEN
            CALL ProgAbort ( ' The requested quantile for the #'//TRIM( Int2LStr( IC ) ) &
                       //' statistical-extrapolation column must be between 0 and 1'//' (exclusive).')
         ENDIF

      ENDDO ! IC

   ELSEIF ( NumESCols < 0 )  THEN

      CALL ProgAbort ( ' The number of columns for statistical extrapolation cannot be negative.' )

   ENDIF


      !-----  Input Files  ------------------------------------------------------------

   CALL ReadCom ( IU, InpFile,                       'the Input Files title line' )
   CALL ReadVar ( IU, InpFile, NumFiles, 'NumFiles', 'the number of input files'  )


      ! Allocate arrays for file names.

   CALL AllocAry ( FileName, NumFiles, 'FileName' )
   CALL AllocAry ( BadList , NumFiles, 'BadList'  )
   CALL AllocAry ( RootName, NumFiles, 'RootName' )


      ! Get file names.  Parse out their roots.

   DO Fi=1,NumFiles

      CALL ReadVar ( IU, InpFile, FileName(Fi), 'FileName', 'the name of input file #'//TRIM( Int2LStr( FI ) )  )

      NameLen = LEN_TRIM( FileName(Fi) )

      IF ( NameLen == 0 )  THEN
         CALL ProgAbort ( ' Warning.  The name field for input file #'//TRIM( Int2LStr( Fi ) )//' is blank.')
      ENDIF

      CALL GetRoot ( FileName(Fi), RootName(Fi) )

      IF ( NameLen > MaxNLen )  MaxNLen = NameLen

   ENDDO ! Fi

   MaxNLen = MAX( MaxNLen , 9 )


      ! If the files are FAST binary files and the user did not specify the comumn information,
      ! get the channel information from the header of the first file.

   IF ( FASTbin .AND. ( NumCols == 0 ) )  THEN

      FASTdata%File = FileName(1)

      CALL ReadFASTbin ( DU, .TRUE., FASTdata, ErrStat, ErrMsg )

      NumInCols = FASTdata%NumChans + 1


         ! Allocate arrays for column info.

      CALL AllocAry ( Titles , NumInCols+NumCChan, 'Titles'  )
      CALL AllocAry ( Units  , NumInCols+NumCChan, 'Units'   )
      CALL AllocAry ( ColList, NumInCols+NumCChan, 'ColList' )
      CALL AllocAry ( Scal   , NumInCols+NumCChan, 'Scal'    )
      CALL AllocAry ( Offset , NumInCols+NumCChan, 'Offset'  )


         ! Get info for each channel.

      DO IC=1,NumInCols
         Titles (IC) = FASTdata%ChanNames(IC)
         Units  (IC) = FASTdata%ChanUnits(IC)
         ColList(IC) = IC
         Scal   (IC) = 1.0
         Offset (IC) = 0.0
      ENDDO ! IC

      HaveUnits = .TRUE.
      NumCols   = NumInCols

   ENDIF


      ! Close input file.

   CLOSE ( IU )


   RETURN
   END SUBROUTINE GetParams
!=======================================================================
   SUBROUTINE ReadFData ( InFile, GoodFile, Error )


      ! This routine reads the formatted data in file Fi.


   USE                             CrunchSubs
   USE                             DataMod
   USE                             ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: GoodFile
   INTEGER, INTENT(IN)          :: InFile

   LOGICAL, INTENT(OUT)         :: Error


      ! Local declarations.

   REAL(ReKi)                   :: Time                                       ! The time of a given record.

   INTEGER(IntKi)               :: IC
   INTEGER(IntKi)               :: IOS
   INTEGER(IntKi)               :: IR
   INTEGER(IntKi)               :: IXT
   INTEGER(IntKi)               :: KeepRec                                    ! The index of kept records in the ConvData array.



      !  Open data file.

   CALL GetNewUnit   ( DU )
   CALL OpenFInpFile ( DU, FileName(InFile) )


      ! Process the requested data records of this file.

   CALL WrScr1 ( ' =======================================================' )
   CALL WrScr  ( ' Reading in data from file "'//TRIM( FileName(InFile) )//'".' )
   CALL WrScr  ( ' ' )


      ! Skip over the unused records of this file.

   DO IR=1,FirstRec-1

      READ (DU,'()',IOSTAT=IOS)

      IF ( IOS < 0 )  THEN


            ! This file is too short.  Skip it.

         Error = .TRUE.
         CALL UsrAlarm
         CALL WrScr ( ' File "'//TRIM( FileName(InFile) )//'" is being skipped due to a premature end of file on line #' &
                               //TRIM( Int2LStr( IR ) )//'.' )
         RETURN

      ELSEIF ( IOS > 0 )  THEN


            ! We had an error reading this file.  Skip it.

         Error = .TRUE.
         CALL UsrAlarm
         CALL WrScr ( ' File "'//TRIM( FileName(InFile) )//'" is being skipped due to read error #' &
                               //TRIM( Int2LStr( IOS ) )//' on line #' &
                               //TRIM( Int2LStr( IR ) )//'.' )
         RETURN

      ENDIF

   ENDDO ! IR


      ! Read the data.

   KeepRec = 1

   DO IR=1,NumRecs


         ! Read in raw data.  Check for premature end of file.

      READ (DU,*,IOSTAT=IOS)  ( RawData(IC), IC=1,NumInCols )

      IF ( IOS < 0 )  THEN


            ! This file is too short.  Skip it.

         Error = .TRUE.
         CALL UsrAlarm
         CALL WrScr ( ' File "'//TRIM( FileName(InFile) )//'" is being skipped due to a premature end of file on line #' &
                               //TRIM( Int2LStr( IR+FDRow-1 ) )//'.' )
         RETURN

      ELSEIF ( IOS > 0 )  THEN


            ! We had an error reading this file.  Skip it.

         Error = .TRUE.
         CALL UsrAlarm
         CALL WrScr ( ' File "'//TRIM( FileName(InFile) )//'" is being skipped due to read error #' &
                               //TRIM( Int2LStr( IOS ) )//' on line #' &
                               //TRIM( Int2LStr( IR+FDRow-1 ) )//'.' )
         RETURN


            ! If we are using time to determine what to store, check the time against the limits.

         IF ( TEnd > 0.0 ) THEN

            Time = Scal(TimeCol)*RawData(TimeCol) + Offset(TimeCol)

            IF ( Time < TStart )  Cycle
            IF ( Time > TEnd   )  EXIT

         ENDIF

      ENDIF

         ! Print out a status every 10000 records.

      IF ( MOD( IR , 10000 )  .EQ. 0 )  THEN
         CALL WrOver ( '  Reading record #'//TRIM( Int2LStr( IR ) )//'.' )
      ENDIF


         ! Convert and store data in new form.

      DO IC=1,NumCols

         ConvData(IC,KeepRec,GoodFile) = Scal(IC)*RawData(ColList(IC)) + Offset(IC)

      ENDDO ! IC


         ! Remove crosstalk for requested pairs.

      DO IXT=1,NumXT

         CALL RemoveXT ( ConvData(XT_Cols(1,IXT),KeepRec,GoodFile) , ConvData(XT_Cols(2,IXT),KeepRec,GoodFile) , XT(1,1,IXT) )

     ENDDO ! IXT

      KeepRec = KeepRec + 1

   ENDDO


      ! Let's store the start time.

   IF ( InFile == 1 )  TStart = ConvData(TimeCol,1,GoodFile)


   CALL WrOver ( '  '//TRIM( Int2LStr( NumRecs ) )//' records were read.   ' )


      ! Close input file.

   CLOSE ( DU )

   ERROR = .FALSE.


   RETURN
   END SUBROUTINE ReadFData ! ( InFile, GoodFile, Error )
!=======================================================================
   SUBROUTINE WrBadList ( Unit )


      ! This routine writes out the list of flies that had been skipped to
      ! the output file for the calling routine.


   USE                                DataMod


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Unit


      ! Local declarations.

   INTEGER                         :: IBF

   CHARACTER(200)                  :: Frmt



   IF ( BadFiles == 1 )  THEN
      Frmt = "( //, 'WARNING!'" &
            //" //, '   The input file ""' , A , '"" was excluded from this analysis'" &
            //"  /, '   because of an incompatible file format or a read error.' )"
      WRITE (Unit,Frmt)  TRIM( BadList(1) )
   ELSE
      Frmt = "( //, 'WARNING!'" &
            //" //, '   The following input files were excluded from this analysis because'" &
            //"  /, '   of incompatible file formats or read errors:' / )"
      WRITE (Unit,Frmt)
      DO IBF=1,BadFiles
        WRITE (Unit,'(6X,A)')  TRIM( BadList(IBF) )
      ENDDO ! IBF
   ENDIF


   RETURN
   END SUBROUTINE WrBadList ! ( Unit )
!=======================================================================

END MODULE CrunchIO
