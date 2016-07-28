MODULE CCSubs

   ! This module contains the routines needed to compute calculated channels.

IMPLICIT NONE

CONTAINS

!=======================================================================
   SUBROUTINE AddCC


      ! This routine adds the titles and units of the calculated channels
      ! to the main arrays.


   USE                                DataMod
   USE                                ParsMod


      ! Local declarations.

   INTEGER                         :: IC
   INTEGER                         :: Sttus

   CHARACTER( 10), ALLOCATABLE     :: TmpTitles    (:)
   CHARACTER( 10), ALLOCATABLE     :: TmpUnits     (:)



      ! Reallocate arrays for titles and units to make room for the calculated channels.

   ALLOCATE ( TmpTitles(NumCols) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for TmpTitles array.' )
   ENDIF


   IF ( HaveUnits )  THEN

      ALLOCATE ( TmpUnits(NumCols) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for TmpUnits array.' )
      ENDIF

   ENDIF


   DO IC=1,NumCols

      TmpTitles(IC) = Titles(IC)

      IF ( HaveUnits )  TmpUnits (IC) = Units (IC)

   ENDDO ! IC


   DEALLOCATE ( Titles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for Titles array.' )
   ENDIF

   ALLOCATE ( Titles(NumCols+NumCChan) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for enlarged Titles array.' )
   ENDIF


   IF ( HaveUnits )  THEN

      DEALLOCATE ( Units , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for Units array.' )
      ENDIF

      ALLOCATE ( Units(NumCols+NumCChan) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for enlarged Units array.' )
      ENDIF

   ENDIF

   DO IC=1,NumCols

      Titles(IC) = TmpTitles(IC)

      IF ( HaveUnits )  Units (IC) = TmpUnits(IC)

   ENDDO ! IC


   DEALLOCATE ( TmpTitles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for TmpTitles array.' )
   ENDIF


   IF ( HaveUnits )  THEN

      DEALLOCATE ( TmpUnits , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for TmpUnits array.' )
      ENDIF

   ENDIF


   DO IC=1,NumCChan

      Titles(NumCols+IC) = CCTitles(IC)
      IF ( HaveUnits )  Units(NumCols+IC) = CCUnits(IC)

   ENDDO


   RETURN
   END SUBROUTINE AddCC
!=======================================================================
   SUBROUTINE CalcChan


      ! This routine generates the calculated channels for all files.


   USE                                DataMod
   USE                                ParsMod


      ! Local declarations.

   REAL(Dbki)                      :: Value

   INTEGER                         :: IC
   INTEGER                         :: IFi
   INTEGER                         :: IR

   LOGICAL                         :: Error

   CHARACTER(80)                   :: ErrStr



      ! Tell 'em why we're here.

   CALL WrScr ( '  Generating calculated channels.' )


   DO IFi=1,GoodFiles

      DO IR=1,NumRecs

         DO IC=1,NumCChan

            CALL GetValue( Equation(IC) , IR , IFi , Value , Error , ErrStr )

            IF ( Error )  THEN
               CALL WrScr1 ( ErrStr )
               CALL ProgAbort ( ' The error occurred for calculated channel #'//TRIM( Int2LStr( IC ) ) &
                          //', row #'//TRIM( Int2LStr( IR ) )//', file "'//TRIM( FileName(IFi) )//'".' )
            ELSE
               ConvData(NumCols+IC,IR,IFi) = Value
            ENDIF

         ENDDO ! IC

      ENDDO ! IR

   ENDDO ! IFi


   RETURN
   END SUBROUTINE CalcChan
!=======================================================================
   SUBROUTINE CC_Init ( IC )


      ! This routine tests and tokenizes all the equations for the calculated
      ! channels.


   USE                                DataMod
   USE                                ProgGen
   USE                                ParsMod


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: IC


      ! Local declarations.

   REAL(ReKi)                      :: DummyRN

   INTEGER                         :: ErrIndex
   INTEGER                         :: ICh
   INTEGER, ALLOCATABLE            :: RandSeed (:)
   INTEGER                         :: SeedSize
   INTEGER                         :: StrLen

   LOGICAL                         :: Error

   CHARACTER(100)                  :: ErrStr



      ! Initialize the random number generator in case it gets used.

   CALL RANDOM_SEED   ( SIZE=SeedSize )
   ALLOCATE( RandSeed(SeedSize) )
   RandSeed(:) = 0
   RandSeed(1) = Seed
   CALL RANDOM_SEED   ( PUT=RandSeed )
   CALL RANDOM_NUMBER ( DummyRN )


      ! Be sure that the expression is null-terminated

   StrLen = LEN_TRIM( EqnStr(IC) )
   EqnStr(IC)(StrLen+1:StrLen+1) = NullChar


      ! Tokenize the equation.

   CALL Tokenize ( EqnStr(IC), Equation(IC), NumCols+IC-1, Error, ErrIndex, ErrStr )

   IF ( Error )  THEN

      CALL WrScr1 ( ' There was an error in the expression for calculated-channel #'//TRIM( Int2LStr( IC ) )//'.' )
      CALL WrScr  ( TRIM( ErrStr ) )
      CALL WrScr  ( ' The error occured at or near the "^" below:' )
      CALL WrScr  ( '   '//TRIM( EqnStr(IC) ) )

      ErrStr = ' '

      DO ICh=1,ErrIndex-1
         ErrStr(ICh:ICh) = '.'
      ENDDO ! ICh

      ErrStr(ErrIndex:ErrIndex) = '^'

      CALL ProgAbort ( ' ..'//ErrStr(1:ErrIndex) )

   ENDIF


   RETURN
   END SUBROUTINE CC_Init ! ( IC )
!=======================================================================

END MODULE CCSubs
