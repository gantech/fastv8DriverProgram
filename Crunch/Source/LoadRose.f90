MODULE LRSubs

   ! This module contains routines to calculate load roses.

CONTAINS

!=======================================================================
   SUBROUTINE AddRoseChans


      ! This routine adds information for the load roses to the titles and units arrays.


   USE                             DataMod


      ! Local declarations.

   INTEGER                      :: Col
   INTEGER                      :: IC
   INTEGER                      :: IR
   INTEGER                      :: RC
   INTEGER                      :: RNL
   INTEGER                      :: Sttus

   CHARACTER(10), ALLOCATABLE   :: TmpTitles    (:)
   CHARACTER(10), ALLOCATABLE   :: TmpUnits     (:)



      ! Check the validity of the input data.

   DO IR=1,NumRoses

      IF ( ( RoseCh00(IR) < 1 ) .OR. ( RoseCh00(IR) > NumCols+NumCChan+NumMA ) )  THEN
         CALL ProgAbort ( ' The column for the 0 degree load in load rose #'//TRIM( Int2LStr( IR ) ) &
                    //' must be between 1 and '//TRIM( Int2LStr( NumCols+NumCChan+NumMA ) )//' (inclusive).')
      ENDIF

      IF ( ( RoseCh90(IR) < 1 ) .OR. ( RoseCh90(IR) > NumCols+NumCChan+NumMA ) )  THEN
         CALL ProgAbort ( ' The column for the 0 degree load in load rose #'//TRIM( Int2LStr( IR ) ) &
                    //' must be between 1 and '//TRIM( Int2LStr( NumCols+NumCChan+NumMA ) )//' (inclusive).')
      ENDIF

      IF ( HaveUnits )  THEN
         IF ( Units(RoseCh00(IR)) /= Units(RoseCh90(IR)) )  THEN
            CALL ProgAbort ( ' For load rose #'//TRIM( Int2LStr( IR ) ) &
                       //', the units for channel #'//TRIM( Int2LStr( RoseCh00(IR) ) ) &
                       //' are not the same as for channel #'//TRIM( Int2LStr( RoseCh90(IR) ) )//'.')
         ENDIF
      ENDIF

   ENDDO ! IR


      ! Reallocate arrays for titles and units to make room for the new channels.

   ALLOCATE ( TmpTitles(NumCols+NumCChan+NumMA) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the TmpTitles array in AddRoseChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      ALLOCATE ( TmpUnits(NumCols+NumCChan+NumMA) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the TmpUnits array in AddRoseChans().' )
      ENDIF

   ENDIF

   DO IC=1,NumCols+NumCChan+NumMA
      TmpTitles(IC) = Titles(IC)
      IF ( HaveUnits )  TmpUnits (IC) = Units (IC)
   ENDDO ! IC


      ! Deallocate the old arrays.

   DEALLOCATE ( Titles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for the Titles array in AddRoseChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      DEALLOCATE ( Units , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the Units array in AddRoseChans().' )
      ENDIF

   ENDIF


      ! Reallocate arrays to add room for the rose channels.

   ALLOCATE ( Titles(NumCols+NumCChan+NumMA+NumRoseCh) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the enlarged Titles array in AddRoseChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      ALLOCATE ( Units(NumCols+NumCChan+NumMA+NumRoseCh) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the enlarged Units array in AddRoseChans().' )
      ENDIF

   ENDIF


      ! Move old data into new arrays.  Deallocate the temporary arrays.

   DO IC=1,NumCols+NumCChan+NumMA
      Titles(IC) = TmpTitles(IC)
      IF ( HaveUnits )  Units(IC) = TmpUnits (IC)
   ENDDO ! IC

   DEALLOCATE ( TmpTitles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for the TmpTitles array in AddRoseChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      DEALLOCATE ( TmpUnits , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the TmpUnits array in AddRoseChans().' )
      ENDIF

   ENDIF


      ! Create titles and units for new channels.

   Col = NumCols + NumCChan + NumMA

   DO IR=1,NumRoses

      DO RC=1,RoseSects(IR)

         Col         = Col + 1
         RNL         = LEN_TRIM( RoseName(IR) )
         Titles(Col) = RoseName(IR)

         IF ( HaveUnits )  Units(Col) = Units(RoseCh00(IR))

         WRITE (Titles(Col)(RNL+1:RNL+2),'(I2.2)')  RC

      ENDDO ! RC

   ENDDO ! IR


   RETURN
   END SUBROUTINE AddRoseChans
!=======================================================================
   SUBROUTINE LoadRose


      ! This routine generates load roses.


   USE                                DataMod
   USE                                ParsMod


      ! Local declarations.

   REAL(ReKi)                      :: Angle
   REAL(ReKi)                      :: CosAng
   REAL(ReKi)                      :: SinAng

   INTEGER                         :: Col
   INTEGER                         :: IFi
   INTEGER                         :: IR
   INTEGER                         :: RC00
   INTEGER                         :: RC90
   INTEGER                         :: Rec
   INTEGER                         :: RS



      ! Tell 'em why we're here.

   CALL WrScr1 ( ' =======================================================' )
   CALL WrScr  ( ' Generating load roses.' )


      ! Generate the roses.

   Col = NumCols + NumCChan + NumMA

   DO IR=1,NumRoses

      DO RS=1,RoseSects(IR)

         Angle  = Pi*( RS - 0.5 )/RoseSects(IR)
         CosAng = COS( Angle )
         SinAng = SIN( Angle )
         RC00   = RoseCh00(IR)
         RC90   = RoseCh90(IR)
         Col    = Col + 1

         DO IFi=1,GoodFiles

           DO Rec=1,NumRecs

               ConvData(Col,Rec,IFi) = ConvData(RC00,Rec,IFi)*CosAng + ConvData(RC90,Rec,IFi)*SinAng

            ENDDO ! Rec

         ENDDO ! IFi

      ENDDO ! RS

   ENDDO ! IR


   RETURN
   END SUBROUTINE LoadRose
!=======================================================================

END MODULE LRSubs
