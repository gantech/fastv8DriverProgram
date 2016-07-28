MODULE MoveAverSubs

   ! This module contains the routines needed to compute moving averages.

CONTAINS

!=======================================================================
   SUBROUTINE AddMAChans


      ! This routine adds information for the moving averages to the titles and units arrays.


   USE                                DataMod


      ! Local declarations.

   INTEGER                         :: Col
   INTEGER                         :: IC
   INTEGER                         :: IMA
   INTEGER                         :: Sttus

   CHARACTER(10), ALLOCATABLE      :: TmpTitles    (:)
   CHARACTER(10), ALLOCATABLE      :: TmpUnits     (:)



      ! Check the validity of the input data.

   DO IMA=1,NumMA

      IF ( ( MAChans(IMA) < 1 ) .OR. ( MAChans(IMA) > NumCols+NumCChan ) )  THEN
         CALL ProgAbort ( ' The column for the #'//TRIM( Int2LStr( IMA ) ) &
                    //' moving average must be between 1 and '//TRIM( Int2LStr( NumCols+NumCChan ) )//' (inclusive).')
      ENDIF

   ENDDO ! IMA


      ! Reallocate arrays for titles and units to make room for the new channels.

   ALLOCATE ( TmpTitles(NumCols+NumCChan) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the TmpTitles array in AddMAChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      ALLOCATE ( TmpUnits(NumCols+NumCChan) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the TmpUnits array in AddMAChans().' )
      ENDIF

   ENDIF

   DO IC=1,NumCols+NumCChan
      TmpTitles(IC) = Titles(IC)
      IF ( HaveUnits )  TmpUnits(IC) = Units(IC)
   ENDDO ! IC


      ! Deallocate the old arrays.

   DEALLOCATE ( Titles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for the Titles array in AddMAChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      DEALLOCATE ( Units , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the Units array in AddMAChans().' )
      ENDIF

   ENDIF


      ! Reallocate arrays to add room for the rose channels.

   ALLOCATE ( Titles(NumCols+NumCChan+NumMA) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the enlarged Titles array in AddMAChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      ALLOCATE ( Units(NumCols+NumCChan+NumMA) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the enlarged Units array in AddMAChans().' )
      ENDIF

   ENDIF


      ! Move old data into new arrays.  Deallocate the temporary arrays.

   DO IC=1,NumCols+NumCChan
      Titles(IC) = TmpTitles(IC)
      IF ( HaveUnits )  Units(IC) = TmpUnits(IC)
   ENDDO ! IC

   DEALLOCATE ( TmpTitles , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error deallocating memory for the TmpTitles array in AddMAChans().' )
   ENDIF

   IF ( HaveUnits )  THEN

      DEALLOCATE ( TmpUnits , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the TmpUnits array in AddMAChans().' )
      ENDIF

   ENDIF


      ! Create titles and units for new channels.

   Col = NumCols + NumCChan

   DO IMA=1,NumMA

      Titles(Col+IMA) = MATitles(IMA)

      IF ( HaveUnits )  Units(Col+IMA) = Units(MAChans(IMA))

   ENDDO ! IMA


      ! Deallocate the titles array, which we no longer need.

   DEALLOCATE ( MATitles )


   RETURN
   END SUBROUTINE AddMAChans
!=======================================================================
   SUBROUTINE MoveAver


      ! This routine calculate the moving average of one channel to create
      ! a new channel.


   USE                                DataMod
   USE                                ProgGen


      ! Local declarations.

   REAL(Dbki)                      :: Aver         ! The moving average.  Use RK8 for precision.
   REAL(Dbki)                      :: RecsInv      ! The inverse of the number of records the the new average.  Use RK8 for precision.

   REAL                            :: DelTime      ! The time step.

   INTEGER                         :: Fi           ! The number of the current file.
   INTEGER                         :: IMA          ! The number of the current moving average.
   INTEGER                         :: MARecs       ! The number of records for the current moving average.
   INTEGER                         :: IR           ! The number of the current record.
   INTEGER                         :: OrigCh       ! The number of the channel being averaged.
   INTEGER                         :: NewCh        ! The number of the channel containing the moving average.
   INTEGER                         :: RecsOld      ! The number of records currently in the old moving average.



      ! Calculate the time step.

   DelTime = ConvData(TimeCol,2,1) - ConvData(TimeCol,1,1)


      ! Calculate all the moving averages.

   DO IMA=1,NumMA


         ! Calculate the number of records in this moving average and the channel
         ! numbers.

      MARecs = NINT( MAPeriod(IMA)/DelTime )
      OrigCh = MAChans(IMA)
      NewCh  = NumCols + NumCChan + IMA


         ! Loop through all files.

      DO Fi=1,NumFiles


            ! Initialize the first point.  Loop through the records within this file.

         ConvData(NewCh,1,Fi) = ConvData(OrigCh,1,Fi)
         Aver                 = ConvData(OrigCh,1,Fi)

         DO IR=2,NumRecs


               ! Calculate the moving average.  Use the number of points currently
               ! in the moving average with a maximum of AverRecs.

            IF ( IR <= MARecs )  THEN
               RecsInv = 1.0_8/IR
               RecsOld = IR - 1
            ELSE
             RecsOld = RecsOld
            ENDIF

            Aver = RecsInv*( Aver*RecsOld + ConvData(OrigCh,IR,Fi) )

            ConvData(NewCh,IR,Fi) = Aver

         ENDDO ! IR

      ENDDO ! Fi

   ENDDO ! IMA


      ! Deallocate the moving-average arrays.

   DEALLOCATE ( MAChans  )
   DEALLOCATE ( MAPeriod )


   RETURN
   END SUBROUTINE MoveAver
!=======================================================================

END MODULE MoveAverSubs
