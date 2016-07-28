MODULE FilterSubs

   ! This module contains routines to filter data.

CONTAINS

!=======================================================================
   SUBROUTINE Filter ( Fi )


      ! This routine implements a general bandpass digital filter.


      ! This routine was originally coded by Norm Weaver of InterWeaver Consulting
      ! for the NWTC.  It has since been converted to free-form Fortran and
      ! ported to Crunch by Marshall Buhl of the NWTC.


      ! The formulation follows the bilinear infinite-impulse recursive (IIR)
      ! filter described in Numerical Recipes, 2nd ed. on pages 553-555.

      ! This recursive filter is stable with a somewhat rounded and asymmetric
      ! filter response below and above the user selected cutoff frequencies.
      ! The data is filtered twice (forward and backward) to remove phase shift.

      ! The filter formulation assumes evenly spaced data, with time in the
      ! first column.

      ! The response function of the filter is defined by the even function:

      !    | H(f) |^2 = (w^2 / (w^2 + a^2)) * (b^2 / (w^2 + b^2))

      !     where the parameter "w" is defined by the change of variable:

      !        w = tan( Pi * freq. * timestep )

      !     and "a" and "b" are the low and high cutoff frequencies.

      !        If freq. "a" is zero this is a low-pass filter.
      !        If freq. "b" is equal to the Nyquist freq., this is a high-pass
      !        filter.

      ! The solution for stable poles of this function define the following
      ! discrete filter coefficients and corresponding recursion relation:

      !    c1 = -b/((1+a)*(1+b))
      !    c2 = -c1
      !    d1 = ((1+a)*(1-b)+(1-a)*(1+b))/((1+a)*(1+b))
      !    d2 = =-((1-a)*(1-b))/((1+a)*(1+b))

      !    Y(n) = c1*X(n) + c2*X(n-2) + d1*Y(n-1) + d2*Y(n-2)



   USE                                DataMod
   USE                                ProgGen


      ! Argument declarations.

   INTEGER, INTENT(IN)             :: Fi


      ! Local declarations.

   REAL(ReKi)                      :: A
   REAL(ReKi)                      :: B
   REAL(ReKi)                      :: C          (2)
   REAL(ReKi)                      :: D          (2)
   REAL(ReKi)                      :: Nyquist
   REAL(ReKi), ALLOCATABLE         :: FiltTemp   (:,:)

   INTEGER                         :: FC
   INTEGER                         :: IC
   INTEGER                         :: IR
   INTEGER                         :: Sttus



      ! Tell user we a filtering the data.

   CALL WrScr ( '  Filtering selected columns.' )


      ! Let's set other frequencies for low-pass and high-pass filters.

   TimeStep = ( ConvData(TimeCol,NumRecs,Fi) - ConvData(TimeCol,1,Fi) )/( NumRecs - 1 )
   Nyquist  = 1.0/( 2.0*TimeStep )


      ! Check to make sure the cut-off frequencies were valid.

   SELECT CASE ( FiltType )

      CASE ( 1 )

         LoCut = 0.0
         IF ( HiCut > Nyquist )  THEN
            CALL ProgAbort ( ' The high cut-off frequency must be less than the Nyquist frequency, ' &
                       //TRIM( Flt2Lstr( Nyquist ) )//' Hz.')
         ENDIF

      CASE ( 2 )

         HiCut = Nyquist
         IF ( LoCut > Nyquist )  THEN
            CALL ProgAbort ( ' The low cut-off frequency must be less than the Nyquist frequency, ' &
                       //TRIM( Flt2Lstr( Nyquist ) )//' Hz.')
         ENDIF


      CASE ( 3 )

         IF ( HiCut > Nyquist )  THEN
            CALL ProgAbort ( ' The high cut-off frequency must be less than the Nyquist frequency, ' &
                       //TRIM( Flt2Lstr( Nyquist ) )//' Hz.')
         ENDIF

   ENDSELECT


      ! Allocate temporary array space

   ALLOCATE ( FiltTemp(TotCols,NumRecs) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FiltTemp array.' )
   ENDIF


      ! Compute the filter coefficients

   A = TAN( Pi*LoCut*TimeStep )
   B = TAN( Pi*HiCut*TimeStep )

   C(1) = -B/( ( 1 + A )*( 1 + B ) )
   C(2) = -C(1)
   D(1) =  ( ( 1 + A )*( 1 - B ) + ( 1 - A )*( 1 + B ) )/( ( 1 + A )*( 1 + B ) )
   D(2) = -( ( 1 - A )*( 1 - B ) )/( ( 1 + A )*( 1 + B ) )


      ! Filter selected columns.

   DO IC=1,NumFilt

      FC = FiltCols(IC)


         ! Apply forward filter pass.
         !    Recursion Relation:
         !       Y(n) = c(1)*X(n) + c(2)*X(n-2) + d(1)*Y(n-1) + d(2)*Y(n-2)
         !    Compute the filtered value.  Store it in the temporary array.

      FiltTemp(FC,1) = C(1)*ConvData(FC,1,Fi)
      FiltTemp(FC,2) = C(1)*ConvData(FC,2,Fi) + D(1)*FiltTemp(FC,1)

      DO IR=3,NumRecs
         FiltTemp(FC,IR) = C(1)*ConvData(FC,IR,Fi) + C(2)*ConvData(FC,IR-2,Fi) &
                         + D(1)*FiltTemp(FC,IR-1 ) + D(2)*FiltTemp(FC,IR-2   )
      ENDDO


         ! Compute the filtered value.  Store it in the original array.HC,IR,Fil
         ! Apply reverse filter pass.

      ConvData(FC,NumRecs  ,Fi) = C(1)*FiltTemp(FC,NumRecs)
      ConvData(FC,NumRecs-1,Fi) = C(1)*FiltTemp(FC,NumRecs-1) + D(1)*ConvData(FC,NumRecs,Fi)

      DO IR=NumRecs-2,1,-1
         ConvData(FC,IR,Fi) = C(1)*FiltTemp(FC,IR     ) + C(2)*FiltTemp(FC,IR+2   ) &
                            + D(1)*ConvData(FC,IR+1,Fi) + D(2)*ConvData(FC,IR+2,Fi)
      ENDDO

   ENDDO ! IC


      ! Release FiltTemp storage.

   DEALLOCATE( FiltTemp )


   RETURN
   END SUBROUTINE Filter ! ( Fi )
!=======================================================================

END MODULE FilterSubs
