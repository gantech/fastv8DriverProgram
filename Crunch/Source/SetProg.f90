!=======================================================================
SUBROUTINE SetProg


!  This routine sets the program name and version number.  By doing it this way instead
!  of the old way of setting it in a DATA statement in Modules.f90, we  will no longer have
!  to recompile everything every time we change versions.


USE             NWTC_Library



ProgName = 'Crunch'
ProgVer  = ' (v3.02.00c-mlb, 25-Jan-2013)'


RETURN
END SUBROUTINE SetProg
