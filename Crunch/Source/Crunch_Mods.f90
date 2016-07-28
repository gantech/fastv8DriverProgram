!=======================================================================
MODULE DataMod


USE                                      NWTC_Library

IMPLICIT                                 NONE


INTEGER, PARAMETER                    :: NOps   =    256
INTEGER, PARAMETER                    :: NConst =    100

TYPE Token
   REAL(DbKi)                         :: Nums        (NConst)
   CHARACTER(1)                       :: Ops         (NOps)
END TYPE Token

TYPE(Token), ALLOCATABLE              :: Equation    (:)

REAL(DbKi), ALLOCATABLE               :: AziSum      (:)
REAL(DbKi), ALLOCATABLE               :: DataSum2    (:)
REAL(DbKi), ALLOCATABLE               :: DataSum3    (:)
REAL(DbKi), ALLOCATABLE               :: DataSum4    (:)
REAL(DbKi), ALLOCATABLE               :: DataSums    (:)

REAL(R8Ki)                            :: TimeStep                       ! The time step of the first file.

REAL(ReKi), ALLOCATABLE               :: AziAver     (:,:,:)
REAL(ReKi), ALLOCATABLE               :: AziStDev    (:,:,:)
REAL(ReKi), ALLOCATABLE               :: AziMean     (:)
REAL(ReKi)                            :: Biggest
REAL(ReKi), ALLOCATABLE               :: ConvData    (:,:,:)
REAL(ReKi), ALLOCATABLE               :: CycMean     (:)
REAL(ReKi), ALLOCATABLE               :: CycMult     (:)
REAL(ReKi), ALLOCATABLE               :: CycRange    (:)
REAL(ReKi), ALLOCATABLE               :: DataMaxs    (:,:)
REAL(ReKi), ALLOCATABLE               :: DataMeans   (:,:)              ! Array of means of the data.
REAL(ReKi), ALLOCATABLE               :: DataMins    (:,:)
REAL(ReKi)                            :: DelAzim
REAL(ReKi)                            :: DelAzim2
REAL(ReKi), ALLOCATABLE               :: ExtHrs      (:)
REAL(ReKi), ALLOCATABLE               :: HalfCycMult (:)
REAL(ReKi)                            :: HiCut
REAL(ReKi), ALLOCATABLE               :: Kurtosis    (:,:)
REAL(ReKi)                            :: LoCut
REAL(ReKi), ALLOCATABLE               :: MAPeriod    (:)                ! Time period of moving averages.
REAL(ReKi), ALLOCATABLE               :: MaxEE       (:,:,:)
REAL(ReKi), ALLOCATABLE               :: MaxMean     (:)
REAL(ReKi), ALLOCATABLE               :: MaxRng      (:)
REAL(ReKi), ALLOCATABLE               :: MeanBinWid  (:)
REAL(ReKi), ALLOCATABLE               :: MeanWS      (:)
REAL(ReKi), ALLOCATABLE               :: MinEE       (:,:,:)
REAL(ReKi), ALLOCATABLE               :: MinMean     (:)
REAL(ReKi), ALLOCATABLE               :: Offset      (:)
REAL(ReKi), ALLOCATABLE               :: PLNegThld   (:)                ! Array of negative thresholds for peak listing.
REAL(ReKi), ALLOCATABLE               :: PLPosThld   (:)                ! Array of positive thresholds for peak listing.
REAL(ReKi), ALLOCATABLE               :: PMFBinVal   (:)                ! Array of PMF bin values (centers of bins).
REAL(ReKi), ALLOCATABLE               :: PMFInvDel   (:)                ! Array of PMF inverse deltas.
REAL(ReKi), ALLOCATABLE               :: PMFMaxs     (:)                ! Array of PMF maxima.
REAL(ReKi), ALLOCATABLE               :: PMFMins     (:)                ! Array of PMF minima.
REAL(ReKi), ALLOCATABLE               :: PMFNorm     (:)                ! Array of PMF normalizing constants.
REAL(ReKi), ALLOCATABLE               :: Quant       (:)
REAL(ReKi), ALLOCATABLE               :: RawData     (:)
REAL(ReKi), ALLOCATABLE               :: RF_Counts   (:,:,:)
REAL(ReKi), ALLOCATABLE               :: RngBinWid   (:)
REAL(ReKi), ALLOCATABLE               :: Scal        (:)
REAL(ReKi), ALLOCATABLE               :: Skewness    (:,:)
REAL(ReKi)                            :: Smallest
REAL(ReKi), ALLOCATABLE               :: StdDev      (:,:)              ! Array of standard deviations of the data.
REAL(ReKi), ALLOCATABLE               :: TrbInt      (:)
REAL(ReKi)                            :: TEnd                           ! The time of the last record to proces from the input files.
REAL(ReKi)                            :: TStart                         ! The time of the first record to proces from the input files.
REAL(ReKi), ALLOCATABLE               :: XFreq       (:,:)
REAL(ReKi), ALLOCATABLE               :: XT          (:,:,:)

INTEGER, ALLOCATABLE                  :: AA_Cols     (:)
INTEGER                               :: AnalRecs
INTEGER                               :: AzimCol
INTEGER                               :: BadFiles                       ! Number of data files that were not read successfully.
INTEGER, ALLOCATABLE                  :: Bin_Cnt     (:)
INTEGER, ALLOCATABLE                  :: ColList     (:)
INTEGER                               :: CTRow                          ! Row of data file that has the channel titles.
INTEGER                               :: CURow                          ! Row of data file that has the channel units.
INTEGER, ALLOCATABLE                  :: EE_Cols     (:,:)
INTEGER, ALLOCATABLE                  :: ES_Cols     (:)
INTEGER                               :: FDRow                          ! First row of data file that has valid data.
INTEGER, ALLOCATABLE                  :: FiltCols    (:)
INTEGER                               :: FiltType
INTEGER                               :: FirstRec                       ! The first record of data.
INTEGER                               :: GoodFiles                      ! Number of data files that were read successfully.
INTEGER, ALLOCATABLE                  :: MAChans     (:)                ! The list of moving-average channels.
INTEGER, PARAMETER                    :: MaxChrLen   = 10               ! The maximum number of characters used for channel titles and units.
INTEGER                               :: MaxEECols   = 20               ! The maximum number of EE columns allowed.
INTEGER, ALLOCATABLE                  :: MaxEEFile   (:,:)
INTEGER                               :: MaxNLen     = 0                ! The length of the longest input file name.
INTEGER, PARAMETER                    :: MaxPathLen  = 1024             ! The declaration size for strings to hold path and file names.
INTEGER, ALLOCATABLE                  :: MinEEFile   (:,:)
INTEGER                               :: NonAACols                      ! Total number of columns, not counting AA columns.
INTEGER                               :: NumAABins
INTEGER                               :: NumAACols
INTEGER                               :: NumCChan
INTEGER                               :: NumCols
INTEGER, ALLOCATABLE                  :: NumCross    (:)
INTEGER, ALLOCATABLE                  :: NumEECols   (:)                ! Number of extreme-event columns.
INTEGER, ALLOCATABLE                  :: NumEEICols  (:)                ! Number of extreme-event informational columns.
INTEGER                               :: NumEEGrps                      ! Number of extreme-event groups.
INTEGER                               :: NumESCols
INTEGER                               :: NumFiles                       ! Number of requested input files (not all are good ones).
INTEGER                               :: NumFilt
INTEGER                               :: NumHead
INTEGER                               :: NumInCols
INTEGER                               :: NumMA                          ! Number of moving averages.
INTEGER                               :: NumPLCh                        ! Number of channels to have their peaks and valleys listed.
INTEGER                               :: NumPMFBins                     ! Number of PMF bins.
INTEGER                               :: NumPFCols
INTEGER                               :: NumRecs
INTEGER                               :: NumRFMBins
INTEGER                               :: NumRFRBins
INTEGER                               :: NumRFCols
INTEGER                               :: NumRoseCh
INTEGER                               :: NumRoses
INTEGER                               :: NumSFCols
INTEGER                               :: NumXT
INTEGER                               :: NumPMF                         ! Number of PMF columns.
INTEGER, ALLOCATABLE                  :: PL_Cols     (:)                ! List of columns to be peak listed.
INTEGER                               :: PL_Meth                        ! Method of finding peaks and valleys.
INTEGER, ALLOCATABLE                  :: PMFCols     (:)                ! List of PMF columns.
INTEGER, ALLOCATABLE                  :: PMFCnt      (:,:)
INTEGER, ALLOCATABLE                  :: PF_Cols     (:)
INTEGER, ALLOCATABLE                  :: RF_Cols     (:)
INTEGER, ALLOCATABLE                  :: RF_Cycles   (:)
INTEGER                               :: RF_Per
INTEGER, ALLOCATABLE                  :: RoseCh00    (:)                ! Channel number for the 0 degree load for a load rose.
INTEGER, ALLOCATABLE                  :: RoseCh90    (:)                ! Channel number for the 00 degree load for a load rose.
INTEGER, ALLOCATABLE                  :: RoseSects   (:)                ! Number of rose sectors.
INTEGER                               :: Seed                           ! Seed for the random number generator.
INTEGER, ALLOCATABLE                  :: SF_Cols     (:)
INTEGER                               :: TimeCol
INTEGER                               :: TotCols                        ! Total number of columns, including AA columns.
INTEGER, ALLOCATABLE                  :: TotEECols   (:)                ! Number of extreme-event columns.
INTEGER                               :: TotRecs
INTEGER                               :: WS_Col
INTEGER, ALLOCATABLE                  :: XT_Cols     (:,:)

LOGICAL                               :: AutoCols                       ! Are we autodetecting the number of input columns? = .NOT. ManCols
LOGICAL, ALLOCATABLE                  :: AutoMeans   (:)
LOGICAL, ALLOCATABLE                  :: AutoRange   (:)
LOGICAL                               :: FASTbin                        ! A flag that indicates if the input data are in FAST binary form.
LOGICAL                               :: HaveUnits                      ! Do we have units?
LOGICAL, ALLOCATABLE                  :: PLMeanThld  (:)                ! Array of flags to use the mean value for the peak-listing thresholds.
LOGICAL, ALLOCATABLE                  :: PLWrPk      (:)                ! Flags indicating we output peaks in the peak lists.
LOGICAL, ALLOCATABLE                  :: PLWrTr      (:)                ! Flags indicating we output troughs in the peak lists.
LOGICAL, ALLOCATABLE                  :: PMFAutoScl  (:)                ! Flags indicating we need to calculate the min/max for PMFs for each file.
LOGICAL                               :: RFZC_Blank                     ! Flag indicating we output a space for RF bins with zero counts.
LOGICAL                               :: RF_Norm
LOGICAL                               :: UseTime                        ! Use time is used to limit input data?
LOGICAL                               :: WrPLtime                       ! Include time in the peak lists?

CHARACTER(100), ALLOCATABLE           :: BadList     (:)                ! The list of files that could not be processed.
CHARACTER(MaxChrLen), ALLOCATABLE     :: CCTitles    (:)                ! Titles for calculated channels when we autodetect channels.
CHARACTER(MaxChrLen), ALLOCATABLE     :: CCUnits     (:)                ! Units for calculated channels when we autodetect channels.
CHARACTER(100), ALLOCATABLE           :: EE_Names    (:)
CHARACTER(200), ALLOCATABLE           :: EqnStr      (:)
CHARACTER(MaxPathLen), ALLOCATABLE    :: FileName    (:)                ! A string to hold a file name
CHARACTER(MaxChrLen), ALLOCATABLE     :: MATitles    (:)                ! Titles for the new moving-average channels.
CHARACTER( 50)                        :: PLFrmt                         ! Format for peak-list files.
CHARACTER(MaxPathLen), ALLOCATABLE    :: RootName    (:)
CHARACTER(MaxChrLen), ALLOCATABLE     :: RoseName    (:)                ! Root name for load rose column titles.
CHARACTER(100)                        :: SumForm
CHARACTER(MaxChrLen), ALLOCATABLE     :: Titles      (:)                ! Names of the channels.
CHARACTER(MaxChrLen), ALLOCATABLE     :: Units       (:)                ! Units of the channels.


END MODULE DataMod
!=======================================================================
MODULE ProgGen


IMPLICIT                          NONE

INTEGER                         :: AU                                         ! I/O unit for azimuth-averages file.
INTEGER                         :: DU                                         ! I/O unit for input-data files.
INTEGER                         :: EU                                         ! I/O unit for extreme-event files.
INTEGER                         :: HU                                         ! I/O unit for probability-density files.
INTEGER                         :: IU                                         ! I/O unit for parameter-information files.
INTEGER                         :: OU                                         ! I/O unit for modified-data output files.
INTEGER                         :: PU                                         ! I/O unit for peak-list files.
INTEGER, ALLOCATABLE            :: PUC               (:)                      ! I/O unit offset for individual peak-list files.
INTEGER                         :: RU                                         ! I/O unit offset for the binned rainflow file.
INTEGER, ALLOCATABLE            :: RUC               (:)                      ! I/O unit offset for raw rainflow-cycle files.
INTEGER                         :: SU                                         ! I/O unit for xxxxx files.
INTEGER                         :: TextLen

LOGICAL                         :: Aggregate
LOGICAL                         :: Do_PF
LOGICAL                         :: ExtStats
LOGICAL                         :: Out_AA
LOGICAL                         :: Out_Data
LOGICAL                         :: Out_Sumry
LOGICAL                         :: Out_Stats
LOGICAL                         :: TabDelim

CHARACTER(1024)                 :: AggRoot
CHARACTER(  11)                 :: DateStr
CHARACTER(1024)                 :: InpFile  = ''                              ! The primary parameter input file.
CHARACTER( 256)                 :: JobTitle
CHARACTER(   1), PARAMETER      :: NullChar = CHAR( 0 )
CHARACTER(1024)                 :: ParRoot                                    ! Root name of the parameter input file.
CHARACTER(  13)                 :: RealFmt
CHARACTER(   9)                 :: TextFmt  = '(  X,A10)'
CHARACTER(   9)                 :: TextFmtI = '(  X,A12)'
CHARACTER(   8)                 :: TimeStr


END MODULE ProgGen
!=======================================================================
