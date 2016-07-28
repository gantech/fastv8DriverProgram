#================================================================================#
# This makefile was last updated by M. Buhl on 25-Jan-2013.                      #
#                                                                                #
# This makefile has been tested with:                                            #
#    Windows 7                                                                   #
#       GNU Make3.81                                                             #
#       gfortran v4.6.1                                                          #
#    Ubuntu 12.04.0 LTS                                                          #
#       GNU Make3.82                                                             #
#       gfortran v4.6.3                                                          #
#                                                                                #
# It was designed to be used with:                                               #
#     Crunch                  (v3.02.00b-mlb, 18-Jan-2013)                       #
#     NWTC Subroutine Library (v1.07.00c-mlb, 25-Jan-2013)                       #
#                                                                                #
# Older versions of Crunch and the NWTC Library may not work with this makefile. #
#================================================================================#

   # Windows or Linux?  32-bit or 64-bit?

BITS = 32
BITS = 64

   # Location of source files for Crunch and the NWTC Library.  You will probably need to change these for your system.

ifeq ($(OS),Windows_NT)
   LIB_DIR = M:/CAEtools/Miscellaneous/NWTC_Library/trunk/source
else
   LIB_DIR = $(HOME)/PC/CAEtools/Miscellaneous/NWTC_Library/trunk/source
endif

CRUNCH_DIR = Source

   # Name of compiler to use and flags to use.

FC     = gfortran
FFLAGS = -O3 -m$(BITS) -fbacktrace

   # Precision.

# Use "SingPrec" for single precision and "DoubPrec" for double precision.  You may also need to change an option switch to make constants DP.
PREC = SingPrec

   #==========================================================#
   # You should not need to change anything beyond this point #
   #==========================================================#


   # System-specific settings.

ifeq ($(OS),Windows_NT)
      # Windows
   DEL_CMD   = del
   EXE_EXT   = _gwin$(BITS).exe
   INTER_DIR = Obj_win$(BITS)
   MD_CMD    = @mkdir
   OBJ_EXT   = .obj
   PATH_SEP  = \\
   SYS_FILE  = SysGnuWin
else
      # Linux
   DEL_CMD   = rm -f
   EXE_EXT   = _glin$(BITS)
   INTER_DIR = Obj_lin$(BITS)
   MD_CMD    = @mkdir -p
   OBJ_EXT   = .o
   PATH_SEP  = /
   SYS_FILE  = SysGnuLinux
endif

   # Destination for executable.

DEST_DIR = .

   # Library files.

LIB_SOURCES =         \
	$(PREC).f90        \
	$(SYS_FILE).f90    \
	NWTC_IO.f90        \
	NWTC_Num.f90       \
	ModMesh.f90        \
	NWTC_Aero.f90      \
	NWTC_Library.f90

CRUNCH_SOURCES   =    \
	AzimAver.f90       \
	CalcChan.f90       \
	Crunch.f90         \
	Crunch_IO.f90      \
	Crunch_Mods.f90    \
	Crunch_Subs.f90    \
	ExtEvent.f90       \
	ExtrValu.f90       \
	Filter.f90         \
	LoadRose.f90       \
	MoveAver.f90       \
	Parser.f90         \
	PeakList.f90       \
	PMF.f90            \
	Rainflow.f90       \
	SetProg.f90        \
	Stats.f90          \

vpath %.f90 $(LIB_DIR) $(CRUNCH_DIR)
vpath %.mod $(INTER_DIR)
vpath %.obj $(INTER_DIR)

LIB_OBJS    = $(LIB_SOURCES:.f90=.obj)
CRUNCH_OBJS = $(CRUNCH_SOURCES:.f90=.obj)

   # Rule to do everything.

all:     default
default: $(INTER_DIR) $(DEST_DIR)/Crunch$(EXE_EXT)

   # General rule for making the files.

%.obj: %.f90
	$(FC) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR)

   #  Dependency rules.

$(SYS_FILE).obj:  $(PREC).obj
ModMesh.obj:      $(PREC).obj
NWTC_IO.obj:      $(SYS_FILE).obj
NWTC_Num.obj:     NWTC_IO.obj
NWTC_Aero.obj:    NWTC_IO.obj      NWTC_Num.obj
NWTC_Library.obj: NWTC_Aero.obj    ModMesh.obj
AzimAver.obj:     Crunch_IO.obj    Crunch_Mods.obj
CalcChan.obj:     Crunch_Mods.obj  Parser.obj
Crunch.obj:       AzimAver.obj     CalcChan.obj     Crunch_IO.obj    Crunch_Mods.obj Crunch_Subs.obj ExtEvent.obj ExtrValu.obj Filter.obj LoadRose.obj MoveAver.obj PeakList.obj PMF.obj Rainflow.obj Stats.obj
Crunch_IO.obj:    Crunch_Mods.obj  Crunch_Subs.obj  NWTC_Library.obj
Crunch_Mods.obj:  NWTC_Library.obj
Crunch_Subs.obj:  Crunch_Mods.obj  NWTC_Library.obj
ExtEvent.obj:     Crunch_IO.obj    Crunch_Mods.obj  NWTC_Library.obj
ExtrValu.obj:     Crunch_IO.obj    Crunch_Mods.obj  NWTC_Library.obj
Filter.obj:       Crunch_Mods.obj
LoadRose.obj:     Crunch_Mods.obj  Parser.obj
MoveAver.obj:     Crunch_Mods.obj  Parser.obj
Parser.obj:       Crunch_Mods.obj  $(SYS_FILE).obj
PeakList.obj:     Crunch_IO.obj    Crunch_Mods.obj
PMF.obj:          Crunch_IO.obj    Crunch_Mods.obj
Rainflow.obj:     Crunch_IO.obj    Crunch_Mods.obj
SetProg.obj:      NWTC_Library.obj
Stats.obj:        Crunch_IO.obj    Crunch_Mods.obj

#Crunch$(EXE_EXT): Crunch.obj

   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)

   # For compiling Crunch.

$(DEST_DIR)/Crunch$(EXE_EXT): $(LIB_OBJS) $(CRUNCH_OBJS) | $(INTER_DIR)
	$(FC) $(FFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/Crunch$(EXE_EXT) \
	$(foreach src, $(LIB_OBJS), $(addprefix $(INTER_DIR)/,$(src))) \
	$(foreach src, $(CRUNCH_OBJS), $(addprefix $(INTER_DIR)/,$(src)))

   # Cleanup afterwards.

clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj

