#   Copyright (C) 2014 mdm                                     
#   map[dot]plus[dot]plus[dot]help[at]gmail                                
#                                                              
# Licensed to the Apache Software Foundation (ASF) under one   
# or more contributor license agreements.  See the NOTICE file 
# distributed with this work for additional information        
# regarding copyright ownership.  The ASF licenses this file   
# to you under the Apache License, Version 2.0 (the            
# "License"); you may not use this file except in compliance   
# with the License.  You may obtain a copy of the License at   
#                                                              
#   http://www.apache.org/licenses/LICENSE-2.0                 
#                                                              
# Unless required by applicable law or agreed to in writing,   
# software distributed under the License is distributed on an  
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY       
# KIND, either express or implied.  See the License for the    
# specific language governing permissions and limitations            
# under the License.                                             

MAP_VER:=1.20.10

#COMPILER options - gnu, intel, intelPhi
#COMPILER = intelPhi

#BUILD options - release, debug
#BUILD = release

#LAPACK options - lapack, mkl
#LAPACK = LAPACK

BITS = -m64
PLATFORM = $(shell uname -s)
VPATH = cminpack:simclist:bstring:lapack


# Name of compiler to use and flags to use.
ifeq ($(COMPILER),gnu)
     CC = mpigcc
     CPP = mpig++
else ifeq ($(COMPILER),intel)
     CC = mpiicc
     CPP = mpiicpc
else ifeq ($(COMPILER),intelPhi)
     CC = mpiicc
     CPP = mpiicpc
endif

ifeq ($(BUILD),release)
	OPT_OPTIONS = -O2
else ifeq ($(BUILD),debug)
	OPT_OPTIONS = -O0 -g
endif


ifeq ($(OS),Windows_NT)
  DEL_CMD   = del
  LIB_EXT   = dll
  CFLAGS    = $(BITS) -std=c99 -DMAP_DLL_EXPORTS -DCMINPACK_NO_DLL  -DNDEBUG -D_WINDOWS -D_USRDLL -D_MINGW
  LDFLAGS   = $(BITS) -shared -Wl,--export-all-symbols
  LIB_FLAGS :=
else
  PLATFORM = $(shell uname -s)
  DEL_CMD   = rm -rf
  GIT_VERSION := $(shell git describe --abbrev=4 --dirty --always)

  ifeq ($(PLATFORM),Darwin)
    LIB_EXT   = dylib
    CC_TOOLS  = clang
    CFLAGS    = -g -O1 -fno-omit-frame-pointer -fPIC -D DEBUG -Icminpack -Isimclist
    LDFLAGS   = -g -dynamiclib
    LIB_FLAGS := -lm
  else ifeq ($(PLATFORM),Linux)
    ifeq ($(COMPILER),gnu)
	CC_TOOLS  = gcc
	CFLAGS    = $(BITS) $(OPT_OPTIONS) -fPIC -std=c99 -D DEBUG -DGITVERSION=\"$(GIT_VERSION)\" -D WITH_LAPACK
	LDFLAGS   = $(BITS) -shared
	ifeq ($(LAPACK),lapack)
		LAPACK_LIBS = -L../../../../lib/ -L../../../../lib64/ -llapack -lblas -llapacke -lm 
	else ifeq ($(LAPACK),mkl)
		LAPACK_LIBS = -Wl,-rpath,/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/lib/intel64 -Wl,-rpath,/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/../compiler/lib/intel64 -L/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/lib/intel64 -L/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/../compiler/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm
	endif
    else ifeq ($(COMPILER),intel)
	CC_TOOLS  = icc
	CFLAGS    = $(BITS) $(OPT_OPTIONS) -fPIC -std=c99 -D DEBUG -DGITVERSION=\"$(GIT_VERSION)\" -D WITH_LAPACK
	LDFLAGS   = $(BITS) -shared
	LAPACK_LIBS = -Wl,-rpath,/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/lib/intel64 -Wl,-rpath,/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/../compiler/lib/intel64 -L/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/lib/intel64 -L/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/../compiler/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm
    else ifeq ($(COMPILER),intelPhi)
	CC_TOOLS  = icc
	CFLAGS    = $(BITS) $(OPT_OPTIONS) -mmic -fPIC -std=c99 -D DEBUG -DGITVERSION=\"$(GIT_VERSION)\" -D WITH_LAPACK
	LDFLAGS   = $(BITS) -mmic -shared
	LAPACK_LIBS  = -Wl,-rpath,/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/lib/mic -Wl,-rpath,/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/../compiler/lib/mic -L/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/lib/mic -L/nopt/intel/16.0/compilers_and_libraries_2016.2.181/linux/mkl/../compiler/lib/mic -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm
    endif
    LIB_EXT   = so
    #CFLAGS    = -g -O1 -fsanitize=address -fno-omit-frame-pointer -fPIC -D DEBUG -DGITVERSION=\"$(GIT_VERSION)\" -Icminpack -Isimclist
    #CFLAGS    = -g -fPIC -std=c99 -Wuninitialized -Wall -D DEBUG -DGITVERSION=\"$(GIT_VERSION)\"
    LIB_FLAGS := ${LAPACK_LIBS}
  endif
endif

DEBUG     = 
OBJ       = lmder.o \
	dpmpar.o \
	lmpar.o \
	qrsolv.o \
	enorm.o \
	enorm_u.o \
	qrfac.o \
	simclist.o \
	bstrlib.o \
	bstraux.o \
	freedata.o \
	mapinit.o \
	maperror.o \
	lineroutines.o \
	numeric.o \
	outputstream.o \
	mapapi.o \
	lmroutines.o \
	jacobian.o \
	residual.o \


all : $(OBJ)
	$(CC_TOOLS) $(LDFLAGS) -o ../../../../lib/libmap-$(MAP_VER).$(LIB_EXT) $(DEBUG) $(OBJ) $(LIB_FLAGS) 

.c.o :
	$(CC_TOOLS) -c $(CFLAGS) $<

clean:
	$(DEL_CMD) *.$(LIB_EXT) *.o *~ ../../../../lib/libmap-$(MAP_VER).$(LIB_EXT)

memcheck:	
	valgrind --tool=memcheck --leak-check=yes python -E -tt ./main.py	
