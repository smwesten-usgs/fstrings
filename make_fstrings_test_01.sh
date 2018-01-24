#!/bin/sh

export GCC_VERSION=7.2.0
# set CMAKE-related and build-related variables
export GCCLIST=$( glocate gcc-7 | grep Cellar | grep bin | grep $GCC_VERSION )
export GCCARR=($GCCLIST)
export GCC=${GCCARR[1]}
export GFORTRANLIST=$( glocate gfortran-7 | grep Cellar | grep bin | grep $GCC_VERSION )
export GFORTRANARR=($GFORTRANLIST)
export GFORTRAN=${GFORTRANARR[1]}

export GPP=$( glocate g++-7 | grep Cellar | grep bin | grep $GCC_VERSION )

export FC=$GFORTRAN
export CC=$GCC
export CXX=$GPP

export LIBGFORTRAN=$(glocate libgfortran.a | grep gcc/7 | grep -v i386)
export LIBGCC=$(glocate libgcc.a | grep gcc/7 | grep -v i386)

/usr/local/bin/gfortran-7 -O -g -Wall -Wno-maybe-uninitialized          \
         -Wno-unused-variable                                           \
         -Wno-unused-function                                           \
         -fcheck=all                                                    \
         -c                                                             \
         fstrings.F90                                                   \
         fstrings_test_01.F90

/usr/local/bin/gcc-7                                                 \
      -lgfortran                                                     \
      -lgcc                                                          \
      fstrings.o                                                     \
      fstrings_test_01.o                                             \
   -o fstrings_test_01
