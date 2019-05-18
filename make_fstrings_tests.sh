#!/bin/sh

gfortran -O -g -Wall -Wno-maybe-uninitialized          \
         -Wno-unused-variable                          \
         -Wno-unused-function                          \
         -fcheck=all                                   \
         fstrings.F90                                  \
         fstrings_test_01.F90                          \
         -o fstrings_test_01

gfortran -O -g -Wall -Wno-maybe-uninitialized          \
         -Wno-unused-variable                          \
         -Wno-unused-function                          \
         -fcheck=all                                   \
         fstrings.F90                                  \
         fstrings_test_02.F90                          \
         -o fstrings_test_02

gfortran -O -g -Wall -Wno-maybe-uninitialized          \
         -Wno-unused-variable                          \
         -Wno-unused-function                          \
         -fcheck=all                                   \
         fstrings.F90                                  \
         fstrings_test_03.F90                          \
         -o fstrings_test_03
