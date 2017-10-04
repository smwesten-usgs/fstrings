#!/bin/sh
gfortran -O -g -Wall -Wno-maybe-uninitialized          \
         -Wno-unused-variable                          \
         -Wno-unused-function                          \
         -fcheck=all                                   \
         constants_and_conversions.F90                 \
         strings.F90                                   \
         string_list.F90                               \
         string_test_01.F90                            \
      -o string_test_01
