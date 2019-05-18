#!/bin/sh

python make_test_suite.py



gfortran -O2                                           \
         -c                                            \
         $(ls test*.F90)

gfortran -O2                                           \
         fruit_util.F90                                \
         fruit.F90                                     \
         fstrings.F90                                  \
         fruit_driver.F90                              \
         $(ls test*.o)                                 \
         -o fstring_tests
