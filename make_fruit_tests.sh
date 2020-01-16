#!/bin/sh

python make_test_suite.py


gfortran -O2                                           \
         -c                                            \
         fstring.F90


gfortran -O2                                           \
         -c                                            \
         fstring_list.F90

gfortran -O2                                           \
         -c                                            \
         $(ls test*.F90)

gfortran -O2                                           \
         fruit_util.F90                                \
         fruit.F90                                     \
         fruit_driver.F90                              \
         $(ls test*.o)                                 \
         $(ls fstring*.o)                              \
         -o fstring_tests
