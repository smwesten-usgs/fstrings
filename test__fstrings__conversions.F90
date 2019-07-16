module test__fstrings__conversions

  use fruit
  use fstring
  use iso_c_binding
  implicit none

contains

  subroutine test_c_double_conversions
    ! test as_character for c_double value
    character (len=256) :: mystring

    mystring = as_character( 3.1415926535897932_c_double, fmt_string="F0.7")
    call assert_equals ("3.1415927", mystring)

    mystring = as_character( 3.1415926535897932_c_double, fmt_string="E13.7")
    call assert_equals ("0.3141593E+01", mystring)

    mystring = as_character( 3.1415926535897932_c_double, fmt_string="ES13.7")
    call assert_equals ("3.1415927E+00", mystring)

    mystring = as_character( 3.1415926535897932_c_double, fmt_string="G12.7")
    call assert_equals ("3.141593", mystring)

  end subroutine test_c_double_conversions

  subroutine test_float_conversions
    ! test as_character for c_float value
    character (len=256) :: mystring

    mystring = as_character( 3.141592_c_float, fmt_string="F0.6")
    call assert_equals ("3.141592", mystring)

    mystring = as_character( 3.141592_c_float, fmt_string="E12.6")
    call assert_equals ("0.314159E+01", mystring)

    mystring = as_character( 3.141592_c_float, fmt_string="ES12.6")
    call assert_equals ("3.141592E+00", mystring)

    mystring = as_character( 3.141592_c_float, fmt_string="G12.6")
    call assert_equals ("3.14159", mystring)

  end subroutine test_float_conversions


  subroutine test_short_conversions
    ! test as_character for c_short value
    character (len=256) :: mystring

    mystring = as_character( 42_c_short, fmt_string="I5.4")
    call assert_equals ("0042", mystring)

    ! demonstrate use of Fortran "sign print" descriptor, "sp":
    mystring = as_character( 42_c_short, fmt_string="SP,I0")
    call assert_equals ("+42", mystring)

  end subroutine test_short_conversions


  subroutine test_integer_conversions
    ! test as_character for c_int value
    character (len=256) :: mystring

    mystring = as_character( 65536_c_int, fmt_string="I8.6")
    call assert_equals ("065536", mystring)

  end subroutine test_integer_conversions

  subroutine test_string_to_integer_conversions
    ! test as_integer function
    integer (c_int)     :: value
    value = as_integer("CN_42")
    call assert_equals (42, value)
  end subroutine test_string_to_integer_conversions

  subroutine test_string_to_float_conversions
    ! test as_float function
    real (c_float)     :: value
    value = as_float("SN_3.1415")
    call assert_equals (3.1415_c_float, value)
  end subroutine test_string_to_float_conversions


end module test__fstrings__conversions
