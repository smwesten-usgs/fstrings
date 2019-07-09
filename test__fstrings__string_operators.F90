module test__fstrings__string_operators

  use fruit
  use fstring
  use iso_c_binding
  implicit none

contains

  subroutine test_integer_concat
    ! append integer value to a string
    character (len=:), allocatable :: mystring

    mystring = "output__"
    mystring = mystring + 1997

    call assert_equals("output__1997", mystring )

  end subroutine test_integer_concat

  subroutine test_float_concat
    ! append float value to a string
    character (len=:), allocatable :: mystring

    mystring = "pi = "
    mystring = mystring + 3.14159265_c_float

    ! need to discard rightmost part of string; system differences may make
    ! these last digits of pi unpredictable
    call assert_equals("pi = 3.14159", left(mystring, 12 ) )

  end subroutine test_float_concat

  subroutine test_double_concat
    ! append double value to a string
    character (len=:), allocatable :: mystring

    mystring = "pi = "
    mystring = mystring + 3.1415926535897932_c_double

    ! need to discard rightmost part of string; system differences may make
    ! these last digits of pi unpredictable
    call assert_equals("pi = 3.1415926535897", left(mystring, 20 ) )

  end subroutine test_double_concat

  subroutine test_char_concat
    ! append double value to a string
    character (len=:), allocatable :: mystring

    mystring = "pi = "
    mystring = mystring + "3.1415926535897932"

    call assert_equals("pi = 3.1415926535897932", mystring )

  end subroutine test_char_concat

  subroutine test_contains_operator
    ! test .contains. operator
    character (len=:), allocatable :: mystring
    logical :: is_true, is_false

    mystring = "We earth men have a talent for ruining big, beautiful things."

    is_true = mystring .contains. "We"
    is_false = mystring .contains. "we"

    call assert_true(is_true)
    call assert_false(is_false)

    is_true = mystring .contains. "talent"
    call assert_true(is_true)

    is_false = mystring .contains. "EaRtH MEN"
    call assert_false(is_false)

  end subroutine test_contains_operator

  subroutine test_containssimilar_operator
    ! test .containssimilar. operator
    character (len=:), allocatable :: mystring
    logical :: is_true, is_false

    mystring = "We earth men have a talent for ruining big, beautiful things."

    is_true = mystring .contains. "We"
    call assert_true(is_true)

    is_true = mystring .containssimilar. "we"
    call assert_true(is_true)

    is_true = mystring .containssimilar. "EaRtH MEN"
    call assert_true(is_true)

  end subroutine test_containssimilar_operator

  subroutine test_strequal_operator
    ! test .contains. operator
    character (len=:), allocatable :: mystring
    logical :: is_true, is_false

    mystring = "We earth men have a talent for ruining big, beautiful things."

    is_true = mystring .strequal. "We earth men have a talent for ruining big, beautiful things."
    is_false = mystring .strequal. "We earth men have a talent for ruining big, beautiful things. Eventually."

    call assert_true(is_true)
    call assert_false(is_false)

  end subroutine test_strequal_operator

  subroutine test_strapprox_operator
    ! test .contains. operator
    character (len=:), allocatable :: mystring
    logical :: is_true, is_false

    mystring = "We earth men have a talent for ruining big, beautiful things."

    is_true = mystring .strapprox. "We earth men have a talent for ruining big, beautiful things."
    call assert_true(is_true)

    is_true = mystring .strapprox. "We EARTH men have a talent for ruining big, beautiful things."
    call assert_true(is_true)

  end subroutine test_strapprox_operator

end module test__fstrings__string_operators
