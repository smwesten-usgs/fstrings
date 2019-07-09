module test__fstrings__chomp

  use fruit
  use fstring
  use iso_c_binding
  implicit none

contains

  subroutine test_chomp
    ! test 'chomp' subroutine
    character (len=256) :: mystring
    character (len=256) :: mysubstring

    mystring = "one, two, three, four"

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("one", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("two", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("three", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("four", mysubstring)

    mystring = "one, , three, four"

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("one", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("three", mysubstring)

  end subroutine test_chomp

end module test__fstrings__chomp
