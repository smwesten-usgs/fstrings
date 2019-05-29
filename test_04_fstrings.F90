module test_04_fstrings

  use fruit
  use fstrings
  use iso_c_binding
  implicit none

contains

  subroutine test_grep_function
    ! create FSTRINGS_T objects from delimited character strings
    type (FSTRINGS_T)              :: mylist
    TYPE (FSTRINGS_T)              :: mysubset
    character (len=:), allocatable :: mystring

    mystring = "one, two, three, four, five, six, seven"
    mylist = split(mystring)

    mysubset = mylist%grep("four")

    call assert_equals("four", mysubset%get(1) )

  end subroutine test_grep_function

end module test_04_fstrings
