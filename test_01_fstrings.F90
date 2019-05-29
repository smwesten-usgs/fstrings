module test_01_fstrings

  use fruit
  use fstrings
  use iso_c_binding
  implicit none

contains

  subroutine test_count_function
    ! create FSTRINGS_T objects from delimited character strings
    type (FSTRINGS_T)              :: mylist
    character (len=:), allocatable :: mystring

    mystring = "one, two, three, four, five, six, seven"
    mylist = split(mystring)

    call assert_equals(7, mylist%count() )

    call mylist%append("eight")
    call mylist%append("nine")
    call mylist%append("ten")
    call mylist%append("eleven")

    call assert_equals(11,mylist%count())

  end subroutine test_count_function

end module test_01_fstrings
