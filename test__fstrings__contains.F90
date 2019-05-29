module test__fstrings__count_matching

  use fruit
  use fstrings
  use iso_c_binding
  implicit none

contains

  subroutine test_count_matching_function
    ! create FSTRINGS_T objects from delimited character strings
    type (FSTRINGS_T)              :: mylist
    TYPE (FSTRINGS_T)              :: mysubset
    character (len=:), allocatable :: mystring

    mystring = "zero, one, two, three, four, five, six, seven"
    mylist = split(mystring)

    call mylist%append("zero")
    call mylist%append("eight")
    call mylist%append("nine")
    call mylist%append("eleven")
    call mylist%append("zero")
    call mylist%append("twelve")
    call mylist%append("thirteen")
    call mylist%append("fourteen")

    call assert_equals(3, mylist%count_matching("zero") )

  end subroutine test_count_matching_function

end module test__fstrings__count_matching
