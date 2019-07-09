module test__fstrings__grep

  use fruit
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_grep_function
    ! create FSTRING_LIST_T 'grep' functionality
    type (FSTRING_LIST_T)              :: mylist
    TYPE (FSTRING_LIST_T)              :: mysubset
    character (len=:), allocatable :: mystring

    mystring = "one, two, three, four, five, six, seven"
    mylist = split(mystring)

    mysubset = mylist%grep("four")

    call assert_equals("four", mysubset%get(1) )

  end subroutine test_grep_function

end module test__fstrings__grep
