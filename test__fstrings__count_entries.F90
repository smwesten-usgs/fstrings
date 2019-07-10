module test__fstrings__count_entries

  use fruit
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_count_function
    ! create FSTRING_LIST_T objects from delimited character strings
    type (FSTRING_LIST_T)              :: mylist
    character (len=:), allocatable :: mystring

    mystring = "one, two, three, four, five, six, seven"
    mylist = split(mystring)

    call assert_equals(7, mylist%count_entries() )

    call mylist%append("eight")
    call mylist%append("nine")
    call mylist%append("ten")
    call mylist%append("eleven")

    call assert_equals(11,mylist%count_entries())

  end subroutine test_count_function

end module test__fstrings__count_entries
