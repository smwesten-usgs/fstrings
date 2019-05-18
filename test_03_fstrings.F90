module test_03_fstrings

  use fruit
  use fstrings
  use iso_c_binding
  implicit none

contains

  subroutine test_split_function_to_generate_list_from_string
    ! create FSTRINGS_T objects from delimited character strings
    type (FSTRINGS_T)              :: mylist, another_list
    character (len=:), allocatable :: mystring

    mystring = "one, two, three, four"
    mylist = split(mystring)

    another_list = split("five, six, seven, eight")

    call assert_equals ("three", mylist%get(3))
    call assert_equals ("eight", another_list%get(4))

  end subroutine test_split_function_to_generate_list_from_string

end module test_03_fstrings
