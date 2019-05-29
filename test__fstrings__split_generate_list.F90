module test__fstrings__split_generate_list

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


  subroutine test_generate_list_string_missing_values
    ! create FSTRINGS_T objects from list w missing values
    type (FSTRINGS_T)              :: mylist, another_list
    character (len=:), allocatable :: mystring
    mystring = "one, two, three, four"
    mylist = split(mystring)
    another_list = split(", six, seven, eight")
    call assert_equals ("three", mylist%get(3))
    call assert_equals ("eight", another_list%get(4))
  end subroutine test_generate_list_string_missing_values

end module test__fstrings__split_generate_list
