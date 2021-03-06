module test__fstrings__split_generate_list

  use fruit
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_generate_list_from_delimited_string
    ! create FSTRING_LIST_T objects from delimited character strings
    type (FSTRING_LIST_T)              :: mylist, another_list
    character (len=:), allocatable :: mystring

    mystring = "one, two, three, four"
    mylist = split(mystring)

    another_list = split("five, six, seven, eight")

    call assert_equals ("three", mylist%get(3))
    call assert_equals ("eight", another_list%get(4))

  end subroutine test_generate_list_from_delimited_string

  subroutine test_append_list_string_missing_values
    ! create FSTRING_LIST_T objects from appended list w missing values
    type (FSTRING_LIST_T)              :: mylist

    call mylist%append("")
    call mylist%append("two")
    call mylist%append("three")
    call mylist%append("")
    call mylist%append("five"//c_null_char)

    call assert_equals ("", mylist%get(1))
    call assert_equals ("five", mylist%get(5))
  end subroutine test_append_list_string_missing_values

  subroutine test_generate_list_string_missing_values
    ! create FSTRING_LIST_T objects from split list w missing values
    type (FSTRING_LIST_T)              :: mylist, another_list
    character (len=:), allocatable :: mystring
    mystring = "one, two, , four"
    mylist = split(mystring)
    another_list = split(", six, seven, eight")
    call assert_equals ("four", mylist%get(4))
    call assert_equals ("", another_list%get(1))
    call assert_equals ("eight", another_list%get(4))
  end subroutine test_generate_list_string_missing_values

end module test__fstrings__split_generate_list
