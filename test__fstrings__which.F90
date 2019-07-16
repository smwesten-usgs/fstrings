module test__fstrings__which

  use fruit
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_count_which_function
    ! test 'which' function; should return indices of matching strings
    type (FSTRING_LIST_T)              :: mylist
    TYPE (FSTRING_LIST_T)              :: mysubset
    character (len=:), allocatable     :: mystring
    integer (c_int), allocatable       :: index_values(:)

    mystring = "blue, green, blue, red, yellow, orange, blue, purple, pink"
    mylist = split(mystring)

    index_values = mylist%which("red")
    call assert_equals(4, index_values(1) )

    index_values = mylist%which("blue")
    call assert_equals(1, index_values(1) )
    call assert_equals(3, index_values(2) )
    call assert_equals(7, index_values(3) )

  end subroutine test_count_which_function

end module test__fstrings__which
