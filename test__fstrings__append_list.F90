module test__fstrings__append_list

  use fruit
  use fstrings
  use iso_c_binding
  implicit none

contains

  subroutine test_append_list_function
    ! append one FSTRINGS_T object to another FSTRINGS_T object
    type (FSTRINGS_T)              :: mylist, mylist2
    character (len=:), allocatable :: mystring, mystring2

    mystring = "one, two, three, four, five, six, seven"
    mylist = split(mystring)

    call mylist%append("eight")
    call mylist%append("nine")
    call mylist%append("ten")
    call mylist%append("eleven")

    mystring2 = "twelve, thirteen, fourteen, fifteen, sixteen"
    mylist2 = split(mystring2)

    call mylist%append( mylist2 )

    call assert_equals(16, mylist%str_count)

  end subroutine test_append_list_function

  subroutine test_append_list_missing_values
    ! create FSTRINGS_T objects appended w missing values
    type (FSTRINGS_T)              :: mylist

    call mylist%append("one")
    call mylist%append("two")
    call mylist%append("")
    call mylist%append("four")
    call mylist%append("")
    call mylist%append("six")
    call mylist%append("seven")

    call assert_equals ("four", mylist%get(4))
    call assert_equals ("", mylist%get(5))

  end subroutine test_append_list_missing_values


end module test__fstrings__append_list
