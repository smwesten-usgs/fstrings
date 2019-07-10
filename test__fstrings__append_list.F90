module test__fstrings__append_list

  use fruit
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_append_list_function
    ! append one FSTRING_LIST_T object to another FSTRING_LIST_T object
    type (FSTRING_LIST_T)              :: mylist, mylist2
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

    call assert_equals(16, mylist%count)

  end subroutine test_append_list_function

  subroutine test_append_list_missing_values
    ! create FSTRING_LIST_T objects appended w missing values
    type (FSTRING_LIST_T)              :: mylist

    call mylist%append("one")
    call mylist%append("two")
    call mylist%append("")
    call mylist%append("four")
    call mylist%append("")
    call mylist%append("six")
    call mylist%append("seven")

    call assert_equals("four", mylist%get(4))
    call assert_equals("", mylist%get(5))

  end subroutine test_append_list_missing_values

  subroutine test_detect_missing_values
    ! test whether empty list entries are detected and counted
    type (FSTRING_LIST_T)              :: mylist

    call mylist%append("")
    call mylist%append("two")
    call mylist%append("")
    call mylist%append("four")
    call mylist%append("")
    call mylist%append("six")
    call mylist%append("seven")

    call assert_true( logical(mylist%empty_entries_present()) )
    call assert_equals(3, mylist%missing_value_count)

  end subroutine test_detect_missing_values

end module test__fstrings__append_list
