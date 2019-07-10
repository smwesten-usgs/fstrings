module test__fstrings__unique

  use fruit
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_unique_function
    ! pare a list with duplicates down to a unique list
    type (FSTRING_LIST_T)              :: mylist
    TYPE (FSTRING_LIST_T)              :: myuniquevals
    character (len=:), allocatable     :: mystring

    ! count of unique values:
    !            1     2    3    4     5      6     7    8                9
    mystring = "zero, one, two, three, four, five, six, seven, one, two, twenty-one, five"
    mylist = split(mystring)

    call mylist%append("zero")
    call mylist%append("eight")    ! 10
    call mylist%append("nine")     ! 11
    call mylist%append("eleven")   ! 12
    call mylist%append("eleven")
    call mylist%append("zero")
    call mylist%append("twelve")   ! 13
    call mylist%append("thirteen") ! 14
    call mylist%append("fourteen") ! 15

    myuniquevals = mylist%unique()

    call assert_equals("twenty-one", myuniquevals%get(9) )
    call assert_equals(15, myuniquevals%count)
    call assert_equals(15, myuniquevals%count_entries())

  end subroutine test_unique_function

end module test__fstrings__unique
