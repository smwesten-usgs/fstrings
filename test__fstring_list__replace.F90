module test_fstring_list__replace

  use fruit
  use fstring
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_list_replace
    ! test list 'replace' subroutine
    character (len=256)    :: mystring

    type (FSTRING_LIST_T)  :: mylist

    mystring = "one, two, thirteen, four, five"

    mylist = split(mystring, ",")

    call assert_equals ("thirteen", mylist%get(3))

    call mylist%replace(3,"three")
    call assert_equals ("three", mylist%get(3))

  end subroutine test_list_replace

end module test_fstring_list__replace
