module test__fstrings__grep_2

  use fruit
  use fstring
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_grep_function_2
    ! test partial string matching in FSTRING_LIST_T 'grep' functionality
    type (FSTRING_LIST_T)              :: mylist
    TYPE (FSTRING_LIST_T)              :: mysubset
    character (len=:), allocatable :: mystring

    mystring = "corn, oats, deciduous forest, forested wetland, mixed forest, evergreen forest, hay"
    mylist = split(mystring)

    mysubset = mylist%grep("forest")

    call assert_equals("deciduous forest", mysubset%get(1) )
    call assert_equals("forested wetland", mysubset%get(2) )
    call assert_equals("evergreen forest", mysubset%get(4) )

  end subroutine test_grep_function_2

end module test__fstrings__grep_2
