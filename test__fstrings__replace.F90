module test__fstrings__replace

  use fruit
  use fstring
  use iso_c_binding
  implicit none

contains

  subroutine test_replace
    ! test 'chomp' subroutine
    character (len=256) :: mystring
    character (len=256) :: mysubstring

    mystring = "We earth men have a talent for ruining big, beautiful things."

    call replace(mystring, " ", "_")
    call replace(mystring, ",")
    call assert_equals ("We_earth_men_have_a_talent_for_ruining_big_beautiful_things.", mystring)

  end subroutine test_replace

end module test__fstrings__replace
