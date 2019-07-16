module test__fstrings__sort

  use fruit
  use fstring
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_sort_routine
    ! sort FSTRING_LIST_T objects and extract correct values

    type (FSTRING_LIST_T)    :: name
    type (FSTRING_LIST_T)    :: gender
    type (FSTRING_LIST_T)    :: number
    character (len=256)  :: sbuf
    character (len=256)  :: substr
    character (len=256)  :: open_iomsg
    integer              :: lu
    integer (c_int)      :: read_status
    integer (c_int)      :: open_status

    open(unit=10, file='yob1981.txt', iostat=open_status, iomsg=open_iomsg)

    do
      read(10, iostat=read_status, fmt="(a)") sbuf

      if (read_status /= 0) exit

      call chomp(sbuf, substr, ",")
      call name%append(substr)

      call chomp(sbuf, substr, ",")
      call gender%append(substr)

      call chomp(sbuf, substr, ",")
      call number%append(substr)

    end do

    call name%sort()
    call number%sort_integer("decreasing")

    call assert_equals ("5845", number%get(104))
    call assert_equals ("73", number%get(2668))

    call number%sort_float("decreasing")

    call assert_equals ("5845.00000", number%get(104))
    call assert_equals ("73.0000000", number%get(2668))

    call assert_equals ("Aaron", name%get(11))
    call assert_equals ("Aaron", name%get(12))
    call assert_equals ("Stewart", name%get(17000))
    call assert_equals ("Tobias", name%get(18150))
    call assert_equals ("Trixie", name%get(18472))
    call assert_equals ("Ugochukwu", name%get(18641))
    call assert_equals ("Umberto", name%get(18656))

  end subroutine test_sort_routine

end module test__fstrings__sort
