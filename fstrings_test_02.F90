program fstrings_test_02

  use fstrings
  use iso_c_binding
  implicit none

  type (FSTRINGS_T)    :: name
  type (FSTRINGS_T)    :: gender
  type (FSTRINGS_T)    :: number
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

    call chomp(sbuf, substr)
    call name%append(substr)

    call chomp(sbuf, substr)
    call gender%append(substr)

    call chomp(sbuf, substr)
    call number%append(substr)

  end do

  print *, name%str_count

  call name%sort()
  call name%print_all()

end program fstrings_test_02
