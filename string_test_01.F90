program string_test_01

  use string_list
  use strings
  use iso_c_binding
  implicit none

  type( STRING_LIST_T ) :: sl1, sl2, sl3, sl4
  character (len=5)     :: items(4) = [ "one  ","two  ","three", "four "]
  integer (kind=c_int)               :: indx, jndx
  real (kind=c_float)                :: fvals


!   sl4 = [ "one  ","two  ","three", "four "]
! !  sl4 = items
!   call sl4%print()
!
!   sl3 = [ 1.,2.2,3.5,4.3,5.2,6.8,7.1,8.9,9.3,10.0 ]
!   call sl3%print()
!
!   sl3 = "zero"
!
!   call sl3%append("one")
!   call sl3%append("two")
!   call sl3%append("three")
!   call sl3%append("four")
!   call sl3%append("five")
!   call sl3%print()

  do

    jndx=1

    do indx=1, 12

      call random_number(fvals)
      call sl1%append( fvals )

    enddo

    call sl1%print()

    exit

  enddo

  print *, "--- SORTING LIST ---"

  call list_sort( sl1 )
  !call sl1%print()

end program string_test_01
