module simple_list

  type LIST_ELEMENT_T
    character (len=:), allocatable      :: s
    type ( LIST_ELEMENT_T ), pointer    :: next => null()
  end type LIST_ELEMENT_T

  type LIST_T
    type ( LIST_ELEMENT_T ), pointer :: first => null()
    type ( LIST_ELEMENT_T ), pointer :: last  => null()
  contains
    procedure :: append
    procedure :: list_from_array
  end type LIST_T

  public :: assignment(=)
  interface assignment(=)
    procedure :: list_from_array
  end interface

contains

  subroutine append(this, text )

    class ( LIST_T ), intent(inout) :: this
    character (len=*), intent(in)   :: text

    class ( LIST_ELEMENT_T ), pointer :: newElement_ptr

    newElement_ptr => null()
    allocate( newElement_ptr )
    newElement_ptr%s = text
    newElement_ptr%next => null()

    if (associated( this%first ) ) then

      this%last%next => newElement_ptr
      this%last      => newElement_ptr
      this%last%next => null()

    else

      this%first => newElement_ptr
      this%last  => newElement_ptr

    endif

  end subroutine append

  subroutine list_from_array( this, stext_vals )

    class ( LIST_T ), intent(inout) :: this
    character (len=*), intent(in)   :: stext_vals(:)

    integer :: indx

    do indx=1, ubound( stext_vals, 1 )
      call this%append( stext_vals( indx ) )
    enddo

  end subroutine list_from_array

end module simple_list

program minimal_test_case

  use simple_list
  implicit none

  type ( LIST_T )     :: mylist1, mylist2
  integer             :: indx
  character (len=10)  :: text_vals(5)

  text_vals =  ["one  ", "two  ", "three", "four ", "five "]

  call mylist1%append("one")
  call mylist1%append("two")
  call mylist1%append("three")
  call mylist1%append("four")

  call mylist2%list_from_array( text_vals )
  call mylist2%list_from_array( ["one  ", "two  ", "three", "four ", "five "] )

end program minimal_test_case
