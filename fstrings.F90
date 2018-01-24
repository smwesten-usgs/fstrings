module fstrings


  use iso_c_binding
  implicit none

  private

  public :: FSTRINGS_T

  public :: operator(+)
  interface operator(+)
    procedure :: concatenate_character_character_fn
!    procedure :: concatenate_char_int_fn
!    procedure :: concatenate_char_float_fn
!    procedure :: concatenate_char_double_fn
  end interface operator(+)

  type FSTRINGS_T

    character (len=:), allocatable        :: s
    type (FSTRINGS_T), pointer            :: previous  => null()
    type (FSTRINGS_T), pointer            :: next      => null()
    integer (c_int)                       :: index_val

  contains

!    procedure   :: assign_fstring_to_character_sub
    procedure   :: assign_character_to_fstring_sub
    generic     :: assignment(=) => assign_character_to_fstring_sub
!                                    assign_fstring_to_character_sub

    procedure   :: print_all_entries_sub
    generic     :: print_all => print_all_entries_sub

    procedure   :: append_character_to_fstring_sub
    generic     :: append => append_character_to_fstring_sub

    procedure   :: count_strings_in_list_fn
    generic     :: count => count_strings_in_list_fn

    procedure   :: deallocate_all_list_items_sub
    generic     :: clear => deallocate_all_list_items_sub

    procedure   :: char_to_uppercase_fn
    generic     :: upper => char_to_uppercase_fn

  end type FSTRINGS_T

contains

  function c_to_f_string_fn(c_character_str)   result(f_character_str)

    character (len=*), intent(in)                    :: c_character_str
    character (len=:), allocatable                   :: f_character_str

    integer (c_int)   :: indx

    do indx=1,len(c_character_str)
      if (c_character_str(indx:indx) == c_null_char)  exit
    enddo

    f_character_str = c_character_str(1:indx-1)

  end function c_to_f_string_fn


  function f_to_c_string_fn(f_character_str)   result(c_character_str)

    character (len=*), intent(in)                    :: f_character_str
    character (len=:), allocatable                   :: c_character_str

    c_character_str = trim(f_character_str)//c_null_char

  end function f_to_c_string_fn


  subroutine assign_character_to_fstring_sub(this, character_str)

    class (FSTRINGS_T), intent(inout), target        :: this
    character (len=*), intent(in)                    :: character_str

    call this%clear()

    this%index_val = 1
    this%s = character_str//c_null_char

  end subroutine assign_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_fstring_to_character_sub(this, character_str)

    class (FSTRINGS_T), intent(inout), target         :: this
    character (len=*), intent(out)                    :: character_str

    character_str =  this%s

  end subroutine assign_fstring_to_character_sub

!--------------------------------------------------------------------------------------------------

  function count_strings_in_list_fn(this)        result(counter)

    class (FSTRINGS_T), intent(inout), target        :: this
    integer (c_int)                                  :: counter

    ! [ LOCALS ]
    type (FSTRINGS_T), pointer                       :: current

    counter = 0

    current => this

    do while (associated(current))

      counter = counter + 1
      current => current%next

    enddo

  end function count_strings_in_list_fn

!--------------------------------------------------------------------------------------------------

  subroutine append_character_to_fstring_sub(this, character_str)

    class (FSTRINGS_T), intent(inout), target        :: this
    character (len=*), intent(in)                    :: character_str

    ! [ LOCALS ]
    type (FSTRINGS_T), pointer                       :: current
    type (FSTRINGS_T), pointer                       :: last
    type (FSTRINGS_T), pointer                       :: previous
    type (FSTRINGS_T), pointer                       :: new

    allocate(new)

    new%s = character_str
    new%next => null()

    if ( associated(this%next) ) then

      current => this%next

      do while ( associated(current) )

        last => current
        previous => current%previous
        current => current%next

      enddo

!      last%previous => previous
      last%next => new
      !! crash here
!      if (associated( previous) )   last%index_val = previous%index_val + 1

      new%previous => last
      new%index_val = new%previous%index_val + 1

    else

      this%next    => new
      new%previous => this
      new%index_val = this%index_val + 1

    endif

  end subroutine append_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  subroutine print_all_entries_sub(this)

    class (FSTRINGS_T), intent(inout), target   :: this

    ! [ LOCALS ]
    type (FSTRINGS_T), pointer                  :: current
    integer (c_int)                             :: counter

    counter = 0

    current => this

    do while (associated(current))

      counter = counter + 1

      print *, current%index_val, current%s
      current => current%next

    enddo

  end subroutine print_all_entries_sub

!--------------------------------------------------------------------------------------------------

  function concatenate_character_character_fn(character_str1, character_str2)  result(character_str)

    character (len=*), intent(in)       :: character_str1
    character (len=*), intent(in)       :: character_str2
    character (len=:), allocatable      :: character_str

    character_str = character_str1//character_str2

  end function concatenate_character_character_fn

!--------------------------------------------------------------------------------------------------

  subroutine list_finalize_sub(this)

    type (FSTRINGS_T), intent(inout) :: this

    call this%clear()

  end subroutine list_finalize_sub

!--------------------------------------------------------------------------------------------------

  subroutine deallocate_all_list_items_sub(this)

    class (FSTRINGS_T), intent(inout), target :: this

    ! [ LOCALS ]
    type (FSTRINGS_T), pointer  :: current
    type (FSTRINGS_T), pointer  :: toremove

    current => this%next
    toremove => null()

    do while ( associated( current ) )

      toremove => current

      current => current%next

      if ( allocated( toremove%s ) )  deallocate( toremove%s )

      if ( associated( toremove ) )  deallocate( toremove )

    enddo

    if ( allocated( this%s ) )  deallocate( this%s )
    this%next => null()

  end subroutine deallocate_all_list_items_sub

!--------------------------------------------------------------------------------------------------

  ! recursive subroutine list_sort_sub(this)
  !
  !   class (FSTRINGS_T), intent(inout), target :: this
  !
  !   ! [ LOCALS ]
  !   type (FSTRINGS_T), pointer :: left
  !   type (FSTRINGS_T), pointer :: right
  !   type (FSTRINGS_T), pointer :: pivot
  !   type (FSTRINGS_T), pointer :: marker
  !   type (STRING_LIST_T)                  :: list_left_chunk
  !   type (STRING_LIST_T)                  :: list_right_chunk
  !
  !   real (kind=c_float)  :: random_value
  !   integer (kind=c_int) :: pivot_index
  !   integer (kind=c_int) :: itemCount
  !   integer (kind=c_int) :: minIndex
  !   integer (kind=c_int) :: maxIndex
  !
  !   itemCount = this%count()
  !
  !   minIndex = string_list%first%indexVal
  !   maxIndex = string_list%last%indexVal
  !
  !   print *, "+++ ITEM COUNT: ", itemCount
  !
  !   if (itemCount > 1) then
  !
  !       call random_number(random_value)
  !       pivot_index = int( 1 + random_value * (itemCount-1), kind=c_int )
  !
  !       print *, "++++ PIVOT INDEX: ", pivot_index
  !       pivot => string_list%get_pointer( pivot_index )
  !
  !       left => string_list%first
  !       right => string_list%last
  !
  !       print *, "l, r: ", left%indexVal, right%indexVal, pivot%indexVal
  !
  !       do
  !         do while ( right%s > pivot%s )
  !           print *, right%s, pivot%s
  !           if ( associated( right%previous ) )  right => right%previous
  !         end do
  !
  !         do while ( left%s < pivot%s )
  !           print *, left%s, pivot%s
  !           if ( associated( left%next ) )   left => left%next
  !         end do
  !
  !         if ( left%s < right%s )   call swap_list_values( left, right )
  !
  !         exit
  !
  !       end do
  !
  !       if (left%s == right%s) then
  !           if ( associated( left%next ) )   marker => left%next
  !       else
  !           marker => left
  !       end if
  !
  !       list_left_chunk%first => string_list%first
  !       if ( associated( marker%previous) )  list_left_chunk%last => marker%previous
  !
  !       list_right_chunk%last => string_list%last
  !       list_right_chunk%first => marker
  !
  !       print *, marker%indexVal
  !       print *, marker%previous%indexVal
  !
  !       print *, "== LEFT CHUNK =="
  !       call list_left_chunk%print()
  !       print *, "== RIGHT CHUNK =="
  !       call list_right_chunk%print()
  !
  !
  !
  !       call list_sort_sub( list_left_chunk )
  !       call list_sort_sub( list_right_chunk )
  !
  !     end if
  !
  !   end subroutine list_sort_sub


  elemental function char_to_uppercase_fn (this)                    result(text)

    class (FSTRINGS_T), intent(in)                    :: this
    character(len=:), allocatable                     :: text

    ! LOCALS
    integer (c_int) :: indx

    ! CONSTANTS
    integer (c_int), parameter :: LOWER_TO_UPPER = -32
    integer (c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    text = this%s

    do indx=1,len_trim(text)
      if ( ichar(text(indx:indx) ) >= ASCII_SMALL_A .and. ichar(text(indx:indx)) <= ASCII_SMALL_Z ) then
        text(indx:indx) = char( ichar( text(indx:indx) ) + LOWER_TO_UPPER )
      end if
    end do

  end function char_to_uppercase_fn


end module fstrings
