module string_list

  use iso_c_binding, only : c_int
  use constants_and_conversions
  use strings
!  use exceptions
  implicit none

  private

  public :: STRING_LIST_T
  public :: assignment(=)

  interface assignment(=)
     procedure :: list_create_new_from_int_sub
!     procedure :: float_to_char_sub
!     procedure :: double_to_char_sub
  end interface assignment(=)

  type STRING_LIST_ELEMENT_T

    character (len=:), allocatable               :: s
    type (STRING_LIST_ELEMENT_T), pointer        :: next => null()

  end type STRING_LIST_ELEMENT_T


  type STRING_LIST_T

    type (STRING_LIST_ELEMENT_T), pointer        :: first   => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: last    => null()
    integer (kind=c_int)                         :: count   = 0

  contains

    procedure :: list_append_char_sub
    procedure :: list_append_int_sub
    procedure :: list_create_new_from_int_sub
    procedure :: list_get_value_at_index_fn
    procedure :: list_print_sub
    procedure :: list_return_position_of_matching_char_fn
    procedure :: list_items_deallocate_all_sub

    generic :: append => list_append_char_sub, &
                         list_append_int_sub
    generic :: get => list_get_value_at_index_fn
    generic :: print => list_print_sub
    generic :: which => list_return_position_of_matching_char_fn
    generic :: deallocate => list_items_deallocate_all_sub

  end type STRING_LIST_T



contains

  subroutine list_create_new_from_int_sub( this, value_i )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: value_i

    call this%deallocate()
    call this%list_append_char_sub( asCharacter( value_i ) )

  end subroutine list_create_new_from_int_sub




  subroutine list_append_int_sub( this, value_i )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: value_i

    call this%list_append_char_sub( asCharacter( value_i ) )

  end subroutine list_append_int_sub



  subroutine list_append_char_sub( this, text )

    class (STRING_LIST_T), intent(inout)   :: this
    character (len=*), intent(in)          :: text

    ! [ LOCALS ]
    class (STRING_LIST_ELEMENT_T), pointer   :: newElement_ptr => null()
    integer (kind=c_int)                     :: status

    allocate(newElement_ptr, stat=status)
    call assert(status == 0, "There was a problem allocating memory for a new string list element", &
      __FILE__, __LINE__)

    newElement_ptr%s    = text
    newElement_ptr%next => null()

    if (associated( this%last ) ) then

      this%last%next => newElement_ptr
      this%last      => newElement_ptr

    else

      this%first => newElement_ptr
      this%last  => newElement_ptr

    endif

    this%count = this%count + 1

  end subroutine list_append_char_sub

!--------------------------------------------------------------------------------------------------

  function list_get_value_at_index_fn(this, indx)   result(text)

    class (STRING_LIST_T), intent(in)        :: this
    integer (kind=c_int), intent(in)         :: indx
    character (len=:), allocatable           :: text

    ! [ LOCALS ]
    integer (kind=c_int)                      :: count_i
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    count_i = 0

    current => this%first

    if (indx > 0 .and. indx <= this%count .and. associated( current ) ) then

      do while ( associated( current%next) )

        count_i = count_i + 1

        if (count_i == indx)  exit

        current => current%next

      enddo

      text = current%s

    else

      text = "<NA>"

    endif


  end function list_get_value_at_index_fn

!--------------------------------------------------------------------------------------------------

  subroutine list_print_sub(this, iLU)

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(in)     :: this
    integer (kind=c_int), optional        :: iLU

    ! [ LOCALS ]
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iLU_

    if (present(iLU) ) then
      iLU_ = iLU
    else
      iLU_ = OUTPUT_UNIT
    endif

    current => this%first

    do while ( associated( current ) )

      write(iLU_, fmt="(a)") current%s

      current => current%next

    enddo

  end subroutine list_print_sub

!--------------------------------------------------------------------------------------------------

  function break_string_into_list_fn(text1)    result( newList )

    character (len=*), intent(inout)  :: text1
    type (STRING_LIST_T)              :: newList

    ! [ LOCALS ]
    character ( len=len_trim(text1) ) :: sTempText

    do

      call chomp( text1=text1, text2=sTempText )

      if (len_trim(sTempText) > 0) then
        call newList%append( trim(sTempText ) )
      else
        exit
      endif

    end do

  end function break_string_into_list_fn

!--------------------------------------------------------------------------------------------------

  function list_return_position_of_matching_char_fn(this, sChar)     result(iResult)

    class (STRING_LIST_T), intent(in)                    :: this
    character (len=*), intent(in)                        :: sChar
    integer (kind=c_int), dimension(:), allocatable      :: iResult

    ! [ LOCALS ]
    integer (kind=c_int) :: indx
    integer (kind=c_int) :: count_i
    integer (kind=c_int) :: status
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%count) :: iTempResult
    type (STRING_LIST_ELEMENT_T), pointer   :: current => null()

    count_i = 0

    current => this%first

    do while ( associated(current) )
      iRetval = index(string = current%s, &
                      substring = sChar)

      if (iRetval /= 0)  then

        count_i = count_i + 1
        iTempResult(count_i) = indx

      endif

      current => current%next

    enddo

    if (count_i == 0) then

      allocate(iResult(1), stat=status)
      iResult(1) = -9999

    else

      allocate(iResult(count_i), stat=status)
      iResult(1:count_i) = iTempResult(1:count_i)

    endif

    call assert( status == 0, "Problem allocating memory", __FILE__, __LINE__)

  end function list_return_position_of_matching_char_fn


  subroutine list_items_deallocate_all_sub(this)

    class (STRING_LIST_T), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: indx
    integer (kind=c_int) :: status
    type (STRING_LIST_ELEMENT_T), pointer :: current => null()
    type (STRING_LIST_ELEMENT_T), pointer :: toremove => null()

    if ( associated(this%first) ) then

      current => this%first

      do while ( associated(current) )

        toremove => current
        current => current%next

        deallocate(toremove%s)

      enddo

    endif


  end subroutine list_items_deallocate_all_sub


end module string_list
