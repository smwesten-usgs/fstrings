module string_list

  use iso_c_binding, only             : c_int, c_float, c_bool, c_null_char
  use constants_and_conversions
  use strings
!  use logfiles, only                  : LOG_DEBUG
!  use exceptions
  implicit none

  private

  public :: STRING_LIST_T, create_list, list_sort

  public :: assignment(=)
  interface assignment(=)
    procedure :: assign_string_list_to_string_list_sub
    procedure :: list_create_new_from_int_sub
    procedure :: list_create_new_from_int_array_sub
    procedure :: list_create_new_from_float_sub
    procedure :: list_create_new_from_float_array_sub
    procedure :: list_create_new_from_string_sub
    procedure :: list_create_new_from_string_array_sub
  end interface assignment(=)

  interface create_list
    procedure :: list_from_delimited_string_fn
  end interface create_list

  interface swap_list_values
    procedure :: list_swap_values_sub
  end interface swap_list_values

  interface list_sort
    procedure :: list_sort_sub
  end interface list_sort

  type STRING_LIST_ELEMENT_T

    character (len=:), allocatable               :: s
    type (STRING_LIST_ELEMENT_T), pointer        :: previous => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: next     => null()
    integer (kind=c_int)                         :: indexval

  end type STRING_LIST_ELEMENT_T


  type STRING_LIST_T

    type (STRING_LIST_ELEMENT_T), pointer        :: first        => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: last         => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: current      => null()
    logical (kind=c_bool)                        :: autocleanup  = TRUE
    integer (kind=c_int)                         :: count        = 0
    logical (kind=c_bool)                        :: is_populated = FALSE

  contains

    procedure :: list_append_string_sub
    procedure :: list_append_int_sub
    procedure :: list_append_float_sub
    procedure :: list_from_delimited_string_sub
    procedure :: list_get_value_at_index_fn
    procedure :: list_get_values_in_range_fn
    procedure :: list_get_pointer_at_index_fn
    procedure :: list_replace_value_at_index_sub
    procedure :: list_print_sub
    procedure :: list_all_fn
    procedure :: list_all_delimited_fn
    procedure :: list_return_position_of_matching_string_fn
    procedure :: list_return_count_of_matching_string_fn
    procedure :: list_is_string_in_list_fn
    procedure :: list_items_deallocate_all_sub
    procedure :: list_return_all_as_float_fn
    procedure :: list_return_all_as_int_fn
    procedure :: list_return_all_as_logical_fn
    procedure :: list_return_all_as_character_fn
    procedure :: list_subset_partial_matches_fn
    procedure :: list_set_auto_cleanup_sub
    procedure :: list_update_count_fn
    final     :: list_finalize_sub

    generic :: append        => list_append_string_sub,             &
                                list_append_int_sub,                &
                                list_append_float_sub
    generic :: get           => list_get_value_at_index_fn,         &
                                list_get_values_in_range_fn
    generic :: get_pointer   => list_get_pointer_at_index_fn
    generic :: replace       => list_replace_value_at_index_sub
    generic :: create_list   => list_from_delimited_string_sub
    generic :: set_autocleanup  => list_set_auto_cleanup_sub
    generic :: print         => list_print_sub
    generic :: listall       => list_all_fn, &
                                list_all_delimited_fn
    generic :: grep          => list_subset_partial_matches_fn
    generic :: which         => list_return_position_of_matching_string_fn
    generic :: countmatching => list_return_count_of_matching_string_fn
    generic :: iselement     => list_is_string_in_list_fn
    generic :: clear         => list_items_deallocate_all_sub
    generic :: asFloat       => list_return_all_as_float_fn
    generic :: asInt         => list_return_all_as_int_fn
    generic :: asLogical     => list_return_all_as_logical_fn
    generic :: asCharacter   => list_return_all_as_character_fn
    generic :: get_count     => list_update_count_fn

  end type STRING_LIST_T

contains


    subroutine list_create_new_from_float_sub( this, value_f )

      class (STRING_LIST_T), intent(inout)   :: this
      real (kind=c_float), intent(in)        :: value_f

      call this%clear()
      call this%list_append_string_sub( asCharacter( value_f ) )

    end subroutine list_create_new_from_float_sub

  !----------------------------------------------------------------------------

    subroutine list_create_new_from_float_array_sub( this, values_f )

      class (STRING_LIST_T), intent(inout)   :: this
      real (kind=c_float), intent(in)        :: values_f(:)

      integer (kind=c_int) :: indx

      call this%clear()

      do indx=1, ubound( values_f, 1)

        call this%list_append_string_sub( asCharacter( values_f( indx ) ) )

      enddo

    end subroutine list_create_new_from_float_array_sub

  !----------------------------------------------------------------------------

    subroutine list_append_float_sub( this, value_f )

      class (STRING_LIST_T), intent(inout)   :: this
      real (kind=c_float), intent(in)        :: value_f

      call this%list_append_string_sub( asCharacter( value_f ) )

    end subroutine list_append_float_sub

  !----------------------------------------------------------------------------

  subroutine list_create_new_from_int_sub( this, value_i )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: value_i

    call this%clear()
    call this%list_append_string_sub( asCharacter( value_i ) )

  end subroutine list_create_new_from_int_sub

!----------------------------------------------------------------------------

  subroutine list_create_new_from_int_array_sub( this, values_i )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: values_i(:)

    integer (kind=c_int) :: indx

    call this%clear()

    do indx=1, ubound( values_i, 1)

      call this%list_append_string_sub( asCharacter( values_i( indx ) ) )

    enddo

  end subroutine list_create_new_from_int_array_sub

!----------------------------------------------------------------------------

  subroutine list_append_int_sub( this, iValue )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: iValue

    call this%list_append_string_sub( asCharacter(iValue) )

  end subroutine list_append_int_sub

!--------------------------------------------------------------------------------------------------

  subroutine list_set_auto_cleanup_sub( this, autocleanup )

    class (STRING_LIST_T), intent(inout)    :: this
    logical (kind=c_bool), intent(in)       :: autocleanup

    this%autocleanup = autocleanup

  end subroutine list_set_auto_cleanup_sub

  !--------------------------------------------------------------------------------------------------

  subroutine assign_string_list_to_string_list_sub(slList2, slList1)

    type (STRING_LIST_T), intent(out)     :: slList2
    type (STRING_LIST_T), intent(inout)   :: slList1

    ! [ LOCALS ]
    integer (kind=c_int) :: indx

    if ( slList1%count > 0 ) then

      do indx=1, slList1%count

        call slList2%append( slList1%get(indx) )

      enddo

    endif

  end subroutine assign_string_list_to_string_list_sub

  !------------------------------------------------------------------------------

  subroutine list_create_new_from_string_sub( this, text )

    class (STRING_LIST_T), intent(inout)   :: this
    character (len=*), intent(in)          :: text

    call this%clear()
    call this%list_append_string_sub( text )

  end subroutine list_create_new_from_string_sub

  !----------------------------------------------------------------------------

    subroutine list_create_new_from_string_array_sub( this, text_vals )

      class (STRING_LIST_T), intent(inout)   :: this
      character (len=*), intent(in)          :: text_vals(:)

      integer (kind=c_int)           :: indx
      character (len=:), allocatable :: temp_text

      call this%clear()

      do indx=1, ubound( text_vals, 1)

        temp_text = text_vals( indx )
        call this%append( temp_text )

      enddo

    end subroutine list_create_new_from_string_array_sub

  !------------------------------------------------------------------------------

  subroutine list_swap_values_sub( element1_ptr, element2_ptr )

    type (STRING_LIST_ELEMENT_T), pointer :: element1_ptr
    type (STRING_LIST_ELEMENT_T), pointer :: element2_ptr

    character (len=:), allocatable :: temp_text

    if ( associated( element1_ptr ) .and. associated( element2_ptr ) ) then

      temp_text      = element1_ptr%s
      element1_ptr%s = element2_ptr%s
      element2_ptr%s = temp_text

    end if

  end subroutine list_swap_values_sub

!------------------------------------------------------------------------------

  subroutine list_append_string_sub( this, text )

    class (STRING_LIST_T), intent(inout)   :: this
    character (len=*), intent(in) :: text

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer   :: newElement_ptr
    type (STRING_LIST_ELEMENT_T), pointer   :: oldLastElement_ptr
    integer (kind=c_int)                    :: status

    newElement_ptr => null()
    allocate(newElement_ptr, stat=status)
    call assert(status == 0, "There was a problem allocating memory for a new string list element", &
        __FILE__, __LINE__)

    newElement_ptr%s    = trim( text )
    newElement_ptr%next => null()

    if (associated( this%first ) ) then

      if (this%count == 0)  call die("Internal logic error: count should *not* be zero in this block", &
          __FILE__, __LINE__)

      oldLastElement_ptr => this%last
      oldLastElement_ptr%next => newElement_ptr
      newElement_ptr%previous => oldLastElement_ptr
      newElement_ptr%indexval = newElement_ptr%previous%indexval + 1
      this%last      => newElement_ptr
      this%last%next => null()

    else

      this%first    => newElement_ptr
      this%last     => newElement_ptr
      newElement_ptr%indexval = 1

    endif

    this%count = this%count + 1
    this%is_populated = TRUE

  end subroutine list_append_string_sub

  !--------------------------------------------------------------------------------------------------

  subroutine list_replace_value_at_index_sub(this, indx, text)

    class (STRING_LIST_T), intent(inout)     :: this
    integer (kind=c_int), intent(in)         :: indx
    character (len=*), intent(in)            :: text

    ! [ LOCALS ]
    integer (kind=c_int)                      :: icount

    icount = 0

    this%current => this%first

    do while ( associated( this%current ) .and. icount < this%count )

      icount = icount + 1

      if (icount == indx)  exit

      this%current => this%current%next

    enddo

    if (associated(this%current) ) then
      this%current%s = trim( text )
    else
      !      call warn(sMessage="Unable to find a pointer associated with index: "//asCharacter(indx),  &
      !                iLogLevel=LOG_DEBUG,                                                               &
      !                sModule=__FILE__,                                                                  &
      !                iLine=__LINE__ )
    endif

  end subroutine list_replace_value_at_index_sub

  !--------------------------------------------------------------------------------------------------

  function list_get_value_at_index_fn(this, indx)   result(text)

    class (STRING_LIST_T), intent(inout)     :: this
    integer (kind=c_int), intent(in)         :: indx
    character (len=:), allocatable           :: text

    ! [ LOCALS ]
    integer (kind=c_int)                      :: icount

    icount = 0

    this%current => this%first

    do while ( associated( this%current ) .and. icount < this%count )

      icount = icount + 1

      if (icount == indx)  exit

      this%current => this%current%next

    enddo

    if (associated(this%current) ) then
      text = this%current%s
    else
      text = "<NA>"
      !      call warn(sMessage="Unable to find a pointer associated with index: "//asCharacter(indx),  &
      !                iLogLevel=LOG_DEBUG,                                                               &
      !                sModule=__FILE__,                                                                  &
      !                iLine=__LINE__ )
    endif

  end function list_get_value_at_index_fn


  function list_get_pointer_at_index_fn(this, indx)   result( string_element_ptr )

    class (STRING_LIST_T), intent(in)        :: this
    integer (kind=c_int), intent(in)         :: indx
    type (STRING_LIST_ELEMENT_T), pointer    :: string_element_ptr

    ! [ LOCALS ]
    integer (kind=c_int)                      :: icount
    type (STRING_LIST_ELEMENT_T), pointer     :: current

    icount = 0

    current => this%first

    do while ( associated( this%current ) )

      icount = icount + 1

      if (icount == indx)  exit

      current => current%next

    enddo

    if (associated(current) ) then
      string_element_ptr => current
    else
      string_element_ptr => null()
    endif

  end function list_get_pointer_at_index_fn

  !--------------------------------------------------------------------------------------------------

  !> Iterate over a range of indices; return a space-delimited string comprised of the values.
  function list_get_values_in_range_fn(this, startIndex, endIndex)   result(text)

    class (STRING_LIST_T), intent(inout)     :: this
    integer (kind=c_int), intent(in)         :: startIndex
    integer (kind=c_int), intent(in)         :: endIndex
    character (len=:), allocatable           :: text

    ! [ LOCALS ]
    integer (kind=c_int)                      :: icount
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    icount = 0
    text = " "

    this%current => this%first

    do while ( associated( current ) .and. icount < this%count )

      icount = icount + 1

      if (icount == startIndex ) then
        text = this%current%s
      elseif (icount > startIndex .and. icount <= endIndex ) then
        text = text//" "//this%current%s
      endif

      this%current => this%current%next

    enddo

    if ( len_trim(text) == 0 ) then
      text = "<NA>"
      !      call warn("Unable to find a pointer associated with index range: " &
      !          //asCharacter(startIndex)//" to "//asCharacter(endIndex), &
      !          __FILE__, __LINE__ )
    endif

  end function list_get_values_in_range_fn

  !--------------------------------------------------------------------------------------------------

  subroutine list_print_sub(this, iLU)

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(inout)  :: this
    integer (kind=c_int), optional        :: iLU

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iLU_
    integer (kind=c_int)                      :: icount

    if (present(iLU) ) then
      iLU_ = iLU
    else
      iLU_ = OUTPUT_UNIT
    endif

    this%current => this%first
    icount = 0

    do while ( associated( this%current ) .and. icount < this%last%indexVal )

      icount = icount + 1

      write(iLU_, fmt="(t5,a,6(t20,a))") "["//asCharacter(this%current%indexVal)//"] ", &
        this%current%s

      this%current => this%current%next

    enddo

  end subroutine list_print_sub

  !--------------------------------------------------------------------------------------------------

  function list_all_fn(this)  result( sListValues )

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(inout)  :: this
    character (len=:), allocatable        :: sListValues

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iLU_
    integer (kind=c_int)                      :: icount
    character (len=2048)                      :: sBuf

    this%current => this%first
    icount = 0

    sBuf = ""

    do while ( associated( this%current ) .and. icount < this%count )

      icount = icount + 1

      sBuf = trim(sBuf)//" ("//asCharacter(icount)//") "//this%current%s

      this%current => this%current%next

    enddo

    sListValues = adjustl( trim(sBuf) )

  end function list_all_fn

!------------------------------------------------------------------------------

  function list_update_count_fn(this)          result( icount )

    class (STRING_LIST_T), intent(in)  :: this
    integer (kind=c_int)               :: icount

    type (STRING_LIST_ELEMENT_T), pointer   :: current

    icount = 0
    current => this%first

    do while ( associated( current ) .and. icount < this%last%indexVal )

      icount = icount + 1
      current => current%next

    enddo

    nullify( current )

  end function list_update_count_fn

  !--------------------------------------------------------------------------------------------------

  function list_all_delimited_fn(this, delimiter)  result( sListValues )

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(inout)  :: this
    character (len=*), intent(in)         :: delimiter
    character (len=:), allocatable        :: sListValues

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iLU_
    integer (kind=c_int)                      :: icount
    character (len=2048)                      :: sBuf

    this%current => this%first
    icount = 0

    sBuf = ""

    do while ( associated( this%current ) .and. icount <= this%count )

      icount = icount + 1

      if ( icount == 1 ) then

        sBuf = trim( this%current%s )

      elseif( icount == this%count ) then

        sBuf = trim( sBuf )//trim( delimiter )//trim( this%current%s )//trim( delimiter )

      else

        sBuf = trim( sBuf )//trim( delimiter )//trim( this%current%s )

      endif

      this%current => this%current%next

    enddo

    sListValues = adjustl( trim(sBuf) )

  end function list_all_delimited_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_float_fn(this)    result(rValues)

    class (STRING_LIST_T), intent(inout)  :: this
    real (kind=c_float), allocatable      :: rValues(:)

    ! [ LOCALS ]
    integer (kind=c_int)                      :: status
    integer (kind=c_int)                      :: indx

    allocate( rValues( 1:this%count ), stat=status )
    if (status /= 0)  call die("Failed to allocate memory for list conversion", __FILE__, __LINE__)

    this%current => this%first
    indx = 0

    do while ( associated( this%current ) .and. indx < ubound(rValues,1) )

      indx = indx + 1
      rValues(indx) = asFloat( this%current%s )

      this%current => this%current%next

    enddo

  end function list_return_all_as_float_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_character_fn(this, null_terminated )    result(sValues)

    class (STRING_LIST_T), intent(inout)  :: this
    character (len=64), allocatable       :: sValues(:)
    logical (kind=c_bool), optional       :: null_terminated

    ! [ LOCALS ]
    integer (kind=c_int)                      :: status
    integer (kind=c_int)                      :: indx
    logical (kind=c_bool)                     :: null_terminated_

    if ( present( null_terminated) ) then
      null_terminated_ = null_terminated
    else
      null_terminated_ = FALSE
    endif

    allocate( sValues( 1:this%count ), stat=status )
    if (status /= 0)  call die("Failed to allocate memory for list conversion", __FILE__, __LINE__)

    this%current => this%first
    indx = 0

    do while ( associated( this%current ) .and. indx < ubound(sValues,1) )

      indx = indx + 1

      if ( null_terminated_ ) then
        sValues(indx) = trim(this%current%s)//c_null_char
      else
        sValues(indx) = this%current%s
      endif

      this%current => this%current%next

    enddo

  end function list_return_all_as_character_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_int_fn(this)    result(iValues)

    class (STRING_LIST_T), intent(inout)  :: this
    integer (kind=c_int), allocatable     :: iValues(:)

    ! [ LOCALS ]
    integer (kind=c_int)                      :: status
    integer (kind=c_int)                      :: indx

    allocate( iValues(this%count ), stat=status )
    if (status /= 0)  call die("Failed to allocate memory for list conversion", __FILE__, __LINE__)

    this%current => this%first
    indx = 0

    do while ( associated( this%current ) .and. indx < ubound(iValues,1) )

      indx = indx + 1

      iValues(indx) = asInt( this%current%s )

      this%current => this%current%next

    enddo

  end function list_return_all_as_int_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_logical_fn(this)    result(lValues)

    class (STRING_LIST_T), intent(inout)   :: this
    logical (kind=c_bool), allocatable     :: lValues(:)

    ! [ LOCALS ]
    integer (kind=c_int)                      :: status
    integer (kind=c_int)                      :: indx

    allocate( lValues( this%count ), stat=status )
    if (status /= 0)  call die("Failed to allocate memory for list conversion", __FILE__, __LINE__)

    this%current => this%first
    indx = 0

    do while ( associated( this%current ) .and. indx < ubound(lValues,1) )

      indx = indx + 1

      lValues(indx) = asLogical( this%current%s )

      this%current => this%current%next

    enddo

  end function list_return_all_as_logical_fn

  !--------------------------------------------------------------------------------------------------

  function list_from_delimited_string_fn(sText1, delimiters)    result( newList )

    character (len=*), intent(in)           :: sText1
    character (len=*), intent(in), optional :: delimiters
    type (STRING_LIST_T)                    :: newList

    ! [ LOCALS ]
    character (len=:), allocatable :: sTempText
    character (len=:), allocatable :: sTempArg
    character (len=256)            :: delimiters_

    if ( present( delimiters ) ) then
      delimiters_ = delimiters
    else
      delimiters_ = PUNCTUATION
    endif

    sTempText = sText1
    sTempArg = sText1

    do

      call chomp( text1=sTempText, text2=sTempArg, delimiterChars=delimiters_ )

      if (len_trim(sTempArg) > 0) then
        call newList%append( trim( adjustl( sTempArg ) ) )
      else
        exit
      endif

    end do

  end function list_from_delimited_string_fn

  !--------------------------------------------------------------------------------------------------
  !!
  subroutine list_from_delimited_string_sub(this, text, delimiters)

    class (STRING_LIST_T), intent(inout)    :: this
    character (len=*), intent(in)           :: text
    character (len=*), intent(in), optional :: delimiters

    ! [ LOCALS ]
    character (len=:), allocatable    :: sTempText
    character (len=:), allocatable    :: sTempArg
    character (len=256)    :: delimiters_

    if ( present( delimiters ) ) then
      delimiters_ = delimiters
    else
      delimiters_ = PUNCTUATION
    endif

    sTempText = text
    sTempArg = ""

    call this%clear()

    do

      call chomp( text1=sTempText, text2=sTempArg, delimiterChars=trim(delimiters_) )

      if (len_trim(sTempArg ) > 0) then
        call this%append( sTempArg )
      else
        exit
      endif

    end do

  end subroutine list_from_delimited_string_sub

  !--------------------------------------------------------------------------------------------------

  function list_is_string_in_list_fn(this, sChar)  result( lResult )

    class (STRING_LIST_T), intent(inout) :: this
    character (len=*), intent(in)        :: sChar
    logical (kind=c_bool)                :: lResult

    ! [ LOCALS ]
    integer (kind=c_int) :: icount

    icount = 0

    icount = this%countmatching( sChar )

    if ( icount > 0 ) then
      lResult = lTRUE
    else
      lResult = lFALSE
    endif

  end function list_is_string_in_list_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_count_of_matching_string_fn(this, sChar) result(icount)

    class (STRING_LIST_T), intent(inout) :: this
    character (len=*), intent(in)        :: sChar
    integer (kind=c_int)                 :: icount

    ! [ LOCALS ]
    integer (kind=c_int) :: indx
    integer (kind=c_int) :: status
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%count) :: iTempResult

    icount = 0
    indx = 0

    this%current => this%first

    do while ( associated(this%current) .and. icount < this%count )

      indx = indx + 1

      if ( (this%current%s .strequal. sChar) .or. index(this%current%s, sChar) /=0 ) then
        icount = icount + 1
        iTempResult(icount) = indx
      endif

      this%current => this%current%next

    enddo

  end function list_return_count_of_matching_string_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_position_of_matching_string_fn(this, sChar)     result(iResult)

    class (STRING_LIST_T), intent(inout)                 :: this
    character (len=*), intent(in)                        :: sChar
    integer (kind=c_int), dimension(:), allocatable      :: iResult

    ! [ LOCALS ]
    integer (kind=c_int) :: indx
    integer (kind=c_int) :: icount
    integer (kind=c_int) :: status
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%count) :: iTempResult

    indx = 0
    icount = 0

    this%current => this%first

    do while ( associated(this%current) .and. indx < this%count )

      indx = indx + 1

      if ( (this%current%s .strequal. sChar) .or. index(this%current%s, sChar) /=0 ) then
        icount = icount + 1
        iTempResult(icount) = indx
      endif

      this%current => this%current%next

    enddo

    if (icount == 0) then

      allocate(iResult(1), stat=status)
      if (status /= 0)   call die("Problem allocating memory", __FILE__, __LINE__)

      iResult(1) = -9999

    else

      allocate(iResult(icount), stat=status)
      if (status /= 0)   call die("Problem allocating memory", __FILE__, __LINE__)

      iResult(1:icount) = iTempResult(1:icount)

    endif

  end function list_return_position_of_matching_string_fn

  !--------------------------------------------------------------------------------------------------

  function list_subset_partial_matches_fn( this, sChar )     result(newList)

    class (STRING_LIST_T), intent(inout)                 :: this
    character (len=*), intent(in)                        :: sChar
    type (STRING_LIST_T)                                 :: newList

    ! [ LOCALS ]
    integer (kind=c_int) :: indx
    integer (kind=c_int) :: status
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int) :: icount
    integer (kind=c_int), dimension(this%count) :: iTempResult

    icount = 0

    this%current => this%first

    do while ( associated(this%current) .and. icount < this%count )

      if ( (this%current%s .strequal. sChar) .or. &
          (index(string=asUppercase(this%current%s), substring=asUppercase(sChar) ) > 0 ) ) then

        icount = icount + 1
        call newList%append(this%current%s)

      endif

      this%current => this%current%next

    enddo

  end function list_subset_partial_matches_fn

  !--------------------------------------------------------------------------------------------------

  subroutine list_finalize_sub(this)

    type (STRING_LIST_T) :: this

    if ( this%autocleanup )  call this%clear()

  end subroutine list_finalize_sub

  !--------------------------------------------------------------------------------------------------

  subroutine list_items_deallocate_all_sub(this)

    class (STRING_LIST_T), intent(inout) :: this

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer :: current
    type (STRING_LIST_ELEMENT_T), pointer :: toremove

    current => null()
    toremove => null()

    if ( associated( this%first ) ) then

      current => this%first

      do while ( associated( current ) )

        toremove => current

        current => current%next

        if ( allocated( toremove%s ) )  deallocate( toremove%s )

        if ( associated( toremove ) )  deallocate( toremove )

      enddo

    endif

    this%count = 0
    this%first => null()
    this%last => null()
    this%current => null()

  end subroutine list_items_deallocate_all_sub

!------------------------------------------------------------------------------

  recursive subroutine list_sort_sub( string_list )

    type (STRING_LIST_T), intent(in) :: string_list

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer :: left
    type (STRING_LIST_ELEMENT_T), pointer :: right
    type (STRING_LIST_ELEMENT_T), pointer :: pivot
    type (STRING_LIST_ELEMENT_T), pointer :: marker
    type (STRING_LIST_T)                  :: list_left_chunk
    type (STRING_LIST_T)                  :: list_right_chunk

    real (kind=c_float)  :: random_value
    integer (kind=c_int) :: pivot_index
    integer (kind=c_int) :: itemCount
    integer (kind=c_int) :: minIndex
    integer (kind=c_int) :: maxIndex

    itemCount = string_list%get_count()

    minIndex = string_list%first%indexVal
    maxIndex = string_list%last%indexVal

    print *, "+++ ITEM COUNT: ", itemCount

    if (itemCount > 1) then

        call random_number(random_value)
        pivot_index = int( 1 + random_value * (itemCount-1), kind=c_int )

        print *, "++++ PIVOT INDEX: ", pivot_index
        pivot => string_list%get_pointer( pivot_index )

        left => string_list%first
        right => string_list%last

        print *, "l, r: ", left%indexVal, right%indexVal, pivot%indexVal

        do
          do while ( right%s > pivot%s )
            print *, right%s, pivot%s
            if ( associated( right%previous ) )  right => right%previous
          end do

          do while ( left%s < pivot%s )
            print *, left%s, pivot%s
            if ( associated( left%next ) )   left => left%next
          end do

          if ( left%s < right%s )   call swap_list_values( left, right )

          exit

        end do

        if (left%s == right%s) then
            if ( associated( left%next ) )   marker => left%next
        else
            marker => left
        end if

        list_left_chunk%first => string_list%first
        if ( associated( marker%previous) )  list_left_chunk%last => marker%previous

        list_right_chunk%last => string_list%last
        list_right_chunk%first => marker

        print *, marker%indexVal
        print *, marker%previous%indexVal

        print *, "== LEFT CHUNK =="
        call list_left_chunk%print()
        print *, "== RIGHT CHUNK =="
        call list_right_chunk%print()



        call list_sort_sub( list_left_chunk )
        call list_sort_sub( list_right_chunk )

      end if

    end subroutine list_sort_sub

end module string_list
