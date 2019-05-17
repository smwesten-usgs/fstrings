module fstrings

  use iso_c_binding
  implicit none

  private

  public :: FSTRINGS_T

  public :: assignment(=)
  interface assignment(=)
    module procedure   :: assign_fstring_to_character_sub
  end interface assignment(=)

  private::f_to_c_str
  interface f_to_c_str
    module procedure :: f_to_c_string_fn
  end interface f_to_c_str

  private::c_to_f_str
  interface c_to_f_str
    module procedure :: c_to_f_string_fn
  end interface c_to_f_str

  public::chomp
  interface chomp
    module procedure :: split_and_return_text_sub
  end interface chomp

  type FSTRINGS_T

    character (len=:), allocatable        :: s
    integer (c_int)                       :: str_count

  contains

    procedure   :: assign_character_to_fstring_sub
    generic     :: assignment(=) => assign_character_to_fstring_sub

    procedure   :: print_all_entries_sub
    generic     :: print_all => print_all_entries_sub

    procedure   :: append_character_to_fstring_sub
    procedure   :: append_character_array_to_fstring_sub
    generic     :: append => append_character_to_fstring_sub,                   &
                             append_character_array_to_fstring_sub

    procedure   :: count_strings_in_list_fn
    generic     :: count => count_strings_in_list_fn

    procedure   :: retrieve_value_from_list_at_index_fn
    generic     :: get => retrieve_value_from_list_at_index_fn

    procedure   :: retrieve_values_as_integer_fn
    generic     :: get_integer => retrieve_values_as_integer_fn

    procedure   :: retrieve_values_as_float_fn
    generic     :: get_float => retrieve_values_as_float_fn

    procedure   :: retrieve_values_as_double_fn
    generic     :: get_double => retrieve_values_as_double_fn

    procedure   :: retrieve_values_as_logical_fn
    generic     :: get_logical => retrieve_values_as_logical_fn

    procedure   :: quicksort_sub
    generic     :: sort => quicksort_sub

    procedure   :: clear_list_sub
    generic     :: clear => clear_list_sub

  end type FSTRINGS_T

  type SORT_GROUP_T
      integer                       :: order    ! original order of unsorted data
      character(len=:), allocatable :: value    ! values to be sorted by
  end type SORT_GROUP_T

  integer (c_int), parameter  :: NA_INT    = - (huge(1_c_int)-1_c_int)
  real (c_float), parameter   :: NA_FLOAT  = - (huge(1._c_float)-1._c_float)
  real (c_double), parameter  :: NA_DOUBLE = - (huge(1._c_double)-1._c_double)

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

!--------------------------------------------------------------------------------------------------

  function f_to_c_string_fn(f_character_str)   result(c_character_str)

    character (len=*), intent(in)                    :: f_character_str
    character (len=:), allocatable                   :: c_character_str

    c_character_str = trim(f_character_str)//c_null_char

  end function f_to_c_string_fn

!--------------------------------------------------------------------------------------------------

  subroutine assign_character_to_fstring_sub(this, character_str)

    class (FSTRINGS_T), intent(inout)                :: this
    character (len=*), intent(in)                    :: character_str

    call this%clear()

    this%s = c_to_f_str(trim(character_str))//c_null_char
    this%str_count = 1

  end subroutine assign_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_fstring_to_character_sub(character_str, this)

    character (len=:), allocatable, intent(out)      :: character_str
    type (FSTRINGS_T), intent(inout)                 :: this

    character_str = this%get(1)

  end subroutine assign_fstring_to_character_sub

!--------------------------------------------------------------------------------------------------

  function count_strings_in_list_fn(this)        result(str_count)

    class (FSTRINGS_T), intent(inout), target        :: this
    integer (c_int)                                  :: str_count

    integer (c_int) :: i

    str_count = 0

    do i=1, len_trim(this%s)
      if( this%s(i:i) == c_null_char ) str_count = str_count + 1
    enddo

  end function count_strings_in_list_fn

!--------------------------------------------------------------------------------------------------

  subroutine append_character_to_fstring_sub(this, character_str)

    class (FSTRINGS_T), intent(inout), target        :: this
    character (len=*), intent(in)                    :: character_str

    this%s = trim(this%s)//trim(adjustl(c_to_f_str(character_str)))//c_null_char
    this%str_count = this%str_count + 1

  end subroutine append_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

subroutine append_character_array_to_fstring_sub(this, character_str)

  class (FSTRINGS_T), intent(inout), target        :: this
  character (len=*), intent(in)                    :: character_str(:)

  integer (c_int) :: i

  do i=1, size(character_str,1)

    this%s = trim(this%s)//trim(adjustl(c_to_f_str(character_str(i))))//c_null_char
    this%str_count = this%str_count + 1

  enddo

end subroutine append_character_array_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  subroutine print_all_entries_sub(this)

    class (FSTRINGS_T), intent(inout), target   :: this

    character (len=:), allocatable :: sbuf
    integer (c_int)                :: start_pos
    integer (c_int)                :: end_pos
    integer (c_int)                :: str_len
    integer (c_int)                :: i

    start_pos = 1
    end_pos = index( this%s, c_null_char ) - 1
    str_len = len_trim( this%s )

    do i=1, this%count()

      write(*,fmt="(a)") this%s(start_pos:end_pos)

      start_pos = end_pos + 2
      end_pos = index( this%s(start_pos:str_len), c_null_char ) + start_pos - 2

    end do

  end subroutine print_all_entries_sub

!--------------------------------------------------------------------------------------------------

  subroutine list_finalize_sub(this)

    type (FSTRINGS_T), intent(inout)          :: this

    call this%clear()

  end subroutine list_finalize_sub

!--------------------------------------------------------------------------------------------------

  subroutine clear_list_sub(this)

    class (FSTRINGS_T), intent(inout)        :: this

    this%s = ""
    this%str_count = 0

  end subroutine clear_list_sub

!--------------------------------------------------------------------------------------------------

  function retrieve_values_as_integer_fn(this)   result(values)

    class (FSTRINGS_T), intent(inout)         :: this
    integer (c_int), allocatable              :: values(:)

    integer (c_int)    :: i
    integer (c_int)    :: value
    integer (c_int)    :: op_status
    character (len=64) :: sbuf

    allocate(values(this%str_count),stat=op_status)

    do i=1,this%str_count
      sbuf = this%get(i)
      read(unit=sbuf, fmt=*, iostat=op_status) value
      if ( op_status==0 ) then
        values(i) = value
      else
        values(i) = NA_INT
      endif
    enddo

  end function retrieve_values_as_integer_fn

!--------------------------------------------------------------------------------------------------

  function retrieve_values_as_float_fn(this)   result(values)

    class (FSTRINGS_T), intent(inout)         :: this
    real (c_float), allocatable               :: values(:)

    integer (c_int)    :: i
    real (c_float)     :: value
    integer (c_int)    :: op_status
    character (len=64) :: sbuf

    allocate(values(this%str_count),stat=op_status)

    do i=1,this%str_count
      sbuf = this%get(i)
      read(unit=sbuf, fmt=*, iostat=op_status) value
      if ( op_status==0 ) then
        values(i) = value
      else
        values(i) = NA_FLOAT
      endif
    enddo

  end function retrieve_values_as_float_fn

!--------------------------------------------------------------------------------------------------

function retrieve_values_as_double_fn(this)   result(values)

  class (FSTRINGS_T), intent(inout)         :: this
  real (c_double), allocatable              :: values(:)

  integer (c_int)    :: i
  real (c_double)    :: value
  integer (c_int)    :: op_status
  character (len=64) :: sbuf

  allocate(values(this%str_count),stat=op_status)

  do i=1,this%str_count
    sbuf = this%get(i)
    read(unit=sbuf, fmt=*, iostat=op_status) value
    if ( op_status==0 ) then
      values(i) = value
    else
      values(i) = NA_DOUBLE
    endif
  enddo

end function retrieve_values_as_double_fn

!--------------------------------------------------------------------------------------------------

function retrieve_values_as_logical_fn(this)   result(values)

  class (FSTRINGS_T), intent(inout)         :: this
  logical (c_bool), allocatable             :: values(:)

  integer (c_int)    :: i
  logical (c_bool)   :: value
  integer (c_int)    :: op_status
  character (len=64) :: sbuf

  allocate(values(this%str_count),stat=op_status)

  do i=1,this%str_count
    sbuf = this%get(i)

    select case(sbuf)

      case("true","T","True","TRUE","1","Y","Yes","yes","YES")
        values(i) = .TRUE._c_bool
      case default
        values(i) = .FALSE._c_bool

    end select

  enddo

end function retrieve_values_as_logical_fn

!--------------------------------------------------------------------------------------------------

  function retrieve_value_from_list_at_index_fn(this, index_val)   result(text)

    class (FSTRINGS_T), intent(inout)         :: this
    integer (c_int), intent(in)               :: index_val
    character(len=:), allocatable             :: text

    integer (c_int)                :: start_pos
    integer (c_int)                :: end_pos
    integer (c_int)                :: str_len
    integer (c_int)                :: i

    start_pos = 1
    end_pos = index( this%s, c_null_char ) - 1
    str_len = len_trim( this%s )

    text = "<NA>"

    do i=1, this%str_count

      if ( index_val == i ) then
        text = this%s(start_pos:end_pos)
        exit
      endif

      ! skip over the 'c_null_char' to position of beginning of
      ! next substring
      start_pos = end_pos + 2
      end_pos = index( this%s(start_pos:str_len), c_null_char ) + start_pos - 2

    end do

  end function retrieve_value_from_list_at_index_fn

!--------------------------------------------------------------------------------------------------

  subroutine quicksort_sub(this)

    class (FSTRINGS_T), intent(inout)         :: this

    type (SORT_GROUP_T), allocatable :: sort_group(:)
    integer (c_int)                  :: i
    integer (c_int)                  :: str_count

    str_count = this%str_count

    allocate(sort_group(str_count))

    ! create the 'sort_group' data structure
    do i=1, str_count
      sort_group(i)%order = i
      sort_group(i)%value = this%get(i)
    enddo

    call qsort(sort_group, this%str_count)

    ! wipe out previous values
    call this%clear()

    ! copy sorted values back into list structure
    do i=1, str_count
      call this%append(sort_group(i)%value)
    enddo

  end subroutine quicksort_sub

!--------------------------------------------------------------------------------------------------

  recursive subroutine qsort(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                          :: nrec

  ! LOCAL VARIABLES
  integer                          :: left, right
  real                             :: random
  character (len=:), allocatable   :: pivot
  type (SORT_GROUP_T)              :: temp
  integer                          :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%value < pivot)
                  left = left + 1
              end do
              if (left < right) then
                  temp = sort_group(left)
                  sort_group(left) = sort_group(right)
                  sort_group(right) = temp
              end if
          end do

          if (left == right) then
              marker = left + 1
          else
              marker = left
          end if

          call qsort(sort_group(:marker-1),marker-1)
          call qsort(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort

!--------------------------------------------------------------------------------------------------

  subroutine split_and_return_text_sub(str, substr, delimiter_chr)

    character (len=*), intent(inout)                     :: str
    character (len=*), intent(out)                       :: substr
    character (len=*), intent(in), optional              :: delimiter_chr

    ! [ LOCALS ]
    character (len=:), allocatable :: delimiter_chr_
    integer (kind=c_int) :: iIndex

    if ( present(delimiter_chr) ) then
      delimiter_chr_ = delimiter_chr
    else
      delimiter_chr_ = ","
    endif

    str = adjustl(str)

    iIndex = scan( string = str, set = delimiter_chr_ )

    if (iIndex == 0) then
      ! no delimiters found; return string as was supplied originally
      substr = str
      str = ""
    else
      ! delimiters were found; split and return the chunks of text
      substr = trim( str(1:iIndex-1) )
      str = trim( adjustl( str(iIndex + 1:) ) )
    endif

  end subroutine split_and_return_text_sub


end module fstrings
