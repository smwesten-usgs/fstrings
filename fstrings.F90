module fstrings

  use iso_c_binding
  implicit none

  private

  public :: FSTRINGS_T

  ! public :: operator(+)
  ! interface operator(+)
  !   module procedure   :: concatenate_fstring_to_fstring_sub
  ! end interface operator(+)
  !
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

  private::as_character
  interface as_character
    module procedure :: int_to_char_fn
    module procedure :: float_to_char_fn
  end interface as_character

  public::chomp
  interface chomp
    module procedure :: split_and_return_text_sub
  end interface chomp

  public::split
  interface split
    module procedure :: split_character_into_fstring_list_fn
  end interface split

  private :: operator( .contains. )
  interface operator( .contains. )
    procedure :: is_substring_present_in_string_case_sensitive_fn
  end interface operator( .contains. )

  private :: operator( .containssimilar. )
  interface operator( .containssimilar. )
    procedure :: is_substring_present_in_string_case_insensitive_fn
  end interface operator( .containssimilar. )

  private :: upper
  interface upper
    procedure :: char_to_uppercase_fn
  end interface upper

  private :: lower
  interface lower
    procedure :: char_to_lowercase_fn
  end interface lower

  type FSTRINGS_T

    character (len=:), allocatable        :: s
    integer (c_int)                       :: str_count = 0

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

    procedure   :: quicksort_alpha_sub
    generic     :: sort => quicksort_alpha_sub

    procedure   :: quicksort_int_sub
    generic     :: sort_integer => quicksort_int_sub

    procedure   :: quicksort_float_sub
    generic     :: sort_float => quicksort_float_sub

    procedure   :: clear_list_sub
    generic     :: clear => clear_list_sub

    procedure   :: return_count_of_matching_strings_fn
    generic     :: count_matching => return_count_of_matching_strings_fn

!    procedure   :: is_substring_present_in_string_case_sensitive_fn
!    procedure   :: is_substring_present_in_string_case_insensitive_fn


    procedure   :: return_subset_of_partial_matches_fn
    generic     :: grep => return_subset_of_partial_matches_fn

    procedure   :: return_list_of_unique_values_fn
    generic     :: unique => return_list_of_unique_values_fn

  end type FSTRINGS_T

  integer (c_int), parameter  :: NA_INT    = - (huge(1_c_int)-1_c_int)
  real (c_float), parameter   :: NA_FLOAT  = - (huge(1._c_float)-1._c_float)
  real (c_double), parameter  :: NA_DOUBLE = - (huge(1._c_double)-1._c_double)

  type ALPHA_SORT_GROUP_T
    integer (c_int)                :: order
    character (len=:), allocatable :: alpha_value
  end type ALPHA_SORT_GROUP_T

  type INT_SORT_GROUP_T
    integer (c_int)                :: order
    integer (c_int)                :: int_value
  end type INT_SORT_GROUP_T

  type FLOAT_SORT_GROUP_T
    integer (c_int)                :: order
    real (c_float)                 :: float_value
  end type FLOAT_SORT_GROUP_T

contains

  function c_to_f_string_fn(c_character_str)   result(f_character_str)

    character (len=*), intent(in)                    :: c_character_str
    character (len=:), allocatable                   :: f_character_str

    integer (c_int)   :: indx

    f_character_str = c_character_str

    do indx=1,len(c_character_str)
      if (c_character_str(indx:indx) == c_null_char) then
        f_character_str = c_character_str(1:indx-1)
        exit
      endif
    enddo

  end function c_to_f_string_fn

!--------------------------------------------------------------------------------------------------

  function f_to_c_string_fn(f_character_str)   result(c_character_str)

    character (len=*), intent(in)                    :: f_character_str
    character (len=:), allocatable                   :: c_character_str

    integer (c_int) :: str_len

    str_len = len_trim(f_character_str)

    if ( f_character_str(str_len:str_len) /= c_null_char ) then
      ! last char is not null character; append c_null_char

      c_character_str = trim(f_character_str)//c_null_char

    else
      ! already has a null character at end; do not append another

      c_character_str = trim(f_character_str)

    endif

  end function f_to_c_string_fn

!--------------------------------------------------------------------------------------------------

  subroutine assign_character_to_fstring_sub(this, character_str)

    class (FSTRINGS_T), intent(inout)                :: this
    character (len=*), intent(in)                    :: character_str

    call this%clear()

    this%s = f_to_c_str(character_str)
    this%str_count = 1

  end subroutine assign_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_fstring_to_character_sub(character_str, this)

    character (len=:), allocatable, intent(out)      :: character_str
    type (FSTRINGS_T), intent(inout)                 :: this

    character_str = this%get(1)

  end subroutine assign_fstring_to_character_sub

!--------------------------------------------------------------------------------------------------

  function split_character_into_fstring_list_fn(character_str, delimiter_chr)   result(new_fstring)

    character (len=*), intent(in)                    :: character_str
    character (len=1), intent(in), optional          :: delimiter_chr
    type (FSTRINGS_T)                                :: new_fstring

    character (len=len(character_str))  :: string
    character (len=len(character_str))  :: substring
    character (len=1)                   :: delimiter_chr_

    if ( present(delimiter_chr) ) then
      delimiter_chr_ = delimiter_chr
    else
      delimiter_chr_ = ","
    endif

    string = character_str

    do

      call chomp(string, substring, delimiter_chr_)

      if ( len_trim(substring) == 0 ) exit

      call new_fstring%append( substring )

    end do

  end function split_character_into_fstring_list_fn

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

    if ( .not. allocated( this%s ) )  this%s = ""
    this%s = trim(this%s)//trim(adjustl(f_to_c_str(character_str)))
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

  subroutine quicksort_alpha_sub(this, sort_order)

    class (FSTRINGS_T), intent(inout)         :: this
    character (len=*), intent(in), optional   :: sort_order

    type (ALPHA_SORT_GROUP_T), allocatable :: sort_group(:)
    integer (c_int)                        :: i
    integer (c_int)                        :: str_count
    logical (c_bool)                       :: decreasing_order

    decreasing_order = .false._c_bool

    if ( present(sort_order) ) then
      select case (sort_order)

      case ("Decreasing","decreasing","DECREASING")
        decreasing_order = .true._c_bool
      end select
    endif

    str_count = this%str_count

    allocate(sort_group(str_count))

    ! create the 'sort_group' data structure
    do i=1, str_count
      sort_group(i)%order = i
      sort_group(i)%alpha_value = this%get(i)
    enddo

    call qsort_alpha(sort_group, this%str_count)

    ! wipe out previous values
    call this%clear()

    if ( decreasing_order ) then
      ! copy sorted values back into list structure (DECREASING ORDER)
      do i=str_count, 1, -1
        call this%append(sort_group(i)%alpha_value)
      enddo
    else
      ! copy sorted values back into list structure (INCREASING ORDER)
      do i=1, str_count
        call this%append(sort_group(i)%alpha_value)
      enddo
    endif

  end subroutine quicksort_alpha_sub

!-------------------------------------------------------------------------------------------------

  subroutine quicksort_int_sub(this, sort_order)
    class (FSTRINGS_T), intent(inout)         :: this
    character (len=*), intent(in), optional   :: sort_order

    type (INT_SORT_GROUP_T), allocatable   :: sort_group(:)
    integer (c_int), allocatable           :: int_values(:)
    integer (c_int)                        :: i
    integer (c_int)                        :: str_count
    logical (c_bool)                       :: decreasing_order

    decreasing_order = .false._c_bool

    if ( present(sort_order) ) then
      select case (sort_order)
      case ("Decreasing","decreasing","DECREASING")
        decreasing_order = .true._c_bool
      end select
    endif

    str_count = this%str_count

    allocate(sort_group(str_count))
    allocate(int_values(str_count))

    int_values = this%get_integer()

    ! create the 'sort_group' data structure
    do i=1, str_count
      sort_group(i)%order = i
      sort_group(i)%int_value = int_values(i)
    enddo

    call qsort_int(sort_group, this%str_count)
    ! wipe out previous values
    call this%clear()
    if ( decreasing_order ) then
      ! copy sorted values back into list structure (DECREASING ORDER)
      do i=str_count, 1, -1
        call this%append( as_character(sort_group(i)%int_value) )
      enddo
    else
      ! copy sorted values back into list structure (INCREASING ORDER)
      do i=1, str_count
        call this%append( as_character(sort_group(i)%int_value) )
      enddo
    endif
  end subroutine quicksort_int_sub

!-------------------------------------------------------------------------------------------------

  subroutine quicksort_float_sub(this, sort_order)

    class (FSTRINGS_T), intent(inout)         :: this
    character (len=*), intent(in), optional   :: sort_order

    type (FLOAT_SORT_GROUP_T), allocatable :: sort_group(:)
    real (c_float), allocatable            :: float_values(:)
    integer (c_int)                        :: i
    integer (c_int)                        :: str_count
    logical (c_bool)                       :: decreasing_order

    decreasing_order = .false._c_bool

    if ( present(sort_order) ) then
      select case (sort_order)

        case ("Decreasing","decreasing","DECREASING")
          decreasing_order = .true._c_bool
      end select
    endif

    str_count = this%str_count

    allocate(sort_group(str_count))

    float_values = this%get_float()

    ! create the 'sort_group' data structure
    do i=1, str_count
      sort_group(i)%order = i
      sort_group(i)%float_value = float_values(i)
    enddo

    call qsort_float(sort_group, this%str_count)

    ! wipe out previous values
    call this%clear()

    if ( decreasing_order ) then
      ! copy sorted values back into list structure (DECREASING ORDER)
      do i=str_count, 1, -1
        call this%append( as_character(sort_group(i)%float_value) )
      enddo
    else
      ! copy sorted values back into list structure (INCREASING ORDER)
      do i=1, str_count
        call this%append( as_character(sort_group(i)%float_value) )
      enddo
    endif

  end subroutine quicksort_float_sub

!-------------------------------------------------------------------------------------------------

  recursive subroutine qsort_alpha(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (ALPHA_SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                                :: nrec

  ! LOCAL VARIABLES
  integer (c_int)                  :: left, right
  real (c_float)                   :: random
  character (len=:), allocatable   :: pivot
  type (ALPHA_SORT_GROUP_T)        :: temp
  integer (c_int)                  :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%alpha_value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%alpha_value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%alpha_value < pivot)
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

          call qsort_alpha(sort_group(:marker-1),marker-1)
          call qsort_alpha(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort_alpha

!--------------------------------------------------------------------------------------------------

  recursive subroutine qsort_int(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (INT_SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                              :: nrec

  ! LOCAL VARIABLES
  integer (c_int)                  :: left, right
  real (c_float)                   :: random
  integer (c_int)                  :: pivot
  type (INT_SORT_GROUP_T)          :: temp
  integer (c_int)                  :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%int_value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%int_value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%int_value < pivot)
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

          call qsort_int(sort_group(:marker-1),marker-1)
          call qsort_int(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort_int

!--------------------------------------------------------------------------------------------------

  recursive subroutine qsort_float(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (FLOAT_SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                                :: nrec

  ! LOCAL VARIABLES
  integer (c_int)                  :: left, right
  real (c_float)                   :: random
  real (c_float)                   :: pivot
  type (FLOAT_SORT_GROUP_T)        :: temp
  integer (c_int)                  :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%float_value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%float_value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%float_value < pivot)
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

          call qsort_float(sort_group(:marker-1),marker-1)
          call qsort_float(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort_float

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


  function int_to_char_fn(value)    result(text)
    integer (c_int), intent(in)     :: value
    character (len=:), allocatable  :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf
    write(sbuf, fmt=*, iostat=status)  value
    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif
  end function int_to_char_fn


  function float_to_char_fn(value)    result(text)
    real (c_float), intent(in)     :: value
    character (len=:), allocatable  :: text
    integer (c_int)      :: status
    character (len=32)   :: sbuf
    write(sbuf, fmt=*, iostat=status)  value
    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif
  end function float_to_char_fn




  !--------------------------------------------------------------------------------------------------

  function char_to_uppercase_fn ( str )               result(text)
    ! ARGUMENTS
    character (len=*), intent(in)   :: str
    character(len=len(str))         :: text
    ! LOCALS
    integer (c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (c_int), parameter :: LOWER_TO_UPPER = -32
    integer (c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    text = str

    do i=1,len_trim(text)
      if ( ichar(text(i:i) ) >= ASCII_SMALL_A .and. ichar(text(i:i)) <= ASCII_SMALL_Z ) then
        text(i:i) = char( ichar( text(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end function char_to_uppercase_fn

!--------------------------------------------------------------------------

  function char_to_lowercase_fn ( str )                  result(text)
    ! ARGUMENTS
    character (len=*), intent(in) :: str
    character(len=len(str)) :: text
    ! LOCALS
    integer (c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (c_int), parameter :: UPPER_TO_LOWER = 32
    integer (c_int), parameter :: ASCII_A = ichar("A")
    integer (c_int), parameter :: ASCII_Z = ichar("Z")

    text = str

    do i=1,len_trim(text)
      if ( ichar(text(i:i) ) >= ASCII_A .and. ichar(text(i:i)) <= ASCII_Z ) then
        text(i:i) = char( ichar( text(i:i) ) + UPPER_TO_LOWER )
      end if
    end do

  end function char_to_lowercase_fn

  !--------------------------------------------------------------------------------------------------

  function is_substring_present_in_string_case_insensitive_fn(str, substr)   result(is_present)

    character (len=*), intent(in)      :: str
    character (len=*), intent(in)      :: substr
    logical (c_bool)                   :: is_present

    ! [ LOCALS ]
    character (len=len_trim(str))  :: temp_str
    character (len=len_trim(substr))  :: temp_substr

    is_present = .FALSE._c_bool

    temp_str = upper(str)
    temp_substr = upper(substr)

    if ( index(temp_str, temp_substr) /= 0 ) is_present = .TRUE._c_bool

  end function is_substring_present_in_string_case_insensitive_fn

!--------------------------------------------------------------------------------------------------

  function is_substring_present_in_string_case_sensitive_fn(str, substr)   result(is_present)

    character (len=*), intent(in)      :: str
    character (len=*), intent(in)      :: substr
    logical (c_bool)                   :: is_present

    is_present = .FALSE._c_bool

    if ( index(str, substr) /= 0 ) is_present = .TRUE._c_bool

  end function is_substring_present_in_string_case_sensitive_fn

!--------------------------------------------------------------------------------------------------

  function return_count_of_matching_strings_fn(this, substr, match_case)    result(count)

    class (FSTRINGS_T), intent(inout)       :: this
    character (len=*), intent(in)           :: substr
    logical (c_bool), intent(in), optional  :: match_case
    integer (c_int)                         :: count

    ! [ LOCALS ]
    integer (c_int)  :: i
    integer (c_int)  :: status
    logical (c_bool) :: match_case_

    if ( present( match_case ) ) then
      match_case_ = match_case
    else
      match_case_ = .FALSE._c_bool
    endif

    count = 0

    if ( match_case_ ) then

      do i=1, this%str_count

        if ( this%get(i) .contains. substr )  count = count + 1

      enddo

    else

      do i=1, this%str_count

        if ( this%get(i) .containssimilar. substr )  count = count + 1

      enddo

    endif

  end function return_count_of_matching_strings_fn

!--------------------------------------------------------------------------------------------------

  function return_subset_of_partial_matches_fn( this, substr )     result(new_fstring)

    class (FSTRINGS_T), intent(inout)                  :: this
    character (len=*), intent(in)                      :: substr
    type (FSTRINGS_T)                                  :: new_fstring

    ! [ LOCALS ]
    integer (c_int)                 :: i
    character (len=:), allocatable  :: temp_str

    do i=1, this%str_count
      temp_str = this%get(i)
      if ( temp_str .containssimilar. substr )   call new_fstring%append(temp_str)
    enddo

    if ( new_fstring%str_count == 0 )  new_fstring = "<NA>"

  end function return_subset_of_partial_matches_fn

!--------------------------------------------------------------------------------------------------

  function return_list_of_unique_values_fn(this)    result(new_fstring)

    class (FSTRINGS_T), intent(inout)   :: this
    type (FSTRINGS_T)                   :: new_fstring

    integer (c_int)                :: i
    character (len=:), allocatable :: temp_str

    do i=1, this%str_count

      temp_str = this%get(i)
      if ( new_fstring%count_matching( temp_str ) == 0 )  call new_fstring%append(temp_str)

    enddo

    if ( new_fstring%str_count == 0 )  new_fstring = "<NA>"

  end function return_list_of_unique_values_fn

end module fstrings
