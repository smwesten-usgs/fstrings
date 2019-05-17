module strings

  use iso_c_binding, only : c_int, c_long_long, c_float, c_double, c_bool, c_short
  use constants_and_conversions
!  use exceptions
  implicit none

  private

  public :: operator(+)
  interface operator(+)
    procedure :: concatenate_char_char_fn
    procedure :: concatenate_char_int_fn
    procedure :: concatenate_char_float_fn
    procedure :: concatenate_char_double_fn
  end interface operator(+)

  !   interface assignment(=)
  !     procedure :: integer_to_char_sub
  !     procedure :: float_to_char_sub
  !     procedure :: double_to_char_sub
  !   end interface assignment(=)

  public :: operator( .strequal. )
  interface operator( .strequal. )
    procedure :: is_char_equal_to_char_case_sensitive_fn
  end interface operator( .strequal. )

  public :: operator( .strapprox. )
  interface operator( .strapprox. )
    procedure :: is_char_equal_to_char_case_insensitive_fn
  end interface operator( .strapprox. )

  public :: operator( .contains. )
  interface operator( .contains. )
    procedure :: is_string2_present_in_string1_case_sensitive_fn
  end interface operator( .contains. )

  public :: operator( .containssimilar. )
  interface operator( .containssimilar. )
    procedure :: is_string2_present_in_string1_case_insensitive_fn
  end interface operator( .containssimilar. )

  public :: asCharacter
  interface asCharacter
    procedure :: short_to_char_fn
    procedure :: int_to_char_fn
    procedure :: long_long_to_char_fn
    procedure :: float_to_char_fn
    procedure :: double_to_char_fn
    procedure :: logical_to_char_fn
  end interface asCharacter

  public :: chomp
  interface chomp
    procedure :: split_and_return_text_sub
  end interface chomp

  public :: fieldCount
  interface fieldCount
    procedure :: count_number_of_fields_fn
  end interface fieldCount

  public :: clean
  interface clean
    procedure :: remove_multiple_characters_fn
  end interface clean

  public :: squote
  interface squote
    procedure :: squote_char_fn
  end interface squote

  public :: dquote
  interface dquote
    procedure :: dquote_char_fn
  end interface dquote

  public :: replace
  interface replace
    procedure :: replace_character_sub
  end interface replace

  public :: asUppercase
  interface asUppercase
    procedure :: char_to_uppercase_fn
  end interface asUppercase

  public :: asLowercase
  interface asLowercase
    procedure :: char_to_lowercase_fn
  end interface asLowercase

  public :: toUppercase
  interface toUppercase
    procedure :: char_to_uppercase_sub
  end interface toUppercase

  public :: toLowercase
  interface toLowercase
    procedure :: char_to_lowercase_sub
  end interface toLowercase

  public :: right
  interface right
    procedure :: return_right_part_of_string_fn
  end interface right

  public :: left
  interface left
    procedure :: return_left_part_of_string_fn
  end interface left

contains

  function return_left_part_of_string_fn( string, indx, substring )   result( left_part )

    character (len=*), intent(in)              :: string
    integer (kind=c_int), intent(in), optional :: indx
    character (len=*), intent(in), optional    :: substring
    character (len=:), allocatable             :: left_part

    ! [ LOCALS ]
    integer (kind=c_int) :: position

    if ( present( indx ) ) then

      if ( ( indx > 0 ) .and. ( indx < len_trim( string ) ) ) then

        left_part = string( 1:indx )

      else

        left_part = "<NA>"

      endif

    elseif ( present( substring ) ) then

      position = index( string, substring )

      if ( position > 0 ) then

        left_part = string( 1:(position-1) )

      else

        left_part = "<NA>"

      endif

    else

      left_part = "<NA>"

    endif


  end function return_left_part_of_string_fn

  !--------------------------------------------------------------------------------------------------

  function return_right_part_of_string_fn( string, indx, substring )   result( right_part )

    character (len=*), intent(in)              :: string
    integer (kind=c_int), intent(in), optional :: indx
    character (len=*), intent(in), optional    :: substring
    character (len=:), allocatable             :: right_part

    ! [ LOCALS ]
    integer (kind=c_int) :: position

    if ( present( indx ) ) then

      if ( ( indx > 0 ) .and. ( indx < len_trim( string ) ) ) then

        right_part = string( (indx+1):len_trim(string) )

      else

        right_part = "<NA>"

      endif

    elseif ( present( substring ) ) then

      position = index( string, substring, back=TRUE )

      if ( position > 0 ) then

        right_part = string( (position+1):len_trim(string) )

      else

        right_part = "<NA>"

      endif

    else

      right_part = "<NA>"

    endif


  end function return_right_part_of_string_fn

  !--------------------------------------------------------------------------------------------------

  function is_string2_present_in_string1_case_insensitive_fn(text1, text2)   result(lBool)

    character (len=*), intent(in)      :: text1
    character (len=*), intent(in)      :: text2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=len_trim(text1))  :: temp_text1
    character (len=len_trim(text2))  :: temp_text2

    lBool = FALSE

    temp_text1 = asUppercase(text1)
    temp_text2 = asUppercase(text2)

    if ( index(temp_text1, temp_text2) /= 0 ) lBool = TRUE

  end function is_string2_present_in_string1_case_insensitive_fn

  !--------------------------------------------------------------------------------------------------

  function is_string2_present_in_string1_case_sensitive_fn(text1, text2)   result(lBool)

    character (len=*), intent(in)      :: text1
    character (len=*), intent(in)      :: text2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=len_trim(text1))  :: temp_text1
    character (len=len_trim(text2))  :: temp_text2

    lBool = FALSE

    temp_text1 = trim( text1 )
    temp_text2 = trim( text2 )

    if ( index(temp_text1, temp_text2) /= 0 ) lBool = TRUE

  end function is_string2_present_in_string1_case_sensitive_fn

  !--------------------------------------------------------------------------------------------------

  function is_char_equal_to_char_case_sensitive_fn(text1, text2)   result(lBool)

    character (len=*), intent(in)      :: text1
    character (len=*), intent(in)      :: text2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=:), allocatable    :: temp_text1
    character (len=:), allocatable    :: temp_text2

    lBool = FALSE

    temp_text1 = trim( text1 )
    temp_text2 = trim( text2 )

    if (trim(adjustl( temp_text1 ) )  .eq. trim(adjustl( temp_text2) ) ) lBool = TRUE

  end function is_char_equal_to_char_case_sensitive_fn

  !--------------------------------------------------------------------------------------------------

  function is_char_equal_to_char_case_insensitive_fn(text1, text2)   result(lBool)

    character (len=*), intent(in)      :: text1
    character (len=*), intent(in)      :: text2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=:), allocatable    :: temp_text1
    character (len=:), allocatable    :: temp_text2

    lBool = FALSE

    temp_text1 = asUppercase( text1 )
    temp_text2 = asUppercase( text2 )

    if (trim(adjustl( temp_text1 ) )  .eq. trim(adjustl( temp_text2) ) ) lBool = TRUE

  end function is_char_equal_to_char_case_insensitive_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_char_fn(text1, text2)   result(text)

    character (len=*), intent(in)      :: text1
    character (len=*), intent(in)      :: text2
    character (len=:), allocatable     :: text

    text = text1 // text2

  end function concatenate_char_char_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_int_fn(text1, value_i)   result(text)

    character (len=*), intent(in)        :: text1
    integer (kind=c_int), intent(in)     :: value_i
    character (len=:), allocatable       :: text

    text = text1 // asCharacter( value_i )

  end function concatenate_char_int_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_float_fn(text1, value_f)   result(text)

    character (len=*), intent(in)        :: text1
    real (kind=c_float), intent(in)      :: value_f
    character (len=:), allocatable       :: text

    text = text1 // asCharacter( value_f )

  end function concatenate_char_float_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_double_fn(text1, value_d)   result(text)

    character (len=*), intent(in)        :: text1
    real (kind=c_double), intent(in)     :: value_d
    character (len=:), allocatable       :: text

    text = text1 // asCharacter( value_d )

  end function concatenate_char_double_fn

  !--------------------------------------------------------------------------------------------------

  function short_to_char_fn(value_i)    result(text)

    integer (kind=c_short), intent(in)  :: value_i
    character (len=:), allocatable    :: text

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sbuf

    write(sbuf, fmt=*, iostat=iStat)  value_i

    if (iStat==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function short_to_char_fn

  !--------------------------------------------------------------------------------------------------

  function int_to_char_fn(value_i)    result(text)

    integer (kind=c_int), intent(in)  :: value_i
    character (len=:), allocatable    :: text

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sbuf

    write(sbuf, fmt=*, iostat=iStat)  value_i

    if (iStat==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function int_to_char_fn

  !--------------------------------------------------------------------------------------------------

  function long_long_to_char_fn(value_i)    result(text)

    integer (kind=c_long_long), intent(in)  :: value_i
    character (len=:), allocatable          :: text

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sbuf

    write(sbuf, fmt=*, iostat=iStat)  value_i

    if (iStat==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function long_long_to_char_fn

  !--------------------------------------------------------------------------------------------------

  function float_to_char_fn(fValue, fieldWidth, numDigits)    result(text)

    real (kind=c_float), intent(in)             :: fValue
    integer (kind=c_int), intent(in), optional  :: fieldWidth
    integer (kind=c_int), intent(in), optional  :: numDigits
    character (len=:), allocatable              :: text

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: format_str
    character (len=32)   :: sbuf

    if ( present( numDigits) .and. present( fieldWidth ) ) then
      write(format_str, fmt="('(G',i0,'.',i0,')')") fieldWidth, numDigits
    elseif (present(numDigits) ) then
      write(format_str, fmt="('(G0.',i0,')')") numDigits
    elseif (present(fieldWidth) ) then
      write(format_str, fmt="('(G',i0,'.4)')") numDigits
    else
      format_str = "(G0.4)"
    endif

    write(sbuf, fmt=trim(format_str), iostat=iStat)  fValue

    if (iStat==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function float_to_char_fn

  !--------------------------------------------------------------------------------------------------

  function double_to_char_fn(dValue, numDigits)    result(text)

    real (kind=c_double), intent(in)             :: dValue
    integer (kind=c_int), intent(in), optional  :: numDigits
    character (len=:), allocatable              :: text

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=:), allocatable :: format_str
    character (len=32)   :: sbuf

    if (present(numDigits) ) then
      write(format_str, fmt="('(G0.',i0,')')") numDigits
    else
      format_str = "(G0.12)"
    endif

    write(sbuf, fmt=format_str, iostat=iStat)  dValue

    if (iStat==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function double_to_char_fn

  !--------------------------------------------------------------------------------------------------

  function logical_to_char_fn(lValue)    result(text)

    logical (kind=c_bool), intent(in)    :: lValue
    character (len=:), allocatable       :: text

    if (lValue) then
      text = "TRUE"
    else
      text = "FALSE"
    endif

  end function logical_to_char_fn

  !--------------------------------------------------------------------------------------------------

  function squote_char_fn(text1)    result(text)

    character (len=*), intent(in)         :: text1
    character (len=:), allocatable        :: text

    text = "'"//trim(text1)//"'"

  end function squote_char_fn

  !--------------------------------------------------------------------------------------------------

  function dquote_char_fn(text1)    result(text)

    character (len=*), intent(in)         :: text1
    character (len=:), allocatable        :: text

    text = '"'//trim(text1)//'"'

  end function dquote_char_fn

  !--------------------------------------------------------------------------------------------------

  function char_to_uppercase_fn ( s )                    result(text)

    ! ARGUMENTS
    character (len=*), intent(in) :: s
    character(len=len(s))         :: text

    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index

    ! CONSTANTS
    integer (kind=c_int), parameter :: LOWER_TO_UPPER = -32
    integer (kind=c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (kind=c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    text = s

    do i=1,len_trim(text)
      if ( ichar(text(i:i) ) >= ASCII_SMALL_A .and. ichar(text(i:i)) <= ASCII_SMALL_Z ) then
        text(i:i) = char( ichar( text(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end function char_to_uppercase_fn

  !--------------------------------------------------------------------------

  function char_to_lowercase_fn ( s )                  result(text)

    ! ARGUMENTS
    character (len=*), intent(in) :: s
    character(len=len(s)) :: text

    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (kind=c_int), parameter :: UPPER_TO_LOWER = 32
    integer (kind=c_int), parameter :: ASCII_A = ichar("A")
    integer (kind=c_int), parameter :: ASCII_Z = ichar("Z")

    text = s

    do i=1,len_trim(text)
      if ( ichar(text(i:i) ) >= ASCII_A .and. ichar(text(i:i)) <= ASCII_Z ) then
        text(i:i) = char( ichar( text(i:i) ) + UPPER_TO_LOWER )
      end if
    end do

  end function char_to_lowercase_fn


  subroutine char_to_uppercase_sub ( s )

    ! ARGUMENTS
    character (len=*), intent(inout) :: s
    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (kind=c_int), parameter :: LOWER_TO_UPPER = -32
    integer (kind=c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (kind=c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ASCII_SMALL_A .and. ichar(s(i:i)) <= ASCII_SMALL_Z ) then
        s(i:i) = char( ichar( s(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end subroutine char_to_uppercase_sub


  subroutine char_to_lowercase_sub ( s )

    ! ARGUMENTS
    character (len=*), intent(inout) :: s
    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (kind=c_int), parameter :: UPPER_TO_LOWER = 32
    integer (kind=c_int), parameter :: ASCII_A = ichar("A")
    integer (kind=c_int), parameter :: ASCII_Z = ichar("Z")

    ! UPPER_TO_LOWER = ichar( "a" ) - ichar( "A" )

    do i=1,len_trim( s )
      if ( ichar(s(i:i) ) >= ASCII_A .and. ichar(s(i:i)) <= ASCII_Z ) then
        s(i:i) = char( ichar( s(i:i) ) + UPPER_TO_LOWER )
      end if
    end do

  end subroutine char_to_lowercase_sub

  !--------------------------------------------------------------------------------------------------

  !> Strip repeated characters from string.
  !!
  !! Remove repeated characters from a string. By default the function looks for repeated spaces and eliminates them.
  !! @param[in] textIn
  function remove_repeats(text1, sChar)            result(text)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: text1
    character (len=*), intent(in), optional    :: sChar
    character (len=:), allocatable :: text

    ! LOCALS
    character (len=256)            :: sbuf
    integer (kind=c_int)           :: iR                 ! Index in sRecord
    integer (kind=c_int)           :: indx1, indx2
    character (len=1)              :: sChar_
    logical (kind=c_bool)          :: lPreviouslyFound

    ! eliminate any leading spaces
    text1 = adjustl(text1)
    sbuf = ""
    indx2 = 0
    lPreviouslyFound = FALSE

    if (present(sChar) ) then
      sChar_ = sChar
    else
      sChar_ = " "
    endif

    do indx1 = 1,len_trim(text1)

      iR = scan(text1(indx1:indx1), sChar_)

      if(iR==0) then
        ! sChar_ was not found
        indx2 = indx2 + 1
        sbuf(indx2:indx2) = text1(indx1:indx1)
        lPreviouslyFound = FALSE

      elseif( lPreviouslyFound ) then
        ! sChar_ was found, and was also found in the position preceding this one

        ! No OP

      else
        ! sChar_ was found, but was *not* found in the preceding position

        indx2 = indx2 + 1
        sbuf(indx2:indx2) = text1(indx1:indx1)
        lPreviouslyFound = TRUE

      end if

    enddo

    text = trim(sbuf)

  end function remove_repeats

  !--------------------------------------------------------------------------------------------------

  function count_number_of_fields_fn( text, delimiter_chr )     result( count_i )

    character (len=*), intent(in)               :: text
    character (len=*), intent(in), optional     :: delimiter_chr
    integer (kind=c_int)                        :: count_i

    ! [ LOCALS ]
    character (len=len(text))      :: text1
    character (len=len(text))      :: text2
    character (len=:), allocatable  :: delimiter_chr_

    if ( present(delimiter_chr) ) then
      delimiter_chr_ = delimiter_chr_
    else
      delimiter_chr_ = WHITESPACE
    endif

    count_i = 0

    text1 = text

    do
      call chomp(text1=text1, text2=text2, delimiter_chr=delimiter_chr_ )

      if ( len_trim( text2 ) == 0 )  exit

      count_i = count_i + 1

    enddo

  end function count_number_of_fields_fn

  !--------------------------------------------------------------------------------------------------

  subroutine split_and_return_text_sub(text1, text2, delimiter_chr)

    character (len=*), intent(inout)                     :: text1
    character (len=*), intent(out)                       :: text2
    character (len=*), intent(in), optional              :: delimiter_chr

    ! [ LOCALS ]
    character (len=:), allocatable :: delimiter_chr_
    integer (kind=c_int) :: iIndex

    if ( present(delimiter_chr) ) then

      select case (delimiter_chr)
      case ("WHITESPACE")
        delimiter_chr_ = WHITESPACE
      case ("TAB", "TABS")
        delimiter_chr_ = TAB
      case ("COMMA", "CSV")
        delimiter_chr_ = ","
      case default
        delimiter_chr_ = delimiter_chr
      end select

    else

      delimiter_chr_ = WHITESPACE

    endif

    text1 = adjustl(text1)

    iIndex = scan( string = text1, set = delimiter_chr_ )

    if (iIndex == 0) then
      ! no delimiters found; return string as was supplied originally
      text2 = text1
      text1 = ""
    else
      ! delimiters were found; split and return the chunks of text
      text2 = trim( text1(1:iIndex-1) )
      text1 = trim( adjustl( text1(iIndex + 1:) ) )
    endif

  end subroutine split_and_return_text_sub

  !--------------------------------------------------------------------------------------------------

  subroutine replace_character_sub(text1, find_chr, replace_chr)

    character (len=*), intent(inout)    :: text1
    character (len=1), intent(in)       :: find_chr
    character (len=1), intent(in)       :: replace_chr

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if ( len(text1) > 0 ) then

      do iIndex = 1, len_trim(text1)

        if ( text1(iIndex:iIndex) .eq. find_chr)    text1(iIndex:iIndex) = replace_chr

      enddo

    endif

  end subroutine replace_character_sub

  !--------------------------------------------------------------------------------------------------

  function remove_multiple_characters_fn(text, delimiter_chr)                result(text1)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: text
    character (len=*), intent(in), optional    :: delimiter_chr
    character (len=:), allocatable             :: text1

    ! LOCALS
    character ( len=len_trim(text) ) :: sTemp
    integer (kind=c_int) :: iR                 ! Index in sRecord
    integer (kind=c_int) :: i, j

    ! eliminate any leading spaces
    text = adjustl(text)
    sTemp = ""
    j = 0

    do i = 1,len_trim(text)

      if(present(delimiter_chr)) then
        iR = scan(text(i:i), delimiter_chr)
      else
        iR = scan(text(i:i),":/;,")
      endif

      if(iR==0) then
        j = j + 1
        sTemp(j:j) = text(i:i)
      end if

    enddo

    text1 = trim(sTemp)

  end function remove_multiple_characters_fn


end module strings
