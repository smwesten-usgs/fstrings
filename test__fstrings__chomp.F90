module test__fstrings__chomp

  use fruit
  use fstring
  use iso_c_binding
  implicit none

contains

  subroutine test_chomp
    ! test 'chomp' subroutine
    character (len=256) :: mystring
    character (len=256) :: mysubstring

    mystring = "one, two, three, four"

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("one", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("two", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("three", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("four", mysubstring)

    mystring = "five, , seven, eight"

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("five", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("seven", mysubstring)

    mystring = "nine,,eleven,"

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("nine", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("eleven", mysubstring)

    call chomp( mystring, mysubstring, ",")
    call assert_equals ("", mysubstring)

    ! specify a clean string containing only a single space between items 
    mystring = "GRID 302 400 230213. 415302. 1000."

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("GRID", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("302", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("400", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("230213.", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("415302.", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("1000.", mysubstring)

    ! specify a messy string containing spaces and tabs between items 
    mystring = "GRID 	302 	 400	 230213.	 	415302.	1000."

    call chomp( mystring, mysubstring, "WHITESPACE", .true._c_bool)
    call assert_equals ("GRID", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE", .true._c_bool)
    call assert_equals ("302", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE", .true._c_bool)
    call assert_equals ("400", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE", .true._c_bool)
    call assert_equals ("230213.", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE", .true._c_bool)
    call assert_equals ("415302.", mysubstring)

    call chomp( mystring, mysubstring, "WHITESPACE")
    call assert_equals ("1000.", mysubstring)

  end subroutine test_chomp

end module test__fstrings__chomp
