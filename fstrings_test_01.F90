program fstrings_test_01

  use fstrings
  use iso_c_binding
  implicit none

  type (FSTRINGS_T)  :: mystring
  type (FSTRINGS_T)  :: myreals
  character (len=:), allocatable  :: sbuf

  integer (c_int)    :: i
  integer (c_int)    :: start_pos
  integer (c_int)    :: end_pos
  integer (c_int)    :: str_len

  mystring = "Baracuda"
  call mystring%append("Coelacanth")
  call mystring%append("Sturgeon")
  call mystring%append("Anchovy")
  call mystring%append("Bluegill")
  call mystring%append(["Crappie"//c_null_char//"Gar"])
  call mystring%append(["Alewife    ","Sea Lamprey"])
  call mystring%append(["Guppy      ",                                &
                        "Black Tetra",                                &
                        "Goldfish   ",                                &
                        "Neon Tetra "])
  call mystring%append("Pollack")
  call mystring%append("Cod")
  call mystring%append("Salmon")
  call mystring%append("Sea Bass")
  call mystring%append("Angelfish")

  print *, "__unsorted__"

  call mystring%print_all()

  call mystring%sort()

  print *, "__sorted__"
  call mystring%print_all()

  print *, "get ind. values..."
  print *, "1) ", mystring%get(1)
  print *, "2) ", mystring%get(2)
  print *, "3) ", mystring%get(3)
  print *, "4) ", mystring%get(4)
  print *, "5) ", mystring%get(5)

    ! new assignment to existing 'mystring' should clear out old list
  mystring = "Muskellunge"
  print *, mystring%count()
  print *, "1) ", mystring%get(1)
  print *, "2) ", mystring%get(2)

  myreals = "02134"
  call myreals%append("3.1415")
  call myreals%append("0556")
  call myreals%append("90210")
  call myreals%append("48109")
  call myreals%append("2.718")
  call myreals%append("53572")
  call myreals%append("1")
  call myreals%append("Yes")

  print *, "__unsorted__"
  call myreals%print_all()

  call myreals%sort()

  print *, "__sorted__"
  call myreals%print_all()

  print *, mystring%get_integer()
  print *, mystring%get_float()
  print *, mystring%get_logical()

  sbuf = myreals

  print *, sbuf

end program fstrings_test_01
