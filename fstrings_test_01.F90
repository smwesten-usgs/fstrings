program fstrings_test_01

  use fstrings
  use iso_c_binding
  implicit none

  character (len=:), allocatable  :: str(:)

  type (FSTRINGS_T)  :: mystring
  integer (c_int)    :: counter

  allocate(character(len=10)::str(3))

  str(1) = "Baracuda"
  str(2) = "Coelacanth"
  str(3) = "Sturgeon"

  print *, str

  mystring = "Baracuda"
  call mystring%append("Coelacanth")
  call mystring%append("Sturgeon")
  call mystring%append("Anchovy")
  call mystring%append("Bluegill")
  call mystring%append("Pollack")
  call mystring%append("Salmon")

  call mystring%print_all()
  print *, mystring%count()

  do counter=1,mystring%count()

    print *, counter, ": ", mystring%get(counter)

  enddo  


  print *, mystring%upper()

  mystring = "Muskellunge"

  call mystring%print_all()
  print *, mystring%count()

end program fstrings_test_01
