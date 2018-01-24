program fstrings_test_01

  use fstrings
  use iso_c_binding
  implicit none

  type (FSTRINGS_T)  :: mystring

  mystring = "Baracuda"
  call mystring%append("Coelacanth")
  call mystring%append("Sturgeon")
  call mystring%append("Anchovy")
  call mystring%append("Bluegill")
  call mystring%append("Pollack")
  call mystring%append("Salmon")

  call mystring%print_all()
  print *, mystring%count()

  print *, mystring%upper()

  mystring = "Muskellunge"

  call mystring%print_all()
  print *, mystring%count()

end program fstrings_test_01
