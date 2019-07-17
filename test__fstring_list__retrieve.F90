module test_fstring_list__retrieve

  use fruit
  use fstring
  use fstring_list
  use iso_c_binding
  implicit none

contains

  subroutine test_list_retrieve_uninitialized
    ! test list 'retrieve' subroutines on uninitialized list
    character (len=256)          :: mystring
    integer (c_int), allocatable :: int_values(:)
    real (c_float), allocatable :: float_values(:)
    real (c_double), allocatable :: double_values(:)

    type (FSTRING_LIST_T)  :: mylist

    int_values = mylist%get_integer()
    call assert_equals (NA_INT, int_values(1))

    float_values = mylist%get_float()
    call assert_equals (NA_FLOAT, float_values(1))

    double_values = mylist%get_double()
    call assert_equals (NA_DOUBLE, double_values(1))

  end subroutine test_list_retrieve_uninitialized

  subroutine test_list_retrieve_as_integer
    ! test list 'retrieve' subroutines on mixed character/integer entry
    character (len=256)          :: mystring
    integer (c_int), allocatable :: int_values(:)

    type (FSTRING_LIST_T)  :: mylist

    mylist = create_list("CN_1, CN_2, CN_3, CN_4")

    int_values = mylist%get_integer()
    call assert_equals (1, int_values(1))
    call assert_equals (2, int_values(2))

  end subroutine test_list_retrieve_as_integer

end module test_fstring_list__retrieve
