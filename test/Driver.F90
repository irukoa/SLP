program Driver

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type

  !Load test suites.
  use Utility_Functionality, only: collect_Utility_Functionality
  use Auxiliary_Functionality, only: collect_Auxiliary_Functionality
  use Linop_Functionality, only: collect_Linop_Functionality
  use Obj_Functionality, only: collect_Obj_Functionality

  implicit none

  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)

  stat = 0

  testsuites = [ &
               new_testsuite("Utility Functionality", collect_Utility_Functionality), &
               new_testsuite("Auxiliary Functionality", collect_Auxiliary_Functionality), &
               new_testsuite("Operator Functionality", collect_Linop_Functionality), &
               new_testsuite("SLP Object Functionality", collect_Obj_Functionality) &
               ]

  call random_seed()
  do is = 1, size(testsuites)
    write (stderr, "(A)") "Testing: "//trim(adjustl(testsuites(is)%name))
    call run_testsuite(testsuites(is)%collect, stderr, stat)
  end do

  if (stat > 0) then
    write (stderr, '(I0, A)') stat, " test(s) failed!"
    error stop
  end if

end program Driver
