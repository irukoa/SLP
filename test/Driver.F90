program Driver

  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type

  !Load test suites.
  use Basic_Functionality, only: collect_Basic_Functionality
  use Utility_Functionality, only: collect_Utility_Functionality

  implicit none

  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)

  stat = 0

  testsuites = [ &
               new_testsuite("Basic Functionality", collect_Basic_Functionality), &
               new_testsuite("Utility Functionality", collect_Utility_Functionality) &
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
