program Driver

  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type

  !Load test suites.
  use Basic_Functionality, only: collect_Basic_Functionality

  implicit none

  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)

  stat = 0

  testsuites = [ &
               new_testsuite("Basic Functionality", collect_Basic_Functionality)]

  do is = 1, size(testsuites)
    write (error_unit, "(A)") "Testing: "//trim(adjustl(testsuites(is)%name))
    call run_testsuite(testsuites(is)%collect, error_unit, stat)
  end do

  if (stat > 0) then
    write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
  end if

end program Driver
