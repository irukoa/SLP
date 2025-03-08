module Error_Stop_Checks

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: new_unittest, unittest_type, error_type

  implicit none
  private

  public :: collect_Error_Stop_Checks

contains

  subroutine collect_Error_Stop_Checks(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                new_unittest("error check: Non-Square real (dp) matrix.", test_sq_matrix_sym_dp, should_fail=.true.) &
                ]

  end subroutine collect_Error_Stop_Checks

  subroutine test_sq_matrix_sym_dp(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: istat
    call execute_command_line("fpm test test_sq_matrix_sym_dp_fail 1> /dev/null 2> /dev/null", exitstat=istat)
    if (istat /= 0) allocate (error)
  end subroutine test_sq_matrix_sym_dp

end module Error_Stop_Checks
