module Basic_Functionality
  use testdrive, only: new_unittest, unittest_type, error_type, check
  implicit none
  private

  public :: collect_Basic_Functionality

contains

  subroutine collect_Basic_Functionality(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                new_unittest("valid", test_valid), &
                new_unittest("invalid", test_invalid, should_fail=.true.) &
                ]

  end subroutine collect_Basic_Functionality

  subroutine test_valid(error)
    use SLP, only: tt
    type(error_type), allocatable, intent(out) :: error
    call tt()
  end subroutine test_valid

  subroutine test_invalid(error)
    type(error_type), allocatable, intent(out) :: error
    ! ...
    allocate (error)
  end subroutine test_invalid

end module Basic_Functionality
