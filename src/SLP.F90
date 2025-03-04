module SLP

  use SLP_kinds, only: wp => dp

  implicit none
  private

  public :: tt

contains

  subroutine tt()
    print *, "testing"
  end subroutine tt

end module SLP
