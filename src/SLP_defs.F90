module SLP_defs

  use SLP_kinds, only: sp, dp

  implicit none
  private

  complex(sp), parameter, public :: cmplx_0_sp = cmplx(0.0_sp, 0.0_sp, sp)
  complex(sp), parameter, public :: cmplx_1_sp = cmplx(1.0_sp, 0.0_sp, sp)
  complex(sp), parameter, public :: cmplx_i_sp = cmplx(0.0_sp, 1.0_sp, sp)

  complex(dp), parameter, public :: cmplx_0_dp = cmplx(0.0_dp, 0.0_dp, dp)
  complex(dp), parameter, public :: cmplx_1_dp = cmplx(1.0_dp, 0.0_dp, dp)
  complex(dp), parameter, public :: cmplx_i_dp = cmplx(0.0_dp, 1.0_dp, dp)

  real(sp), parameter, public :: pi_sp = acos(-1.0_sp)
  real(dp), parameter, public :: pi_dp = acos(-1.0_dp)

end module SLP_defs
