module Linop_Functionality

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: new_unittest, unittest_type, error_type

  use SLP_kinds, only: sp, dp
  use SLP_defs, only: cmplx_1_sp, cmplx_i_sp, cmplx_1_dp, cmplx_i_dp

  implicit none
  private

  real(sp), parameter :: tol_sp = 1.0E1_sp*epsilon(1.0_sp)
  real(sp), parameter :: tol_dp = 1.0E1_dp*epsilon(1.0_dp)

  public :: collect_Linop_Functionality

contains

  subroutine collect_Linop_Functionality(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                new_unittest("get representation of linear operators: fixed real (sp) array.", test_fixed_re_linop_sp), &
                new_unittest("get representation of linear operators: fixed real (dp) array.", test_fixed_re_linop_dp), &
                new_unittest("get representation of linear operators: fixed complex (sp) array.", test_fixed_c_linop_sp), &
                new_unittest("get representation of linear operators: fixed complex (dp) array.", test_fixed_c_linop_dp), &
                new_unittest("get representation of linear operators: random real (sp) arrays.", test_rnd_re_linop_sp), &
                new_unittest("get representation of linear operators: random real (dp) arrays.", test_rnd_re_linop_dp), &
                new_unittest("get representation of linear operators: random complex (sp) arrays.", test_rnd_c_linop_sp), &
                new_unittest("get representation of linear operators: random complex (dp) arrays.", test_rnd_c_linop_dp) &
                ]

  end subroutine collect_Linop_Functionality

  subroutine test_fixed_re_linop_sp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: N = 10
    real(sp) :: L(N, N), p(N), pp(N), q(N)
    real(sp) :: rel_err, abs_err, spacing
    integer :: i

    do i = 1, N
      q(i) = real(i - 1, sp)/real(N - 1, sp)
      p(i) = (real(i - 1, sp)/real(N - 1, sp))**3
      pp(i) = 3*(real(i - 1, sp)/real(N - 1, sp))**2
    enddo
    spacing = 1.0_sp/real(N - 1, sp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

  end subroutine test_fixed_re_linop_sp

  subroutine test_fixed_re_linop_dp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: N = 10
    real(dp) :: L(N, N), p(N), pp(N), q(N)
    real(dp) :: rel_err, abs_err, spacing
    integer :: i

    do i = 1, N
      q(i) = real(i - 1, dp)/real(N - 1, dp)
      p(i) = (real(i - 1, dp)/real(N - 1, dp))**3
      pp(i) = 3*(real(i - 1, dp)/real(N - 1, dp))**2
    enddo
    spacing = 1.0_dp/real(N - 1, dp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

  end subroutine test_fixed_re_linop_dp

  subroutine test_fixed_c_linop_sp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: N = 10
    complex(sp) :: L(N, N), p(N), pp(N), q(N)
    real(sp) :: rel_err, abs_err, spacing
    integer :: i

    do i = 1, N
      q(i) = cmplx_1_sp*real(i - 1, sp)/real(N - 1, sp)
      p(i) = cmplx_1_sp*(real(i - 1, sp)/real(N - 1, sp))**3
      pp(i) = 3*cmplx_1_sp*(real(i - 1, sp)/real(N - 1, sp))**2
    enddo
    spacing = 1.0_sp/real(N - 1, sp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_sp*p(2)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_sp) then
      allocate (error)
      return
    endif

  end subroutine test_fixed_c_linop_sp

  subroutine test_fixed_c_linop_dp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: N = 10
    complex(dp) :: L(N, N), p(N), pp(N), q(N)
    real(dp) :: rel_err, abs_err, spacing
    integer :: i

    do i = 1, N
      q(i) = cmplx_1_dp*real(i - 1, dp)/real(N - 1, dp)
      p(i) = cmplx_1_dp*(real(i - 1, dp)/real(N - 1, dp))**3
      pp(i) = 3*cmplx_1_dp*(real(i - 1, dp)/real(N - 1, dp))**2
    enddo
    spacing = 1.0_dp/real(N - 1, dp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check some values.
    if (abs(L(2, 2) - (q(2) - 2.0_dp*p(2)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(5, 6) - (p(5)/(spacing**2) + pp(5)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N - 1, N - 2) - (p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(5, 6) - L(6, 5)) > tol_dp) then
      allocate (error)
      return
    endif

  end subroutine test_fixed_c_linop_dp

  subroutine test_rnd_re_linop_sp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer :: N, Nmx
    real(sp) :: rndvl
    real(sp), allocatable :: L(:, :), p(:), pp(:), q(:)
    real(sp) :: rel_err, abs_err, spacing
    integer :: i
    character(len=120) :: aux

    Nmx = 10000
    call random_number(rndvl)
    N = nint(6.0_sp + (real(Nmx - 1, sp) - 6.0_sp)*rndvl)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)") "Array size = "//trim(adjustl(aux))//"."
    allocate (L(N, N), p(N), pp(N), q(N))

    call random_number(p)
    call random_number(pp)
    call random_number(q)
    spacing = 1.0_sp/real(N - 1, sp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Dirichlet boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_sp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Neumann boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_sp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - 1.0_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Mixed boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_sp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Periodic boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_sp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Free boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_sp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Singular problem: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_sp, mixing_b=1.0_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_sp) then
      allocate (error)
      return
    endif

    deallocate (L, p, pp, q)

  end subroutine test_rnd_re_linop_sp

  subroutine test_rnd_re_linop_dp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer :: N, Nmx
    real(dp) :: rndvl
    real(dp), allocatable :: L(:, :), p(:), pp(:), q(:)
    real(dp) :: rel_err, abs_err, spacing
    integer :: i
    character(len=120) :: aux

    Nmx = 10000
    call random_number(rndvl)
    N = nint(6.0_dp + (real(Nmx - 1, dp) - 6.0_dp)*rndvl)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)") "Array size = "//trim(adjustl(aux))//"."
    allocate (L(N, N), p(N), pp(N), q(N))

    call random_number(p)
    call random_number(pp)
    call random_number(q)
    spacing = 1.0_dp/real(N - 1, dp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Dirichlet boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_dp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Neumann boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_dp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - 1.0_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Mixed boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_dp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Periodic boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_dp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Free boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_dp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Singular problem: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=1.0_dp, mixing_b=1.0_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - L(2, 3)) > tol_dp) then
      allocate (error)
      return
    endif

    deallocate (L, p, pp, q)

  end subroutine test_rnd_re_linop_dp

  subroutine test_rnd_c_linop_sp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer :: N, Nmx
    real(sp) :: rndvl
    complex(sp), allocatable :: L(:, :), p(:), pp(:), q(:)
    real(sp) :: rel_err, abs_err, spacing
    integer :: i
    character(len=120) :: aux

    Nmx = 10000
    call random_number(rndvl)
    N = nint(6.0_sp + (real(Nmx - 1, sp) - 6.0_sp)*rndvl)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)") "Array size = "//trim(adjustl(aux))//"."
    allocate (L(N, N), p(N), pp(N), q(N))

    do i = 1, N
      call random_number(rndvl)
      p(i) = cmplx_1_sp*rndvl
      call random_number(rndvl)
      p(i) = p(i) + cmplx_i_sp*rndvl
      call random_number(rndvl)
      pp(i) = cmplx_1_sp*rndvl
      call random_number(rndvl)
      pp(i) = pp(i) + cmplx_i_sp*rndvl
      call random_number(rndvl)
      q(i) = cmplx_1_sp*rndvl
      call random_number(rndvl)
      q(i) = q(i) + cmplx_i_sp*rndvl
    enddo
    spacing = 1.0_sp/real(N - 1, sp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Dirichlet boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_sp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Neumann boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_sp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - cmplx_1_sp) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Mixed boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_sp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Periodic boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_sp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Free boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_sp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_sp*p(1)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_sp*p(N)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_sp + (real(N - 3, sp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Singular problem: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_sp*p(i)/(spacing**2))) > tol_sp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_sp, b=1.0_sp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_sp, mixing_b=cmplx_1_sp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_sp) then
      allocate (error)
      return
    endif

    deallocate (L, p, pp, q)

  end subroutine test_rnd_c_linop_sp

  subroutine test_rnd_c_linop_dp(error)
    use SLP_linop, only: linop
    type(error_type), allocatable, intent(out) :: error
    integer :: N, Nmx
    real(dp) :: rndvl
    complex(dp), allocatable :: L(:, :), p(:), pp(:), q(:)
    real(dp) :: rel_err, abs_err, spacing
    integer :: i
    character(len=120) :: aux

    Nmx = 10000
    call random_number(rndvl)
    N = nint(6.0_dp + (real(Nmx - 1, dp) - 6.0_dp)*rndvl)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)") "Array size = "//trim(adjustl(aux))//"."
    allocate (L(N, N), p(N), pp(N), q(N))

    do i = 1, N
      call random_number(rndvl)
      p(i) = cmplx_1_dp*rndvl
      call random_number(rndvl)
      p(i) = p(i) + cmplx_i_dp*rndvl
      call random_number(rndvl)
      pp(i) = cmplx_1_dp*rndvl
      call random_number(rndvl)
      pp(i) = pp(i) + cmplx_i_dp*rndvl
      call random_number(rndvl)
      q(i) = cmplx_1_dp*rndvl
      call random_number(rndvl)
      q(i) = q(i) + cmplx_i_dp*rndvl
    enddo
    spacing = 1.0_dp/real(N - 1, dp)

    !Dirichlet boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Dirichlet boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=1, bound_b_id=1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_dp) then
      allocate (error)
      return
    endif

    !Neumann boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Neumann boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=2, bound_b_id=2, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_dp) then
      allocate (error)
      return
    endif

    !Mixed boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, 2) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) + L(N - 1, N) - cmplx_1_dp) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Mixed boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=3, bound_b_id=3, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_dp) then
      allocate (error)
      return
    endif

    !Periodic boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) + L(1, N)) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(1, N) + L(2, N) + L(N - 1, N) + L(N, N)) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Periodic boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=4, bound_b_id=4, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_dp) then
      allocate (error)
      return
    endif

    !Free boundary conditions.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Free boundary conditions: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=0, bound_b_id=0, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_dp) then
      allocate (error)
      return
    endif

    !Singular problem.
    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.false., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check boundary conditions.
    if (abs(L(1, 1) - (q(1) - 2.0_dp*p(1)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    if (abs(L(N, N) - (q(N) - 2.0_dp*p(N)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    !Check a value.
    call random_number(rndvl)
    i = nint(3.0_dp + (real(N - 3, dp))*rndvl)
    write (aux, fmt="(I0)") i
    write (stderr, fmt="(A)") "Singular problem: check entry #"//trim(adjustl(aux))//"."

    if (abs(L(i, i) - (q(i) - 2.0_dp*p(i)/(spacing**2))) > tol_dp) then
      allocate (error)
      return
    endif

    call linop(a=0.0_dp, b=1.0_dp, N=N, &
               p=p, pp=pp, q=q, &
               bound_a_id=-1, bound_b_id=-1, &
               mixing_a=cmplx_1_dp, mixing_b=cmplx_1_dp, &
               enforce_sym=.true., &
               u=L, &
               rel_err_stm=rel_err, abs_err_stm=abs_err)

    !Check symmetrization.
    if (abs(L(3, 2) - conjg(L(2, 3))) > tol_dp) then
      allocate (error)
      return
    endif

    deallocate (L, p, pp, q)

  end subroutine test_rnd_c_linop_dp

end module Linop_Functionality
