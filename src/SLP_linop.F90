module SLP_linop

  use SLP_kinds, only: sp, dp
  use SLP_defs, only: cmplx_0_sp, cmplx_1_sp, cmplx_0_dp, cmplx_1_dp

  implicit none
  private

  real(sp), parameter :: tol_sp = 1.0E2_sp*epsilon(1.0_sp)
  real(dp), parameter :: tol_dp = 1.0E2_dp*epsilon(1.0_dp)

  interface linop
    module procedure :: linop_re_rep_sp
    module procedure :: linop_re_rep_dp
    module procedure :: linop_c_rep_sp
    module procedure :: linop_c_rep_dp
  end interface linop

  public :: linop

contains

  subroutine linop_re_rep_sp(a, b, N, &
                             p, pp, q, &
                             bound_a_id, bound_b_id, &
                             mixing_a, mixing_b, &
                             enforce_sym, &
                             u, &
                             rel_err_stm, abs_err_stm)

    !Defines the matrix corresponding to the operator
    !L = {p(x), q(x), w(x)} in the
    !x representation.

    !Assumes N > 5.
    !Assumes recognizable and coherent Id-s for boundaries.

    real(sp), intent(in) :: a, b
    integer, intent(in) :: N
    real(sp), intent(in) :: p(N), pp(N), q(N)
    integer, intent(in) :: bound_a_id, bound_b_id
    real(sp), intent(in) :: mixing_a, mixing_b
    logical, intent(in) :: enforce_sym

    real(sp), intent(out) :: u(N, N)
    real(sp), intent(out) :: rel_err_stm, abs_err_stm

    integer :: i
    real(sp) :: spacing

    spacing = abs(b - a)/real(N - 1, sp)
    u = 0.0_sp
    rel_err_stm = 0.0_sp; abs_err_stm = 0.0_sp

    select case (bound_a_id)
    case (1) !Dirichlet.
      u(1, 1) = 1.0_sp
    case (2) !Neumann.
      u(1, 1) = 1.0_sp
      u(1, 2) = -1.0_sp
    case (3) !Mixed.
      u(1, 1) = 1.0_sp + (mixing_a/spacing)
      u(1, 2) = -mixing_a/spacing
    case (4) !Periodic.
      !Continuity.
      u(1, 1) = 1.0_sp
      u(1, N) = -1.0_sp
    case (0, -1) !Free or singular.
      u(1, 1) = q(1) - 2.0_sp*p(1)/(spacing**2)
      u(1, 2) = p(1)/(spacing**2) + pp(1)/(2*spacing)
      u(2, 1) = p(2)/(spacing**2) - pp(2)/(2*spacing)
    end select

    select case (bound_b_id)
    case (1) !Dirichlet.
      u(N, N) = 1.0_sp
    case (2) !Neumann.
      u(N, N) = 1.0_sp
      u(N - 1, N) = -1.0_sp
    case (3) !Mixed.
      u(N, N) = 1.0_sp + (mixing_b/spacing)
      u(N - 1, N) = -mixing_b/spacing
    case (4) !Periodic.
      !Continuity of the derivative.
      u(1, N) = -1.0_sp
      u(2, N) = 1.0_sp
      u(N - 1, N) = 1.0_sp
      u(N, N) = -1.0_sp
    case (0, -1) !Free or singular.
      u(N - 1, N) = p(N - 1)/(spacing**2) + pp(N - 1)/(2*spacing)
      u(N, N - 1) = p(N)/(spacing**2) - pp(N)/(2*spacing)
      u(N, N) = q(N) - 2.0_sp*p(N)/(spacing**2)
    end select

    u(2, 2) = q(2) - 2.0_sp*p(2)/(spacing**2)
    u(2, 3) = p(2)/(spacing**2) + pp(2)/(2*spacing)
    if (abs(pp(2)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - pp(1)/pp(2))
    abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(1) - pp(2))
    do concurrent(i=3:N - 2)
      u(i, i - 1) = p(i)/(spacing**2) - pp(i)/(2*spacing)
      u(i, i) = q(i) - 2.0_sp*p(i)/(spacing**2)
      u(i, i + 1) = p(i)/(spacing**2) + pp(i)/(2*spacing)
      if (abs(pp(N)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - pp(i - 1)/pp(i))
      abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(i - 1) - pp(i))
    enddo
    u(N - 1, N - 2) = p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing)
    u(N - 1, N - 1) = q(N - 1) - 2.0_sp*p(N - 1)/(spacing**2)
    if (abs(pp(N - 1)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - pp(N - 2)/pp(N - 1))
    abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(N - 2) - pp(N - 1))
    if (abs(pp(N)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - pp(N - 1)/pp(N))
    abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(N - 1) - pp(N))

    rel_err_stm = rel_err_stm/real(N - 1, sp)
    abs_err_stm = abs_err_stm/real(N - 1, sp)

    if (enforce_sym) u(2:N - 1, 2:N - 1) = 0.5_sp*(u(2:N - 1, 2:N - 1) + transpose(u(2:N - 1, 2:N - 1)))

  end subroutine linop_re_rep_sp

  subroutine linop_re_rep_dp(a, b, N, &
                             p, pp, q, &
                             bound_a_id, bound_b_id, &
                             mixing_a, mixing_b, &
                             enforce_sym, &
                             u, &
                             rel_err_stm, abs_err_stm)

    !Defines the matrix corresponding to the operator
    !L = {p(x), q(x), w(x)} in the
    !x representation.

    !Assumes N > 5.
    !Assumes recognizable and coherent Id-s for boundaries.

    real(dp), intent(in) :: a, b
    integer, intent(in) :: N
    real(dp), intent(in) :: p(N), pp(N), q(N)
    integer, intent(in) :: bound_a_id, bound_b_id
    real(dp), intent(in) :: mixing_a, mixing_b
    logical, intent(in) :: enforce_sym

    real(dp), intent(out) :: u(N, N)
    real(dp), intent(out) :: rel_err_stm, abs_err_stm

    integer :: i
    real(dp) :: spacing

    spacing = abs(b - a)/real(N - 1, dp)
    u = 0.0_dp
    rel_err_stm = 0.0_dp; abs_err_stm = 0.0_dp

    select case (bound_a_id)
    case (1) !Dirichlet.
      u(1, 1) = 1.0_dp
    case (2) !Neumann.
      u(1, 1) = 1.0_dp
      u(1, 2) = -1.0_dp
    case (3) !Mixed.
      u(1, 1) = 1.0_dp + (mixing_a/spacing)
      u(1, 2) = -mixing_a/spacing
    case (4) !Periodic.
      !Continuity.
      u(1, 1) = 1.0_dp
      u(1, N) = -1.0_dp
    case (0, -1) !Free or singular.
      u(1, 1) = q(1) - 2.0_dp*p(1)/(spacing**2)
      u(1, 2) = p(1)/(spacing**2) + pp(1)/(2*spacing)
      u(2, 1) = p(2)/(spacing**2) - pp(2)/(2*spacing)
    end select

    select case (bound_b_id)
    case (1) !Dirichlet.
      u(N, N) = 1.0_dp
    case (2) !Neumann.
      u(N, N) = 1.0_dp
      u(N - 1, N) = -1.0_dp
    case (3) !Mixed.
      u(N, N) = 1.0_dp + (mixing_b/spacing)
      u(N - 1, N) = -mixing_b/spacing
    case (4) !Periodic.
      !Continuity of the derivative.
      u(1, N) = -1.0_dp
      u(2, N) = 1.0_dp
      u(N - 1, N) = 1.0_dp
      u(N, N) = -1.0_dp
    case (0, -1) !Free or singular.
      u(N - 1, N) = p(N - 1)/(spacing**2) + pp(N - 1)/(2*spacing)
      u(N, N - 1) = p(N)/(spacing**2) - pp(N)/(2*spacing)
      u(N, N) = q(N) - 2.0_dp*p(N)/(spacing**2)
    end select

    u(2, 2) = q(2) - 2.0_dp*p(2)/(spacing**2)
    u(2, 3) = p(2)/(spacing**2) + pp(2)/(2*spacing)
    if (abs(pp(2)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - pp(1)/pp(2))
    abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(1) - pp(2))
    do concurrent(i=3:N - 2)
      u(i, i - 1) = p(i)/(spacing**2) - pp(i)/(2*spacing)
      u(i, i) = q(i) - 2.0_dp*p(i)/(spacing**2)
      u(i, i + 1) = p(i)/(spacing**2) + pp(i)/(2*spacing)
      if (abs(pp(N)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - pp(i - 1)/pp(i))
      abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(i - 1) - pp(i))
    enddo
    u(N - 1, N - 2) = p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing)
    u(N - 1, N - 1) = q(N - 1) - 2.0_dp*p(N - 1)/(spacing**2)
    if (abs(pp(N - 1)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - pp(N - 2)/pp(N - 1))
    abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(N - 2) - pp(N - 1))
    if (abs(pp(N)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - pp(N - 1)/pp(N))
    abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(N - 1) - pp(N))

    rel_err_stm = rel_err_stm/real(N - 1, dp)
    abs_err_stm = abs_err_stm/real(N - 1, dp)

    if (enforce_sym) u(2:N - 1, 2:N - 1) = 0.5_dp*(u(2:N - 1, 2:N - 1) + transpose(u(2:N - 1, 2:N - 1)))

  end subroutine linop_re_rep_dp

  subroutine linop_c_rep_sp(a, b, N, &
                            p, pp, q, &
                            bound_a_id, bound_b_id, &
                            mixing_a, mixing_b, &
                            enforce_sym, &
                            u, &
                            rel_err_stm, abs_err_stm)

    !Defines the matrix corresponding to the operator
    !L = {p(x), q(x), w(x)} in the
    !x representation.

    !Assumes N > 5.
    !Assumes recognizable and coherent Id-s for boundaries.

    real(sp), intent(in) :: a, b
    integer, intent(in) :: N
    complex(sp), intent(in) :: p(N), pp(N), q(N)
    integer, intent(in) :: bound_a_id, bound_b_id
    complex(sp), intent(in) :: mixing_a, mixing_b
    logical, intent(in) :: enforce_sym

    complex(sp), intent(out) :: u(N, N)
    real(sp), intent(out) :: rel_err_stm, abs_err_stm

    integer :: i
    real(sp) :: spacing

    spacing = abs(b - a)/real(N - 1, sp)
    u = cmplx_0_sp
    rel_err_stm = 0.0_sp; abs_err_stm = 0.0_sp

    select case (bound_a_id)
    case (1) !Dirichlet.
      u(1, 1) = cmplx_1_sp
    case (2) !Neumann.
      u(1, 1) = cmplx_1_sp
      u(1, 2) = -cmplx_1_sp
    case (3) !Mixed.
      u(1, 1) = cmplx_1_sp + (mixing_a/spacing)
      u(1, 2) = -mixing_a/spacing
    case (4) !Periodic.
      !Continuity.
      u(1, 1) = cmplx_1_sp
      u(1, N) = -cmplx_1_sp
    case (0, -1) !Free or singular.
      u(1, 1) = q(1) - 2.0_sp*p(1)/(spacing**2)
      u(1, 2) = p(1)/(spacing**2) + pp(1)/(2*spacing)
      u(2, 1) = p(2)/(spacing**2) - pp(2)/(2*spacing)
    end select

    select case (bound_b_id)
    case (1) !Dirichlet.
      u(N, N) = cmplx_1_sp
    case (2) !Neumann.
      u(N, N) = cmplx_1_sp
      u(N - 1, N) = -cmplx_1_sp
    case (3) !Mixed.
      u(N, N) = cmplx_1_sp + (mixing_b/spacing)
      u(N - 1, N) = -mixing_b/spacing
    case (4) !Periodic.
      !Continuity of the derivative.
      u(1, N) = -cmplx_1_sp
      u(2, N) = cmplx_1_sp
      u(N - 1, N) = cmplx_1_sp
      u(N, N) = -cmplx_1_sp
    case (0, -1) !Free or singular.
      u(N - 1, N) = p(N - 1)/(spacing**2) + pp(N - 1)/(2*spacing)
      u(N, N - 1) = p(N)/(spacing**2) - pp(N)/(2*spacing)
      u(N, N) = q(N) - 2.0_sp*p(N)/(spacing**2)
    end select

    u(2, 2) = q(2) - 2.0_sp*p(2)/(spacing**2)
    u(2, 3) = p(2)/(spacing**2) + pp(2)/(2*spacing)
    if (abs(pp(2)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - abs(pp(1)/pp(2)))
    abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(1) - pp(2))
    do concurrent(i=3:N - 2)
      u(i, i - 1) = p(i)/(spacing**2) - pp(i)/(2*spacing)
      u(i, i) = q(i) - 2.0_sp*p(i)/(spacing**2)
      u(i, i + 1) = p(i)/(spacing**2) + pp(i)/(2*spacing)
      if (abs(pp(N)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - abs(pp(i - 1)/pp(i)))
      abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(i - 1) - pp(i))
    enddo
    u(N - 1, N - 2) = p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing)
    u(N - 1, N - 1) = q(N - 1) - 2.0_sp*p(N - 1)/(spacing**2)
    if (abs(pp(N - 1)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - abs(pp(N - 2)/pp(N - 1)))
    abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(N - 2) - pp(N - 1))
    if (abs(pp(N)) > tol_sp) rel_err_stm = rel_err_stm + 0.5_sp*abs(1.0_sp - abs(pp(N - 1)/pp(N)))
    abs_err_stm = abs_err_stm + 0.5_sp*abs(pp(N - 1) - pp(N))

    rel_err_stm = rel_err_stm/real(N - 1, sp)
    abs_err_stm = abs_err_stm/real(N - 1, sp)

    if (enforce_sym) u(2:N - 1, 2:N - 1) = 0.5_sp*(u(2:N - 1, 2:N - 1) + conjg(transpose(u(2:N - 1, 2:N - 1))))

  end subroutine linop_c_rep_sp

  subroutine linop_c_rep_dp(a, b, N, &
                            p, pp, q, &
                            bound_a_id, bound_b_id, &
                            mixing_a, mixing_b, &
                            enforce_sym, &
                            u, &
                            rel_err_stm, abs_err_stm)

    !Defines the matrix corresponding to the operator
    !L = {p(x), q(x), w(x)} in the
    !x representation.

    !Assumes N > 5.
    !Assumes recognizable and coherent Id-s for boundaries.

    real(dp), intent(in) :: a, b
    integer, intent(in) :: N
    complex(dp), intent(in) :: p(N), pp(N), q(N)
    integer, intent(in) :: bound_a_id, bound_b_id
    complex(dp), intent(in) :: mixing_a, mixing_b
    logical, intent(in) :: enforce_sym

    complex(dp), intent(out) :: u(N, N)
    real(dp), intent(out) :: rel_err_stm, abs_err_stm

    integer :: i
    real(dp) :: spacing

    spacing = abs(b - a)/real(N - 1, dp)
    u = cmplx_0_dp
    rel_err_stm = 0.0_dp; abs_err_stm = 0.0_dp

    select case (bound_a_id)
    case (1) !Dirichlet.
      u(1, 1) = cmplx_1_dp
    case (2) !Neumann.
      u(1, 1) = cmplx_1_dp
      u(1, 2) = -cmplx_1_dp
    case (3) !Mixed.
      u(1, 1) = cmplx_1_dp + (mixing_a/spacing)
      u(1, 2) = -mixing_a/spacing
    case (4) !Periodic.
      !Continuity.
      u(1, 1) = cmplx_1_dp
      u(1, N) = -cmplx_1_dp
    case (0, -1) !Free or singular.
      u(1, 1) = q(1) - 2.0_dp*p(1)/(spacing**2)
      u(1, 2) = p(1)/(spacing**2) + pp(1)/(2*spacing)
      u(2, 1) = p(2)/(spacing**2) - pp(2)/(2*spacing)
    end select

    select case (bound_b_id)
    case (1) !Dirichlet.
      u(N, N) = cmplx_1_dp
    case (2) !Neumann.
      u(N, N) = cmplx_1_dp
      u(N - 1, N) = -cmplx_1_dp
    case (3) !Mixed.
      u(N, N) = cmplx_1_dp + (mixing_b/spacing)
      u(N - 1, N) = -mixing_b/spacing
    case (4) !Periodic.
      !Continuity of the derivative.
      u(1, N) = -cmplx_1_dp
      u(2, N) = cmplx_1_dp
      u(N - 1, N) = cmplx_1_dp
      u(N, N) = -cmplx_1_dp
    case (0, -1) !Free or singular.
      u(N - 1, N) = p(N - 1)/(spacing**2) + pp(N - 1)/(2*spacing)
      u(N, N - 1) = p(N)/(spacing**2) - pp(N)/(2*spacing)
      u(N, N) = q(N) - 2.0_dp*p(N)/(spacing**2)
    end select

    u(2, 2) = q(2) - 2.0_dp*p(2)/(spacing**2)
    u(2, 3) = p(2)/(spacing**2) + pp(2)/(2*spacing)
    if (abs(pp(2)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - abs(pp(1)/pp(2)))
    abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(1) - pp(2))
    do concurrent(i=3:N - 2)
      u(i, i - 1) = p(i)/(spacing**2) - pp(i)/(2*spacing)
      u(i, i) = q(i) - 2.0_dp*p(i)/(spacing**2)
      u(i, i + 1) = p(i)/(spacing**2) + pp(i)/(2*spacing)
      if (abs(pp(N)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - abs(pp(i - 1)/pp(i)))
      abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(i - 1) - pp(i))
    enddo
    u(N - 1, N - 2) = p(N - 1)/(spacing**2) - pp(N - 1)/(2*spacing)
    u(N - 1, N - 1) = q(N - 1) - 2.0_dp*p(N - 1)/(spacing**2)
    if (abs(pp(N - 1)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - abs(pp(N - 2)/pp(N - 1)))
    abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(N - 2) - pp(N - 1))
    if (abs(pp(N)) > tol_dp) rel_err_stm = rel_err_stm + 0.5_dp*abs(1.0_dp - abs(pp(N - 1)/pp(N)))
    abs_err_stm = abs_err_stm + 0.5_dp*abs(pp(N - 1) - pp(N))

    rel_err_stm = rel_err_stm/real(N - 1, dp)
    abs_err_stm = abs_err_stm/real(N - 1, dp)

    if (enforce_sym) u(2:N - 1, 2:N - 1) = 0.5_dp*(u(2:N - 1, 2:N - 1) + conjg(transpose(u(2:N - 1, 2:N - 1))))

  end subroutine linop_c_rep_dp

end module SLP_linop
