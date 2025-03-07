module SLP_obj

  use, intrinsic :: Iso_Fortran_ENV, only: stdout => output_unit

  use SLP_kinds, only: sp, dp

  implicit none
  private

  real(sp), parameter :: tol_sp = 1.0E2_sp*epsilon(1.0_sp)
  real(dp), parameter :: tol_dp = 1.0E2_dp*epsilon(1.0_dp)

  type, public :: SLP_sp
    private
    !Name.
    character(len=120) :: nm
    !Number of states.
    integer :: Ns = 1
    !Spacing.
    real(sp) :: spc
    !Boundary condition configuration status.
    logical :: lbid_a, lbid_b
    !Matrix representing the operator and metric.
    real(sp), allocatable :: L(:, :), Me(:, :)
    !Enforce symmetrization.
    logical :: sym = .false.
    !Error estimates.
    real(sp) :: rel_st, abs_st
    !Eigenvalues.
    real(sp), allocatable :: eivl(:)
    !Eigenvectors.
    real(sp), allocatable :: eivc(:, :)
    !Initialization status.
    logical :: init = .false.
    !Solve status.
    logical :: slv = .false.
  contains
    private
    procedure, pass(self) :: cons_gbound_sp
    procedure, pass(self) :: cons_ibound_sp
    generic, public :: define => cons_gbound_sp, cons_ibound_sp
    procedure, pass(self), public :: solve => slv_sp
    procedure, pass(self), public :: name => get_name_sp
    procedure, pass(self), public :: relative_error => get_rel_err_sp
    procedure, pass(self), public :: absolute_error => get_abs_err_sp
    procedure, pass(self), public :: Ndsc => get_states_sp
    procedure, pass(self), public :: spacing => get_spacing_sp
    procedure, pass(self) :: get_all_eigs_sp
    procedure, pass(self) :: get_eig_sp
    generic, public :: eig => get_all_eigs_sp, get_eig_sp
    procedure, pass(self) :: get_all_eivecs_sp
    procedure, pass(self) :: get_eivec_sp
    generic, public :: eivec => get_all_eivecs_sp, get_eivec_sp
    procedure, pass(self), public :: initialized => get_init_status_sp
    procedure, pass(self), public :: solved => get_solv_status_sp
  end type

  type, public :: SLP_dp
    private
    !Name.
    character(len=120) :: nm
    !Number of states.
    integer :: Ns = 1
    !Spacing.
    real(dp) :: spc
    !Boundary condition configuration status.
    logical :: lbid_a, lbid_b
    !Matrix representing the operator and metric.
    real(dp), allocatable :: L(:, :), Me(:, :)
    !Enforce symmetrization.
    logical :: sym = .false.
    !Error estimates.
    real(dp) :: rel_st, abs_st
    !Eigenvalues.
    real(dp), allocatable :: eivl(:)
    !Eigenvectors.
    real(dp), allocatable :: eivc(:, :)
    !Initialization status.
    logical :: init = .false.
    !Solve status.
    logical :: slv = .false.
  contains
    private
    procedure, pass(self) :: cons_gbound_dp
    procedure, pass(self) :: cons_ibound_dp
    generic, public :: define => cons_gbound_dp, cons_ibound_dp
    procedure, pass(self), public :: solve => slv_dp
    procedure, pass(self), public :: name => get_name_dp
    procedure, pass(self), public :: relative_error => get_rel_err_dp
    procedure, pass(self), public :: absolute_error => get_abs_err_dp
    procedure, pass(self), public :: Ndsc => get_states_dp
    procedure, pass(self), public :: spacing => get_spacing_dp
    procedure, pass(self) :: get_all_eigs_dp
    procedure, pass(self) :: get_eig_dp
    generic, public :: eig => get_all_eigs_dp, get_eig_dp
    procedure, pass(self) :: get_all_eivecs_dp
    procedure, pass(self) :: get_eivec_dp
    generic, public :: eivec => get_all_eivecs_dp, get_eivec_dp
    procedure, pass(self), public :: initialized => get_init_status_dp
    procedure, pass(self), public :: solved => get_solv_status_dp
  end type

contains

  subroutine cons_gbound_sp(self, &
                            name, &
                            a, b, N, &
                            p, pp, q, w, &
                            bound_cond, &
                            mixing_param, &
                            enforce_self_adjoint, &
                            silent)

    use SLP_auxiliary, only: boundary_condition_linter
    use SLP_util, only: set_metric
    use SLP_linop, only: linop

    class(SLP_sp), intent(out) :: self
    character(len=*), intent(in) :: name
    real(sp), intent(in) :: a, b
    integer, intent(in) :: N
    real(sp), intent(in) :: p(N), q(N)
    real(sp), optional, intent(in) :: pp(N)
    real(sp), optional, intent(in) :: w(N)
    real(sp) :: ppl(N), wl(N)
    character(len=*), intent(in) :: bound_cond
    real(sp), optional, intent(in) :: mixing_param
    logical, optional, intent(in) :: enforce_self_adjoint
    logical, optional, intent(in) :: silent

    real(sp) :: spacing
    logical :: not_silent = .true.
    integer :: i

    integer :: bid_a, bid_b
    real(sp) :: mx_a, mx_b

    mx_a = 0.0_sp; mx_b = 0.0_sp

    if (present(silent)) not_silent = .not. silent

    self%nm = trim(adjustl(name))

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initializing SLP <<"//trim(adjustl(self%nm))//">>."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Checking interval..."

    if (N < 6) error stop "SLP: N < 6."
    self%Ns = N

    spacing = abs(b - a)/real(N - 1, sp)
    if (spacing < tol_sp) error stop "SLP: Too small spacing between points."
    if (spacing > huge(1.0_sp)) error stop "SLP: Too large spacing between points."
    if (isnan(spacing)) error stop "SLP: Bad input: bounds a, b."
    self%spc = spacing

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verifying data..."

    allocate (self%L(N, N), self%Me(N, N))
    allocate (self%eivl(N), self%eivc(N, N))

    if (any(p <= 0.0_sp)) then
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p(x) < 0 for some x in [a, b]."
      if (not_silent) write (unit=stdout, fmt="(A)") "       Sturm-Lioville theorems are NOT expected to hold."
    endif
    if (abs(p(1)) < tol_sp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(a) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (abs(p(self%Ns)) < tol_sp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(b) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (present(pp)) then
      ppl = pp
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p'(x) not provided, calculating from p(x)..."
      ppl(1) = (p(2) - p(1))/spacing
      do i = 2, N - 1
        ppl(i) = (p(i + 1) - p(i - 1))/(2*spacing)
      enddo
      ppl(N) = (p(N) - p(N - 1))/spacing
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: p'(x) calculated."
    endif

    if (present(w)) then
      wl = w
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: w(x) not provided, setting w(x) = 1."
      wl = 1.0_sp
    endif

    if (any(wl(2:self%Ns) < 0.0_sp)) then
      error stop "SLP: w(x) < 0 for some x in (a, b)."
    endif
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verified."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: At both boundaries:"
    call boundary_condition_linter(name=bound_cond, &
                                   not_silent=not_silent, &
                                   boundary_condition=self%lbid_a, &
                                   boundary_condition_id=bid_a)
    self%lbid_b = self%lbid_a
    bid_b = bid_a
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (bid_a == 3) then
      if (.not. (present(mixing_param))) &
        error stop "SLP: Boundary condition is <<Mixed>> and a mixing parameter has not been specified."
      mx_a = mixing_param; mx_b = mixing_param
    else
      if (present(mixing_param)) &
        error stop "SLP: Boundary condition is not <<Mixed>> and a mixing parameter has been specified."
    endif

    if (present(enforce_self_adjoint)) self%sym = enforce_self_adjoint
    if (self%sym) then
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Enforcing symmetrization."
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Not enforcing symmetrization."
    endif

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating metric..."
    self%Me = set_metric(mass=wl, bound_a=self%lbid_a, bound_b=self%lbid_b)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating matrix representing operator..."
    call linop(a=a, b=b, N=self%Ns, &
               p=p, pp=ppl, q=q, &
               bound_a_id=bid_a, bound_b_id=bid_b, &
               mixing_a=mx_a, mixing_b=mx_b, &
               enforce_sym=self%sym, &
               u=self%L, &
               rel_err_stm=self%rel_st, abs_err_stm=self%abs_st)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    self%init = .true.

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initialized."
    if (not_silent) write (unit=stdout, fmt="(A)") ""

  end subroutine cons_gbound_sp

  subroutine cons_ibound_sp(self, &
                            name, &
                            a, b, N, &
                            p, pp, q, w, &
                            bound_cond_a, bound_cond_b, &
                            mixing_param_a, mixing_param_b, &
                            enforce_self_adjoint, &
                            silent)

    use SLP_auxiliary, only: boundary_condition_linter
    use SLP_util, only: set_metric
    use SLP_linop, only: linop

    class(SLP_sp), intent(out) :: self
    character(len=*), intent(in) :: name
    real(sp), intent(in) :: a, b
    integer, intent(in) :: N
    real(sp), intent(in) :: p(N), q(N)
    real(sp), optional, intent(in) :: pp(N)
    real(sp), optional, intent(in) :: w(N)
    real(sp) :: ppl(N), wl(N)
    character(len=*), intent(in) :: bound_cond_a, bound_cond_b
    real(sp), optional, intent(in) :: mixing_param_a, mixing_param_b
    logical, optional, intent(in) :: enforce_self_adjoint
    logical, optional, intent(in) :: silent

    real(sp) :: spacing
    logical :: not_silent = .true.
    integer :: i

    integer :: bid_a, bid_b
    real(sp) :: mx_a, mx_b

    mx_a = 0.0_sp; mx_b = 0.0_sp

    if (present(silent)) not_silent = .not. silent

    self%nm = trim(adjustl(name))

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initializing SLP <<"//trim(adjustl(self%nm))//">>."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Checking interval..."

    if (N < 6) error stop "SLP: N < 6."
    self%Ns = N

    spacing = abs(b - a)/real(N - 1, sp)
    if (spacing < tol_sp) error stop "SLP: Too small spacing between points."
    if (spacing > huge(1.0_sp)) error stop "SLP: Too large spacing between points."
    if (isnan(spacing)) error stop "SLP: Bad input: bounds a, b."
    self%spc = spacing

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verifying data..."

    allocate (self%L(N, N), self%Me(N, N))
    allocate (self%eivl(N), self%eivc(N, N))

    if (any(p <= 0.0_sp)) then
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p(x) < 0 for some x in [a, b]."
      if (not_silent) write (unit=stdout, fmt="(A)") "       Sturm-Lioville theorems are NOT expected to hold."
    endif
    if (abs(p(1)) < tol_sp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(a) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (abs(p(self%Ns)) < tol_sp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(b) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (present(pp)) then
      ppl = pp
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p'(x) not provided, calculating from p(x)..."
      ppl(1) = (p(2) - p(1))/spacing
      do i = 2, N - 1
        ppl(i) = (p(i + 1) - p(i - 1))/(2*spacing)
      enddo
      ppl(N) = (p(N) - p(N - 1))/spacing
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: p'(x) calculated."
    endif

    if (present(w)) then
      wl = w
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: w(x) not provided, setting w(x) = 1."
      wl = 1.0_sp
    endif

    if (any(wl(2:self%Ns) < 0.0_sp)) then
      error stop "SLP: w(x) < 0 for some x in (a, b)."
    endif
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verified."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: At first boundary :"
    call boundary_condition_linter(name=bound_cond_a, &
                                   not_silent=not_silent, &
                                   boundary_condition=self%lbid_a, &
                                   boundary_condition_id=bid_a)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: At second boundary :"
    call boundary_condition_linter(name=bound_cond_b, &
                                   not_silent=not_silent, &
                                   boundary_condition=self%lbid_b, &
                                   boundary_condition_id=bid_b)

    if ((bid_a == 4) .and. (bid_b /= 4)) &
      error stop "SLP: First boundary condition is <<Periodic>> and second boundary condition is not."
    if ((bid_a /= 4) .and. (bid_b == 4)) &
      error stop "SLP: Second boundary condition is <<Periodic>> and first boundary condition is not."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (bid_a == 3) then
      if (.not. (present(mixing_param_a))) &
        error stop "SLP: First boundary condition is <<Mixed>> and a mixing parameter has not been specified."
      mx_a = mixing_param_a
    else
      if (present(mixing_param_a)) &
        error stop "SLP: First boundary condition is not <<Mixed>> and a mixing parameter has been specified."
    endif

    if (bid_b == 3) then
      if (.not. (present(mixing_param_b))) &
        error stop "SLP: Second boundary condition is <<Mixed>> and a mixing parameter has not been specified."
      mx_b = mixing_param_b
    else
      if (present(mixing_param_b)) &
        error stop "SLP: Second boundary condition is not <<Mixed>> and a mixing parameter has been specified."
    endif

    if (present(enforce_self_adjoint)) self%sym = enforce_self_adjoint
    if (self%sym) then
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Enforcing symmetrization."
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Not enforcing symmetrization."
    endif

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating metric..."
    self%Me = set_metric(mass=wl, bound_a=self%lbid_a, bound_b=self%lbid_b)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating matrix representing operator..."
    call linop(a=a, b=b, N=self%Ns, &
               p=p, pp=ppl, q=q, &
               bound_a_id=bid_a, bound_b_id=bid_b, &
               mixing_a=mx_a, mixing_b=mx_b, &
               enforce_sym=self%sym, &
               u=self%L, &
               rel_err_stm=self%rel_st, abs_err_stm=self%abs_st)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    self%init = .true.

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initialized."
    if (not_silent) write (unit=stdout, fmt="(A)") ""

  end subroutine cons_ibound_sp

  subroutine slv_sp(self, silent)

    use SLP_util, only: dg_gen
    use SLP_auxiliary, only: cshift_N_largest_to_top

    class(SLP_sp), intent(inout) :: self
    logical, optional, intent(in) :: silent

    logical :: not_silent = .true.
    real(sp) :: D(self%Ns, self%Ns)
    integer :: nbcond = 0
    integer :: shift
    character(len=120) :: aux

    if (present(silent)) not_silent = .not. silent

    if (.not. (self%init)) error stop "SLP: Not initialized."

    if (self%lbid_a .or. self%lbid_b) then
      nbcond = 1
      if (self%lbid_a .and. self%lbid_b) nbcond = 2
    endif
    write (aux, fmt="(I0)") nbcond
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Detected "//trim(adjustl(aux))//" boundary conditions."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Solving Sturm-Liouville equation..."
    call dg_gen(matrix=self%L, metric=self%Me, P=self%eivc, D=D, eig=self%eivl)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Normalizing and rearranging..."
    shift = cshift_N_largest_to_top(N=nbcond, arr=self%eivl)
    self%eivl = cshift(self%eivl, shift)
    self%eivc = cshift(self%eivc, shift, dim=2)
    self%eivc = self%eivc/sqrt(self%spc)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    self%slv = .true.

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Solved."
    if (not_silent) write (unit=stdout, fmt="(A)") ""

  end subroutine slv_sp

  pure function get_name_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    character(len=len(trim(adjustl(self%nm)))) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = trim(adjustl(self%nm))
  end function get_name_sp

  pure function get_states_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    integer :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%Ns
  end function get_states_sp

  pure function get_spacing_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    real(sp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%spc
  end function get_spacing_sp

  pure function get_rel_err_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    real(sp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%rel_st
  end function get_rel_err_sp

  pure function get_abs_err_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    real(sp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%abs_st
  end function get_abs_err_sp

  pure function get_all_eigs_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    real(sp) :: u(self%Ns)
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    u = self%eivl
  end function get_all_eigs_sp

  pure function get_eig_sp(self, i) result(u)
    class(SLP_sp), intent(in) :: self
    integer, intent(in) :: i
    real(sp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    if ((i < 1) .or. (i > self%Ns)) error stop "SLP: Requested eigenvalue out of bounds."
    u = self%eivl(i)
  end function get_eig_sp

  pure function get_all_eivecs_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    real(sp) :: u(self%Ns, self%Ns)
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    u = self%eivc
  end function get_all_eivecs_sp

  pure function get_eivec_sp(self, i) result(u)
    class(SLP_sp), intent(in) :: self
    integer, intent(in) :: i
    real(sp) :: u(self%Ns)
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    if ((i < 1) .or. (i > self%Ns)) error stop "SLP: Requested eigenvector out of bounds."
    u = self%eivc(:, i)
  end function get_eivec_sp

  pure function get_init_status_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    logical :: u
    u = self%init
  end function get_init_status_sp

  pure function get_solv_status_sp(self) result(u)
    class(SLP_sp), intent(in) :: self
    logical :: u
    u = self%slv
  end function get_solv_status_sp

  subroutine cons_gbound_dp(self, &
                            name, &
                            a, b, N, &
                            p, pp, q, w, &
                            bound_cond, &
                            mixing_param, &
                            enforce_self_adjoint, &
                            silent)

    use SLP_auxiliary, only: boundary_condition_linter
    use SLP_util, only: set_metric
    use SLP_linop, only: linop

    class(SLP_dp), intent(out) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: a, b
    integer, intent(in) :: N
    real(dp), intent(in) :: p(N), q(N)
    real(dp), optional, intent(in) :: pp(N)
    real(dp), optional, intent(in) :: w(N)
    real(dp) :: ppl(N), wl(N)
    character(len=*), intent(in) :: bound_cond
    real(dp), optional, intent(in) :: mixing_param
    logical, optional, intent(in) :: enforce_self_adjoint
    logical, optional, intent(in) :: silent

    real(dp) :: spacing
    logical :: not_silent = .true.
    integer :: i

    integer :: bid_a, bid_b
    real(dp) :: mx_a, mx_b

    mx_a = 0.0_dp; mx_b = 0.0_dp

    if (present(silent)) not_silent = .not. silent

    self%nm = trim(adjustl(name))

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initializing SLP <<"//trim(adjustl(self%nm))//">>."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Checking interval..."

    if (N < 6) error stop "SLP: N < 6."
    self%Ns = N

    spacing = abs(b - a)/real(N - 1, dp)
    if (spacing < tol_dp) error stop "SLP: Too small spacing between points."
    if (spacing > huge(1.0_dp)) error stop "SLP: Too large spacing between points."
    if (isnan(spacing)) error stop "SLP: Bad input: bounds a, b."
    self%spc = spacing

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verifying data..."

    allocate (self%L(N, N), self%Me(N, N))
    allocate (self%eivl(N), self%eivc(N, N))

    if (any(p <= 0.0_dp)) then
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p(x) < 0 for some x in [a, b]."
      if (not_silent) write (unit=stdout, fmt="(A)") "       Sturm-Lioville theorems are NOT expected to hold."
    endif
    if (abs(p(1)) < tol_dp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(a) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (abs(p(self%Ns)) < tol_dp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(b) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (present(pp)) then
      ppl = pp
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p'(x) not provided, calculating from p(x)..."
      ppl(1) = (p(2) - p(1))/spacing
      do i = 2, N - 1
        ppl(i) = (p(i + 1) - p(i - 1))/(2*spacing)
      enddo
      ppl(N) = (p(N) - p(N - 1))/spacing
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: p'(x) calculated."
    endif

    if (present(w)) then
      wl = w
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: w(x) not provided, setting w(x) = 1."
      wl = 1.0_dp
    endif

    if (any(wl(2:self%Ns) < 0.0_dp)) then
      error stop "SLP: w(x) < 0 for some x in (a, b)."
    endif
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verified."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: At both boundaries:"
    call boundary_condition_linter(name=bound_cond, &
                                   not_silent=not_silent, &
                                   boundary_condition=self%lbid_a, &
                                   boundary_condition_id=bid_a)
    self%lbid_b = self%lbid_a
    bid_b = bid_a
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (bid_a == 3) then
      if (.not. (present(mixing_param))) &
        error stop "SLP: Boundary condition is <<Mixed>> and a mixing parameter has not been specified."
      mx_a = mixing_param; mx_b = mixing_param
    else
      if (present(mixing_param)) &
        error stop "SLP: Boundary condition is not <<Mixed>> and a mixing parameter has been specified."
    endif

    if (present(enforce_self_adjoint)) self%sym = enforce_self_adjoint
    if (self%sym) then
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Enforcing symmetrization."
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Not enforcing symmetrization."
    endif

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating metric..."
    self%Me = set_metric(mass=wl, bound_a=self%lbid_a, bound_b=self%lbid_b)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating matrix representing operator..."
    call linop(a=a, b=b, N=self%Ns, &
               p=p, pp=ppl, q=q, &
               bound_a_id=bid_a, bound_b_id=bid_b, &
               mixing_a=mx_a, mixing_b=mx_b, &
               enforce_sym=self%sym, &
               u=self%L, &
               rel_err_stm=self%rel_st, abs_err_stm=self%abs_st)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    self%init = .true.

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initialized."
    if (not_silent) write (unit=stdout, fmt="(A)") ""

  end subroutine cons_gbound_dp

  subroutine cons_ibound_dp(self, &
                            name, &
                            a, b, N, &
                            p, pp, q, w, &
                            bound_cond_a, bound_cond_b, &
                            mixing_param_a, mixing_param_b, &
                            enforce_self_adjoint, &
                            silent)

    use SLP_auxiliary, only: boundary_condition_linter
    use SLP_util, only: set_metric
    use SLP_linop, only: linop

    class(SLP_dp), intent(out) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: a, b
    integer, intent(in) :: N
    real(dp), intent(in) :: p(N), q(N)
    real(dp), optional, intent(in) :: pp(N)
    real(dp), optional, intent(in) :: w(N)
    real(dp) :: ppl(N), wl(N)
    character(len=*), intent(in) :: bound_cond_a, bound_cond_b
    real(dp), optional, intent(in) :: mixing_param_a, mixing_param_b
    logical, optional, intent(in) :: enforce_self_adjoint
    logical, optional, intent(in) :: silent

    real(dp) :: spacing
    logical :: not_silent = .true.
    integer :: i

    integer :: bid_a, bid_b
    real(dp) :: mx_a, mx_b

    mx_a = 0.0_dp; mx_b = 0.0_dp

    if (present(silent)) not_silent = .not. silent

    self%nm = trim(adjustl(name))

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initializing SLP <<"//trim(adjustl(self%nm))//">>."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Checking interval..."

    if (N < 6) error stop "SLP: N < 6."
    self%Ns = N

    spacing = abs(b - a)/real(N - 1, dp)
    if (spacing < tol_dp) error stop "SLP: Too small spacing between points."
    if (spacing > huge(1.0_dp)) error stop "SLP: Too large spacing between points."
    if (isnan(spacing)) error stop "SLP: Bad input: bounds a, b."
    self%spc = spacing

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verifying data..."

    allocate (self%L(N, N), self%Me(N, N))
    allocate (self%eivl(N), self%eivc(N, N))

    if (any(p <= 0.0_dp)) then
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p(x) < 0 for some x in [a, b]."
      if (not_silent) write (unit=stdout, fmt="(A)") "       Sturm-Lioville theorems are NOT expected to hold."
    endif
    if (abs(p(1)) < tol_dp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(a) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (abs(p(self%Ns)) < tol_dp) then
      if (not_silent) write (unit=stdout, fmt="(A)") &
        "  SLP: Warning: p(b) ~ 0. Consider setting <<singular>> or <<free>> conditions."
    endif
    if (present(pp)) then
      ppl = pp
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: p'(x) not provided, calculating from p(x)..."
      ppl(1) = (p(2) - p(1))/spacing
      do i = 2, N - 1
        ppl(i) = (p(i + 1) - p(i - 1))/(2*spacing)
      enddo
      ppl(N) = (p(N) - p(N - 1))/spacing
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: p'(x) calculated."
    endif

    if (present(w)) then
      wl = w
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "  SLP: Warning: w(x) not provided, setting w(x) = 1."
      wl = 1.0_dp
    endif

    if (any(wl(2:self%Ns) < 0.0_dp)) then
      error stop "SLP: w(x) < 0 for some x in (a, b)."
    endif
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Verified."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: At first boundary :"
    call boundary_condition_linter(name=bound_cond_a, &
                                   not_silent=not_silent, &
                                   boundary_condition=self%lbid_a, &
                                   boundary_condition_id=bid_a)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: At second boundary :"
    call boundary_condition_linter(name=bound_cond_b, &
                                   not_silent=not_silent, &
                                   boundary_condition=self%lbid_b, &
                                   boundary_condition_id=bid_b)

    if ((bid_a == 4) .and. (bid_b /= 4)) &
      error stop "SLP: First boundary condition is <<Periodic>> and second boundary condition is not."
    if ((bid_a /= 4) .and. (bid_b == 4)) &
      error stop "SLP: Second boundary condition is <<Periodic>> and first boundary condition is not."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (bid_a == 3) then
      if (.not. (present(mixing_param_a))) &
        error stop "SLP: First boundary condition is <<Mixed>> and a mixing parameter has not been specified."
      mx_a = mixing_param_a
    else
      if (present(mixing_param_a)) &
        error stop "SLP: First boundary condition is not <<Mixed>> and a mixing parameter has been specified."
    endif

    if (bid_b == 3) then
      if (.not. (present(mixing_param_b))) &
        error stop "SLP: Second boundary condition is <<Mixed>> and a mixing parameter has not been specified."
      mx_b = mixing_param_b
    else
      if (present(mixing_param_b)) &
        error stop "SLP: Second boundary condition is not <<Mixed>> and a mixing parameter has been specified."
    endif

    if (present(enforce_self_adjoint)) self%sym = enforce_self_adjoint
    if (self%sym) then
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Enforcing symmetrization."
    else
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Info.: Not enforcing symmetrization."
    endif

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating metric..."
    self%Me = set_metric(mass=wl, bound_a=self%lbid_a, bound_b=self%lbid_b)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Calculating matrix representing operator..."
    call linop(a=a, b=b, N=self%Ns, &
               p=p, pp=ppl, q=q, &
               bound_a_id=bid_a, bound_b_id=bid_b, &
               mixing_a=mx_a, mixing_b=mx_b, &
               enforce_sym=self%sym, &
               u=self%L, &
               rel_err_stm=self%rel_st, abs_err_stm=self%abs_st)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    self%init = .true.

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Initialized."
    if (not_silent) write (unit=stdout, fmt="(A)") ""

  end subroutine cons_ibound_dp

  subroutine slv_dp(self, silent)

    use SLP_util, only: dg_gen
    use SLP_auxiliary, only: cshift_N_largest_to_top

    class(SLP_dp), intent(inout) :: self
    logical, optional, intent(in) :: silent

    logical :: not_silent = .true.
    real(dp) :: D(self%Ns, self%Ns)
    integer :: nbcond = 0
    integer :: shift
    character(len=120) :: aux

    if (present(silent)) not_silent = .not. silent

    if (.not. (self%init)) error stop "SLP: Not initialized."

    if (self%lbid_a .or. self%lbid_b) then
      nbcond = 1
      if (self%lbid_a .and. self%lbid_b) nbcond = 2
    endif
    write (aux, fmt="(I0)") nbcond
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Detected "//trim(adjustl(aux))//" boundary conditions."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Solving Sturm-Liouville equation..."
    call dg_gen(matrix=self%L, metric=self%Me, P=self%eivc, D=D, eig=self%eivl)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Normalizing and rearranging..."
    shift = cshift_N_largest_to_top(N=nbcond, arr=self%eivl)
    self%eivl = cshift(self%eivl, shift)
    self%eivc = cshift(self%eivc, shift, dim=2)
    self%eivc = self%eivc/sqrt(self%spc)
    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Done."

    self%slv = .true.

    if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Solved."
    if (not_silent) write (unit=stdout, fmt="(A)") ""

  end subroutine slv_dp

  pure function get_name_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    character(len=len(trim(adjustl(self%nm)))) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = trim(adjustl(self%nm))
  end function get_name_dp

  pure function get_states_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    integer :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%Ns
  end function get_states_dp

  pure function get_spacing_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    real(dp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%spc
  end function get_spacing_dp

  pure function get_rel_err_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    real(dp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%rel_st
  end function get_rel_err_dp

  pure function get_abs_err_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    real(dp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    u = self%abs_st
  end function get_abs_err_dp

  pure function get_all_eigs_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    real(dp) :: u(self%Ns)
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    u = self%eivl
  end function get_all_eigs_dp

  pure function get_eig_dp(self, i) result(u)
    class(SLP_dp), intent(in) :: self
    integer, intent(in) :: i
    real(dp) :: u
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    if ((i < 1) .or. (i > self%Ns)) error stop "SLP: Requested eigenvalue out of bounds."
    u = self%eivl(i)
  end function get_eig_dp

  pure function get_all_eivecs_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    real(dp) :: u(self%Ns, self%Ns)
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    u = self%eivc
  end function get_all_eivecs_dp

  pure function get_eivec_dp(self, i) result(u)
    class(SLP_dp), intent(in) :: self
    integer, intent(in) :: i
    real(dp) :: u(self%Ns)
    if (.not. (self%init)) error stop "SLP: Not initialized."
    if (.not. (self%slv)) error stop "SLP: Not solved."
    if ((i < 1) .or. (i > self%Ns)) error stop "SLP: Requested eigenvector out of bounds."
    u = self%eivc(:, i)
  end function get_eivec_dp

  pure function get_init_status_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    logical :: u
    u = self%init
  end function get_init_status_dp

  pure function get_solv_status_dp(self) result(u)
    class(SLP_dp), intent(in) :: self
    logical :: u
    u = self%slv
  end function get_solv_status_dp

end module SLP_obj
