module Obj_Rnd_Functionality

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: new_unittest, unittest_type, error_type

  use SLP_kinds, only: sp, dp

  implicit none
  private

  real(sp), parameter :: tol_sp = 1.0_sp
  real(dp), parameter :: tol_dp = 1.0E-7_dp

  real(sp), parameter :: tol_abserr_sp = 1.0E-5_sp
  real(dp), parameter :: tol_abserr_dp = 1.0E-9_dp

  public :: collect_Obj_Rnd_Functionality

contains

  subroutine collect_Obj_Rnd_Functionality(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                  new_unittest("basic check", test_basic) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Dirichlet Dirichlet.", &
                  t_SLP_rnd_spDirichletDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Dirichlet Neumann.", &
                  t_SLP_rnd_spDirichletNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Dirichlet Mixed.", &
                  t_SLP_rnd_spDirichletMixed) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Dirichlet Free.", &
                  t_SLP_rnd_spDirichletFree) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Dirichlet Singular.", &
                  t_SLP_rnd_spDirichletSingular) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Neumann Dirichlet.", &
                  t_SLP_rnd_spNeumannDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Neumann Neumann.", &
                  t_SLP_rnd_spNeumannNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Neumann Mixed.", &
                  t_SLP_rnd_spNeumannMixed) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Neumann Free.", &
                  t_SLP_rnd_spNeumannFree) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Neumann Singular.", &
                  t_SLP_rnd_spNeumannSingular) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Mixed Dirichlet.", &
                  t_SLP_rnd_spMixedDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Mixed Neumann.", &
                  t_SLP_rnd_spMixedNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Mixed Mixed.", &
                  t_SLP_rnd_spMixedMixed) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Mixed Free.", &
                  t_SLP_rnd_spMixedFree) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Mixed Singular.", &
                  t_SLP_rnd_spMixedSingular) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Free Dirichlet.", &
                  t_SLP_rnd_spFreeDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Free Neumann.", &
                  t_SLP_rnd_spFreeNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Free Mixed.", &
                  t_SLP_rnd_spFreeMixed) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Free Free.", &
                  t_SLP_rnd_spFreeFree) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Free Singular.", &
                  t_SLP_rnd_spFreeSingular) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Singular Dirichlet.", &
                  t_SLP_rnd_spSingularDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Singular Neumann.", &
                  t_SLP_rnd_spSingularNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Singular Mixed.", &
                  t_SLP_rnd_spSingularMixed) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Singular Free.", &
                  t_SLP_rnd_spSingularFree) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Singular Singular.", &
                  t_SLP_rnd_spSingularSingular) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Dirichlet Dirichlet.", &
                  t_SLP_rnd_dpDirichletDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Dirichlet Neumann.", &
                  t_SLP_rnd_dpDirichletNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Dirichlet Mixed.", &
                  t_SLP_rnd_dpDirichletMixed) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Dirichlet Free.", &
                  t_SLP_rnd_dpDirichletFree) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Dirichlet Singular.", &
                  t_SLP_rnd_dpDirichletSingular) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Neumann Dirichlet.", &
                  t_SLP_rnd_dpNeumannDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Neumann Neumann.", &
                  t_SLP_rnd_dpNeumannNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Neumann Mixed.", &
                  t_SLP_rnd_dpNeumannMixed) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Neumann Free.", &
                  t_SLP_rnd_dpNeumannFree) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Neumann Singular.", &
                  t_SLP_rnd_dpNeumannSingular) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Mixed Dirichlet.", &
                  t_SLP_rnd_dpMixedDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Mixed Neumann.", &
                  t_SLP_rnd_dpMixedNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Mixed Mixed.", &
                  t_SLP_rnd_dpMixedMixed) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Mixed Free.", &
                  t_SLP_rnd_dpMixedFree) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Mixed Singular.", &
                  t_SLP_rnd_dpMixedSingular) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Free Dirichlet.", &
                  t_SLP_rnd_dpFreeDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Free Neumann.", &
                  t_SLP_rnd_dpFreeNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Free Mixed.", &
                  t_SLP_rnd_dpFreeMixed) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Free Free.", &
                  t_SLP_rnd_dpFreeFree) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Free Singular.", &
                  t_SLP_rnd_dpFreeSingular) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Singular Dirichlet.", &
                  t_SLP_rnd_dpSingularDirichlet) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Singular Neumann.", &
                  t_SLP_rnd_dpSingularNeumann) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Singular Mixed.", &
                  t_SLP_rnd_dpSingularMixed) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Singular Free.", &
                  t_SLP_rnd_dpSingularFree) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Singular Singular.", &
                  t_SLP_rnd_dpSingularSingular) &
                  , new_unittest("random diferential equation check &
                  &real (sp) SLP: Periodic Periodic.", &
                  t_SLP_rnd_spPeriodicPeriodic) &
                  , new_unittest("random diferential equation check &
                  &real (dp) SLP: Periodic Periodic.", &
                  t_SLP_rnd_dpPeriodicPeriodic) &
                ]

  end subroutine collect_Obj_Rnd_Functionality

  subroutine test_basic(error)
    use SLP_obj, only: SLP_sp, SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb_sp
    type(SLP_dp) :: prb_dp
    if (prb_sp%initialized()) then
      allocate (error)
      return
    endif
    if (prb_dp%initialized()) then
      allocate (error)
      return
    endif
  end subroutine test_basic

  subroutine t_SLP_rnd_spDirichletDirichlet(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(N)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spDirichletDirichlet

  subroutine t_SLP_rnd_spDirichletNeumann(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) - vl(N - 1)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spDirichletNeumann

  subroutine t_SLP_rnd_spDirichletMixed(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) + 1.0_sp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spDirichletMixed

  subroutine t_SLP_rnd_spDirichletFree(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spDirichletFree

  subroutine t_SLP_rnd_spDirichletSingular(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spDirichletSingular

  subroutine t_SLP_rnd_spNeumannDirichlet(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) - vl(2)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spNeumannDirichlet

  subroutine t_SLP_rnd_spNeumannNeumann(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) - vl(N - 1)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spNeumannNeumann

  subroutine t_SLP_rnd_spNeumannMixed(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) + 1.0_sp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spNeumannMixed

  subroutine t_SLP_rnd_spNeumannFree(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spNeumannFree

  subroutine t_SLP_rnd_spNeumannSingular(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spNeumannSingular

  subroutine t_SLP_rnd_spMixedDirichlet(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Dirichlet", &
                        mixing_param_a=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) - 1.0_sp*(vl(2) - vl(1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spMixedDirichlet

  subroutine t_SLP_rnd_spMixedNeumann(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Neumann", &
                        mixing_param_a=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N) - vl(N - 1)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) - 1.0_sp*(vl(2) - vl(1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spMixedNeumann

  subroutine t_SLP_rnd_spMixedMixed(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Mixed", &
                        mixing_param_a=1.0_sp, &
                        mixing_param_b=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - 1.0_sp*(vl(2) - vl(1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) + 1.0_sp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spMixedMixed

  subroutine t_SLP_rnd_spMixedFree(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Free", &
                        mixing_param_a=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - 1.0_sp*(vl(2) - vl(1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spMixedFree

  subroutine t_SLP_rnd_spMixedSingular(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Singular", &
                        mixing_param_a=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - 1.0_sp*(vl(2) - vl(1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spMixedSingular

  subroutine t_SLP_rnd_spFreeDirichlet(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spFreeDirichlet

  subroutine t_SLP_rnd_spFreeNeumann(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N) - vl(N - 1)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spFreeNeumann

  subroutine t_SLP_rnd_spFreeMixed(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N) + 1.0_sp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spFreeMixed

  subroutine t_SLP_rnd_spFreeFree(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spFreeFree

  subroutine t_SLP_rnd_spFreeSingular(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spFreeSingular

  subroutine t_SLP_rnd_spSingularDirichlet(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spSingularDirichlet

  subroutine t_SLP_rnd_spSingularNeumann(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N) - vl(N - 1)) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spSingularNeumann

  subroutine t_SLP_rnd_spSingularMixed(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_sp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(N) + 1.0_sp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spSingularMixed

  subroutine t_SLP_rnd_spSingularFree(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spSingularFree

  subroutine t_SLP_rnd_spSingularSingular(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spSingularSingular

  subroutine t_SLP_rnd_dpDirichletDirichlet(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(N)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpDirichletDirichlet

  subroutine t_SLP_rnd_dpDirichletNeumann(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) - vl(N - 1)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpDirichletNeumann

  subroutine t_SLP_rnd_dpDirichletMixed(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) + 1.0_dp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpDirichletMixed

  subroutine t_SLP_rnd_dpDirichletFree(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpDirichletFree

  subroutine t_SLP_rnd_dpDirichletSingular(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Dirichlet", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpDirichletSingular

  subroutine t_SLP_rnd_dpNeumannDirichlet(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) - vl(2)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpNeumannDirichlet

  subroutine t_SLP_rnd_dpNeumannNeumann(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) - vl(N - 1)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpNeumannNeumann

  subroutine t_SLP_rnd_dpNeumannMixed(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) + 1.0_dp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpNeumannMixed

  subroutine t_SLP_rnd_dpNeumannFree(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpNeumannFree

  subroutine t_SLP_rnd_dpNeumannSingular(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Neumann", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(2)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpNeumannSingular

  subroutine t_SLP_rnd_dpMixedDirichlet(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Dirichlet", &
                        mixing_param_a=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) - 1.0_dp*(vl(2) - vl(1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpMixedDirichlet

  subroutine t_SLP_rnd_dpMixedNeumann(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Neumann", &
                        mixing_param_a=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N) - vl(N - 1)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) - 1.0_dp*(vl(2) - vl(1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpMixedNeumann

  subroutine t_SLP_rnd_dpMixedMixed(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Mixed", &
                        mixing_param_a=1.0_dp, &
                        mixing_param_b=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - 1.0_dp*(vl(2) - vl(1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(N) + 1.0_dp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpMixedMixed

  subroutine t_SLP_rnd_dpMixedFree(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Free", &
                        mixing_param_a=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - 1.0_dp*(vl(2) - vl(1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpMixedFree

  subroutine t_SLP_rnd_dpMixedSingular(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Mixed", &
                        bound_cond_b="Singular", &
                        mixing_param_a=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - 1.0_dp*(vl(2) - vl(1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpMixedSingular

  subroutine t_SLP_rnd_dpFreeDirichlet(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpFreeDirichlet

  subroutine t_SLP_rnd_dpFreeNeumann(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N) - vl(N - 1)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpFreeNeumann

  subroutine t_SLP_rnd_dpFreeMixed(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N) + 1.0_dp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpFreeMixed

  subroutine t_SLP_rnd_dpFreeFree(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpFreeFree

  subroutine t_SLP_rnd_dpFreeSingular(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Free", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpFreeSingular

  subroutine t_SLP_rnd_dpSingularDirichlet(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Dirichlet", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpSingularDirichlet

  subroutine t_SLP_rnd_dpSingularNeumann(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Neumann", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N) - vl(N - 1)) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpSingularNeumann

  subroutine t_SLP_rnd_dpSingularMixed(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Mixed", &
                        mixing_param_b=1.0_dp, &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(N) + 1.0_dp*(vl(N) - vl(N - 1))/prb%spacing()) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpSingularMixed

  subroutine t_SLP_rnd_dpSingularFree(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Free", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpSingularFree

  subroutine t_SLP_rnd_dpSingularSingular(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Singular", &
                        bound_cond_b="Singular", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpSingularSingular

  subroutine t_SLP_rnd_spPeriodicPeriodic(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_i_sp, pi_sp
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: method_tolerance = 0.01_sp !1% max error or reroll.

    type(SLP_sp) :: prb
    real(sp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(sp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(sp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(sp), allocatable :: fsc(:)
    complex(sp) :: sum

    real(sp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_sp
    b = 1.0_sp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_sp
    update_amp_mx = 0.5_sp
    floor = 1.0_sp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          p(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_sp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_sp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_sp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          w(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_sp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_sp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_sp
          do i = 1, N
            x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, sp)/real(N - 1, sp)
          sum = cmplx_0_sp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_sp*pi_sp*cmplx_i_sp*real(j, sp)*x/abs(b - a))
          enddo
          q(i) = real(sum, sp)*(b - a)/real(N - 1, sp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_sp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, sp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Periodic", &
                        bound_cond_b="Periodic", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_sp*real(N - 3, sp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_sp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_sp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_sp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_sp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_sp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(N)) > tol_sp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) + vl(N) - (vl(2) + vl(N - 1))) > tol_sp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_sp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_spPeriodicPeriodic

  subroutine t_SLP_rnd_dpPeriodicPeriodic(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_i_dp, pi_dp
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: method_tolerance = 0.01_dp !1% max error or reroll.

    type(SLP_dp) :: prb
    real(dp) :: rndval, osz_amp_init, update_amp_mx, floor
    real(dp) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(dp), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(dp), allocatable :: fsc(:)
    complex(dp) :: sum

    real(dp) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0_dp
    b = 1.0_dp

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2_dp
    update_amp_mx = 0.5_dp
    floor = 1.0_dp

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx - Nmn)*rndval)

        allocate (p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            p(i) = p(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            p(i) = p(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + p(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          p(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i - 1) + update_amp_mx*(rndval - 0.5_dp)) >= floor) then
            w(i) = w(i - 1) + update_amp_mx*(rndval - 0.5_dp)
          else
            w(i) = w(i - 1) - update_amp_mx*(rndval - 0.5_dp)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + w(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          w(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval - 0.5_dp)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i - 1) + update_amp_mx*(rndval - 0.5_dp)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0_dp
          do i = 1, N
            x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
            fsc(j) = fsc(j) + q(i)*exp(-2.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
        enddo

        do i = 1, N
          x = a + (b - a)*real(i - 1, dp)/real(N - 1, dp)
          sum = cmplx_0_dp
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0_dp*pi_dp*cmplx_i_dp*real(j, dp)*x/abs(b - a))
          enddo
          q(i) = real(sum, dp)*(b - a)/real(N - 1, dp)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5_dp
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b - a)/real(N - 1, dp))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="Periodic", &
                        bound_cond_b="Periodic", &
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5_dp*real(N - 3, dp)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N - 6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j - 1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0_dp*prb%spacing()))) + &
              vl(j)*(q(j) - 2.0_dp*p(j)/(prb%spacing())**2) + &
              vl(j + 1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0_dp*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs - rhs) > tol_abserr_dp) then
          relerr = abs(lhs - rhs)/abs(rhs)
        else
          relerr = 0.0_dp
        endif

        !Check boundary conditions.
        if (abs(vl(1) - vl(N)) > tol_dp) then
          allocate (error)
          return
        endif
        if (abs(vl(1) + vl(N) - (vl(2) + vl(N - 1))) > tol_dp) then
          allocate (error)
          return
        endif

        deallocate (p, pp, q, w, fsc)

        if (relerr < method_tolerance) then
          flag = .false.
          exit iterations
        else
          write (unit=aux1, fmt="(F15.8)") 100.0_dp*relerr
          write (unit=aux2, fmt="(I0)") iter
          if (tb(isym)) then
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write (unit=stderr, fmt="(A)") &
              "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write (unit=aux2, fmt="(I0)") iter
      if (tb(isym)) then
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter > 1) write (unit=stderr, fmt="(A)") &
          "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate (error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd_dpPeriodicPeriodic

end module Obj_Rnd_Functionality
