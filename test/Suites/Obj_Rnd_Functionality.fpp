#:set types = ['real']                           #!Type.
#:set tsuffixes = ['']                           #!Suffix for each type, was ['_r'].
#:set tlst = list(zip(types, tsuffixes))
#:set precisions = ['sp', 'dp']
#:set psuffixes = ['_sp', '_dp']
#:set plst = list(zip(precisions, psuffixes))
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
#:for type, tsfx in tlst
#:for prec, psfx in plst
#:for boundcond_a in ['Dirichlet', 'Neumann', 'Mixed', 'Free', 'Singular']
#:for boundcond_b in ['Dirichlet', 'Neumann', 'Mixed', 'Free', 'Singular']
                  , new_unittest("random diferential equation check &
                  &${type}$ (${prec}$) SLP: ${boundcond_a}$ ${boundcond_b}$.", &
                  t_SLP_rnd${tsfx}$${psfx}$${boundcond_a}$${boundcond_b}$) &
#:endfor
#:endfor
#:endfor
#:endfor
#:for type, tsfx in tlst
#:for prec, psfx in plst
#:for boundcond_a in ['Periodic']
#:for boundcond_b in ['Periodic']
                  , new_unittest("random diferential equation check &
                  &${type}$ (${prec}$) SLP: ${boundcond_a}$ ${boundcond_b}$.", &
                  t_SLP_rnd${tsfx}$${psfx}$${boundcond_a}$${boundcond_b}$) &
#:endfor
#:endfor
#:endfor
#:endfor
                ]

  end subroutine collect_Obj_Rnd_Functionality

  subroutine test_basic(error)
    use SLP_obj, only: SLP_sp, SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb_sp
    type(SLP_dp) :: prb_dp
    if (prb_sp%initialized()) then
      allocate(error)
      return
    endif
    if (prb_dp%initialized()) then
      allocate(error)
      return
    endif
  end subroutine test_basic

#:for type, tsfx in tlst
#:for prec, psfx in plst
#:for boundcond_a in ['Dirichlet', 'Neumann', 'Mixed', 'Free', 'Singular']
#:for boundcond_b in ['Dirichlet', 'Neumann', 'Mixed', 'Free', 'Singular']

  subroutine t_SLP_rnd${tsfx}$${psfx}$${boundcond_a}$${boundcond_b}$(error)
    use SLP_defs, only: cmplx_0${psfx}$, cmplx_i${psfx}$, pi${psfx}$
    use SLP_obj, only: SLP${psfx}$
    type(error_type), allocatable, intent(out) :: error
    real(${prec}$), parameter :: method_tolerance = 0.01${psfx}$ !1% max error or reroll.

    type(SLP${tsfx}$${psfx}$) :: prb
    real(${prec}$) :: rndval, osz_amp_init, update_amp_mx, floor
    real(${prec}$) :: x, a, b, lhs, rhs
    integer :: N, Nmn, Nmx, fcmx, itermx
    integer :: i, j, iter
    real(${prec}$), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
    complex(${prec}$), allocatable :: fsc(:)
    complex(${prec}$) :: sum

    real(${prec}$) :: relerr
    logical :: flag
    character(len=120) :: aux1, aux2

    logical :: tb(2) = [.false., .true.]
    integer :: isym

    a = 0.0${psfx}$
    b = 1.0${psfx}$

    Nmn = 200
    Nmx = 400
    osz_amp_init = 0.2${psfx}$
    update_amp_mx = 0.5${psfx}$
    floor = 1.0${psfx}$

    fcmx = 15

    itermx = 10
    flag = .true.

    do isym = 1, 2

      iterations: do iter = 1, itermx !Up to itermx rerolls.

        call random_number(rndval)
        N = nint(Nmn + (Nmx-Nmn)*rndval)

        allocate(p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

        !Define a non negative random and noisy p.
        call random_number(rndval)
        p(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((p(i-1) + update_amp_mx*(rndval-0.5${psfx}$))>=floor) then
            p(i) = p(i-1) + update_amp_mx*(rndval-0.5${psfx}$)
          else
            p(i) = p(i-1) - update_amp_mx*(rndval-0.5${psfx}$)
          endif
        enddo

        !Compute its Fourier series components.
        do j = -fcmx, fcmx
          fsc(j) = cmplx_0${psfx}$
          do i = 1, N
            x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
            fsc(j) = fsc(j) + p(i)*exp(-2.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
          enddo
        enddo

        !Redefine p by considering only a handful of terms
        !and half of the frequency.
        !This will smooth out and randomize p (again).
        !If we did not employ half of the frequency, endpoints would
        !be periodic, squandering test cases.
        do i = 1, N
          x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
          sum = cmplx_0${psfx}$
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
          enddo
          p(i) = real(sum, ${prec}$)*(b-a)/real(N-1,${prec}$)
        enddo

        !Proceed similarly for w.
        call random_number(rndval)
        w(1) = floor + osz_amp_init*rndval
        do i = 2, N
          call random_number(rndval)
          if ((w(i-1) + update_amp_mx*(rndval-0.5${psfx}$))>=floor) then
            w(i) = w(i-1) + update_amp_mx*(rndval-0.5${psfx}$)
          else
            w(i) = w(i-1) - update_amp_mx*(rndval-0.5${psfx}$)
          endif
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0${psfx}$
          do i = 1, N
            x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
            fsc(j) = fsc(j) + w(i)*exp(-2.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
          enddo
        enddo

        do i = 1, N
          x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
          sum = cmplx_0${psfx}$
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
          enddo
          w(i) = real(sum, ${prec}$)*(b-a)/real(N-1,${prec}$)
        enddo

        !And for q, but allowing for negative values.
        call random_number(rndval)
        q(1) = osz_amp_init*(rndval-0.5${psfx}$)
        do i = 2, N
          call random_number(rndval)
          q(i) = q(i-1) + update_amp_mx*(rndval-0.5${psfx}$)
        enddo

        do j = -fcmx, fcmx
          fsc(j) = cmplx_0${psfx}$
          do i = 1, N
            x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
            fsc(j) = fsc(j) + q(i)*exp(-2.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
          enddo
        enddo

        do i = 1, N
          x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
          sum = cmplx_0${psfx}$
          do j = -fcmx, fcmx
            sum = sum + fsc(j)*exp(1.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
          enddo
          q(i) = real(sum, ${prec}$)*(b-a)/real(N-1,${prec}$)
        enddo

        pp(1) = (p(2) - p(1))
        do i = 2, N - 1
          pp(i) = (p(i + 1) - p(i - 1))*0.5${psfx}$
        enddo
        pp(N) = (p(N) - p(N - 1))
        pp = pp/((b-a)/real(N-1,${prec}$))

        call prb%define(name="Test", &
                        a=a, b=b, N=n, &
                        p=p, pp=pp, q=q, w=w, &
                        bound_cond_a="${boundcond_a}$", &
                        bound_cond_b="${boundcond_b}$", &
    #:if boundcond_a == 'Mixed'
                        mixing_param_a=1.0${psfx}$, &
    #:endif
    #:if boundcond_b == 'Mixed'
                        mixing_param_b=1.0${psfx}$, &
    #:endif
                        enforce_self_adjoint=tb(isym), &
                        silent=.true.)
        call prb%solve(silent=.true.)
        !Select the ith solution.
        call random_number(rndval)
        i = nint(1 + 0.5${psfx}$*real(N-3, ${prec}$)*rndval)
        vl = prb%eivec(i); eig = prb%eig(i)

        !Select the jth position.
        call random_number(rndval)
        j = nint(3 + (N-6)*rndval)
        !Check wether the differential equation holds.
        lhs = vl(j-1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0${psfx}$*prb%spacing()))) + &
              vl(j)*(q(j)-2.0${psfx}$*p(j)/(prb%spacing())**2) + &
              vl(j+1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0${psfx}$*prb%spacing())))
        rhs = eig*w(j)*vl(j)

        !Calculate relative error. If the
        !absolute error is smaller than tolerance,
        !we assume no error.
        if (abs(lhs-rhs)>tol_abserr${psfx}$) then
          relerr = abs(lhs-rhs)/abs(rhs)
        else
          relerr = 0.0${psfx}$
        endif

        !Check boundary conditions.
#:if boundcond_a == 'Dirichlet'
        if (abs(vl(1))>tol${psfx}$) then
          allocate(error)
          return
        endif
#:endif
#:if boundcond_b == 'Dirichlet'
        if (abs(vl(N))>tol${psfx}$) then
          allocate(error)
          return
        endif
#:endif
#:if boundcond_a == 'Neumann'
        if (abs(vl(1)-vl(2))>tol${psfx}$) then
          allocate(error)
          return
        endif
#:endif
#:if boundcond_b == 'Neumann'
        if (abs(vl(N)-vl(N-1))>tol${psfx}$) then
          allocate(error)
          return
        endif
#:endif
#:if boundcond_a == 'Mixed'
        if (abs(vl(1)-1.0${psfx}$*(vl(2)-vl(1))/prb%spacing())>tol${psfx}$) then
          allocate(error)
          return
        endif
#:endif
#:if boundcond_b == 'Mixed'
        if (abs(vl(N)+1.0${psfx}$*(vl(N)-vl(N-1))/prb%spacing())>tol${psfx}$) then
          allocate(error)
          return
        endif
#:endif

        deallocate(p, pp, q, w, fsc)

        if (relerr<method_tolerance) then
          flag = .false.
          exit iterations
        else
          write(unit=aux1,fmt="(F15.8)") 100.0${psfx}$*relerr
          write(unit=aux2,fmt="(I0)") iter
          if (tb(isym)) then
            write(unit=stderr, fmt="(A)") &
            "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
          else
            write(unit=stderr, fmt="(A)") &
            "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
          endif
        endif

      enddo iterations

      write(unit=aux2,fmt="(I0)") iter
      if (tb(isym)) then
        if (iter>1) write(unit=stderr, fmt="(A)") &
        "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
      else
        if (iter>1) write(unit=stderr, fmt="(A)") &
        "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
      endif
      if (flag) then
        allocate(error)
        return
      endif

    enddo

  end subroutine t_SLP_rnd${tsfx}$${psfx}$${boundcond_a}$${boundcond_b}$
#:endfor
#:endfor
#:endfor
#:endfor
#:for type, tsfx in tlst
#:for prec, psfx in plst
#:for boundcond_a in ['Periodic']
#:for boundcond_b in ['Periodic']

subroutine t_SLP_rnd${tsfx}$${psfx}$${boundcond_a}$${boundcond_b}$(error)
  use SLP_defs, only: cmplx_0${psfx}$, cmplx_i${psfx}$, pi${psfx}$
  use SLP_obj, only: SLP${psfx}$
  type(error_type), allocatable, intent(out) :: error
  real(${prec}$), parameter :: method_tolerance = 0.01${psfx}$ !1% max error or reroll.

  type(SLP${tsfx}$${psfx}$) :: prb
  real(${prec}$) :: rndval, osz_amp_init, update_amp_mx, floor
  real(${prec}$) :: x, a, b, lhs, rhs
  integer :: N, Nmn, Nmx, fcmx, itermx
  integer :: i, j, iter
  real(${prec}$), allocatable :: p(:), pp(:), q(:), w(:), vl(:), eig
  complex(${prec}$), allocatable :: fsc(:)
  complex(${prec}$) :: sum

  real(${prec}$) :: relerr
  logical :: flag
  character(len=120) :: aux1, aux2

  logical :: tb(2) = [.false., .true.]
  integer :: isym

  a = 0.0${psfx}$
  b = 1.0${psfx}$

  Nmn = 200
  Nmx = 400
  osz_amp_init = 0.2${psfx}$
  update_amp_mx = 0.5${psfx}$
  floor = 1.0${psfx}$

  fcmx = 15

  itermx = 10
  flag = .true.

  do isym = 1, 2

    iterations: do iter = 1, itermx !Up to itermx rerolls.

      call random_number(rndval)
      N = nint(Nmn + (Nmx-Nmn)*rndval)

      allocate(p(N), pp(N), q(N), w(N), fsc(-fcmx:fcmx))

      !Define a non negative random and noisy p.
      call random_number(rndval)
      p(1) = floor + osz_amp_init*rndval
      do i = 2, N
        call random_number(rndval)
        if ((p(i-1) + update_amp_mx*(rndval-0.5${psfx}$))>=floor) then
          p(i) = p(i-1) + update_amp_mx*(rndval-0.5${psfx}$)
        else
          p(i) = p(i-1) - update_amp_mx*(rndval-0.5${psfx}$)
        endif
      enddo

      !Compute its Fourier series components.
      do j = -fcmx, fcmx
        fsc(j) = cmplx_0${psfx}$
        do i = 1, N
          x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
          fsc(j) = fsc(j) + p(i)*exp(-2.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
        enddo
      enddo

      !Redefine p by considering only a handful of terms
      !and half of the frequency.
      !This will smooth out and randomize p (again).
      !If we did not employ half of the frequency, endpoints would
      !be periodic, squandering test cases.
      do i = 1, N
        x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
        sum = cmplx_0${psfx}$
        do j = -fcmx, fcmx
          sum = sum + fsc(j)*exp(1.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
        enddo
        p(i) = real(sum, ${prec}$)*(b-a)/real(N-1,${prec}$)
      enddo

      !Proceed similarly for w.
      call random_number(rndval)
      w(1) = floor + osz_amp_init*rndval
      do i = 2, N
        call random_number(rndval)
        if ((w(i-1) + update_amp_mx*(rndval-0.5${psfx}$))>=floor) then
          w(i) = w(i-1) + update_amp_mx*(rndval-0.5${psfx}$)
        else
          w(i) = w(i-1) - update_amp_mx*(rndval-0.5${psfx}$)
        endif
      enddo

      do j = -fcmx, fcmx
        fsc(j) = cmplx_0${psfx}$
        do i = 1, N
          x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
          fsc(j) = fsc(j) + w(i)*exp(-2.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
        enddo
      enddo

      do i = 1, N
        x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
        sum = cmplx_0${psfx}$
        do j = -fcmx, fcmx
          sum = sum + fsc(j)*exp(1.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
        enddo
        w(i) = real(sum, ${prec}$)*(b-a)/real(N-1,${prec}$)
      enddo

      !And for q, but allowing for negative values.
      call random_number(rndval)
      q(1) = osz_amp_init*(rndval-0.5${psfx}$)
      do i = 2, N
        call random_number(rndval)
        q(i) = q(i-1) + update_amp_mx*(rndval-0.5${psfx}$)
      enddo

      do j = -fcmx, fcmx
        fsc(j) = cmplx_0${psfx}$
        do i = 1, N
          x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
          fsc(j) = fsc(j) + q(i)*exp(-2.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
        enddo
      enddo

      do i = 1, N
        x = a + (b-a)*real(i-1,${prec}$)/real(N-1,${prec}$)
        sum = cmplx_0${psfx}$
        do j = -fcmx, fcmx
          sum = sum + fsc(j)*exp(1.0${psfx}$*pi${psfx}$*cmplx_i${psfx}$*real(j, ${prec}$)*x/abs(b-a))
        enddo
        q(i) = real(sum, ${prec}$)*(b-a)/real(N-1,${prec}$)
      enddo

      pp(1) = (p(2) - p(1))
      do i = 2, N - 1
        pp(i) = (p(i + 1) - p(i - 1))*0.5${psfx}$
      enddo
      pp(N) = (p(N) - p(N - 1))
      pp = pp/((b-a)/real(N-1,${prec}$))

      call prb%define(name="Test", &
                      a=a, b=b, N=n, &
                      p=p, pp=pp, q=q, w=w, &
                      bound_cond_a="${boundcond_a}$", &
                      bound_cond_b="${boundcond_b}$", &
                      enforce_self_adjoint=tb(isym), &
                      silent=.true.)
      call prb%solve(silent=.true.)
      !Select the ith solution.
      call random_number(rndval)
      i = nint(1 + 0.5${psfx}$*real(N-3, ${prec}$)*rndval)
      vl = prb%eivec(i); eig = prb%eig(i)

      !Select the jth position.
      call random_number(rndval)
      j = nint(3 + (N-6)*rndval)
      !Check wether the differential equation holds.
      lhs = vl(j-1)*((p(j)/(prb%spacing())**2) - (pp(j)/(2.0${psfx}$*prb%spacing()))) + &
            vl(j)*(q(j)-2.0${psfx}$*p(j)/(prb%spacing())**2) + &
            vl(j+1)*((p(j)/(prb%spacing())**2) + (pp(j)/(2.0${psfx}$*prb%spacing())))
      rhs = eig*w(j)*vl(j)

      !Calculate relative error. If the
      !absolute error is smaller than tolerance,
      !we assume no error.
      if (abs(lhs-rhs)>tol_abserr${psfx}$) then
        relerr = abs(lhs-rhs)/abs(rhs)
      else
        relerr = 0.0${psfx}$
      endif

      !Check boundary conditions.
#:if boundcond_a == 'Periodic'
      if (abs(vl(1)-vl(N))>tol${psfx}$) then
        allocate(error)
        return
      endif
      if (abs(vl(1)+vl(N)-(vl(2)+vl(N-1)))>tol${psfx}$) then
        allocate(error)
        return
      endif
#:endif

      deallocate(p, pp, q, w, fsc)

      if (relerr<method_tolerance) then
        flag = .false.
        exit iterations
      else
        write(unit=aux1,fmt="(F15.8)") 100.0${psfx}$*relerr
        write(unit=aux2,fmt="(I0)") iter
        if (tb(isym)) then
          write(unit=stderr, fmt="(A)") &
          "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (Imposed symmetrization)."
        else
          write(unit=stderr, fmt="(A)") &
          "   -Iteration #"//trim(adjustl(aux2))//". Relative error = "//trim(adjustl(aux1))//"%. (No symmetrization)."
        endif
      endif

    enddo iterations

    write(unit=aux2,fmt="(I0)") iter
    if (tb(isym)) then
      if (iter>1) write(unit=stderr, fmt="(A)") &
      "   -Exiting after "//trim(adjustl(aux2))//" iterations. (Imposed symmetrization)."
    else
      if (iter>1) write(unit=stderr, fmt="(A)") &
      "   -Exiting after "//trim(adjustl(aux2))//" iterations. (No symmetrization)."
    endif
    if (flag) then
      allocate(error)
      return
    endif

  enddo

end subroutine t_SLP_rnd${tsfx}$${psfx}$${boundcond_a}$${boundcond_b}$
#:endfor
#:endfor
#:endfor
#:endfor

end module Obj_Rnd_Functionality
