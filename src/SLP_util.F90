module SLP_util

  use SLP_kinds, only: sp, dp
  use SLP_defs, only: cmplx_0_sp, cmplx_1_sp, &
    cmplx_0_dp, cmplx_1_dp

  implicit none
  private

  interface dg_gen
    !Eigenvectors stored as v_i = P(:, i), normalized as P^{-1}*metric*P = Id.
    module procedure :: dg_sym_gen_sp
    module procedure :: dg_sym_gen_dp
    module procedure :: dg_he_gen_sp
    module procedure :: dg_he_gen_dp
  end interface

  interface set_metric
    module procedure :: sm_r_sp
    module procedure :: sm_r_dp
    module procedure :: sm_c_sp
    module procedure :: sm_c_dp
  end interface

  public :: dg_gen
  public :: set_metric

contains

  subroutine dg_sym_gen_sp(matrix, metric, P, D, eig)
    !===========================================!
    !                                           !
    !  Given a symmetric (sp) matrix, computes  !
    !  the elements for its diagonalization     !
    !  with respect to a metric, P, D:          !
    !  matrix = metric*P*D*P^dagger, where      !
    !  D_nm = delta_nm eig_n.                   !
    !                                           !
    !===========================================!

    real(sp), intent(in)  :: matrix(:, :), metric(:, :)
    real(sp), intent(out) :: P(size(matrix(:, 1)), size(matrix(1, :)))
    real(sp), intent(out) :: D(size(matrix(:, 1)), size(matrix(1, :)))
    real(sp), intent(out) :: eig(size(matrix(:, 1)))

    real(sp) :: metric_copy(size(metric(:, 1)), size(metric(1, :)))

    real(sp), allocatable    :: work(:)
    integer, allocatable     :: iwork(:)
    integer                  :: dim, info, &
                                lwork, liwork, &
                                i
    external                 :: ssygvd

    character(len=1024) :: errormsg

    if (size(matrix(:, 1)) /= size(matrix(1, :))) error stop &
      "Matrix to diagonalize is not square."
    if (size(metric(:, 1)) /= size(metric(1, :))) error stop &
      "Metric is not square."
    if (size(matrix(:, 1)) /= size(metric(:, 1))) error stop &
      "Matrix and metric are different sizes."
    dim = size(matrix(:, 1))

    !Initialization.
    P = matrix
    metric_copy = metric

    !Query optimal workspace.
    lwork = -1; liwork = -1
    allocate (work(1), iwork(1))
    call ssygvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, iwork, liwork, info)
    lwork = nint(real(work(1), dp))
    liwork = nint(real(iwork(1), dp))
    deallocate (work, iwork)

    !Calculation.
    allocate (work(lwork), iwork(liwork))
    call ssygvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, iwork, liwork, info)
    deallocate (work, iwork)

    if (info /= 0) then
      write (errormsg, "(i20)") info
      errormsg = "Subroutine ssygvd failed with info = "//trim(adjustl(errormsg))//"."
      error stop trim(errormsg)
    endif

    D = 0.0_sp
    do i = 1, dim
      D(i, i) = eig(i)
    enddo

  end subroutine dg_sym_gen_sp

  subroutine dg_sym_gen_dp(matrix, metric, P, D, eig)
    !===========================================!
    !                                           !
    !  Given a symmetric (dp) matrix, computes  !
    !  the elements for its diagonalization     !
    !  with respect to a metric, P, D:          !
    !  matrix = metric*P*D*P^dagger, where      !
    !  D_nm = delta_nm eig_n.                   !
    !                                           !
    !===========================================!

    real(dp), intent(in)  :: matrix(:, :), metric(:, :)
    real(dp), intent(out) :: P(size(matrix(:, 1)), size(matrix(1, :)))
    real(dp), intent(out) :: D(size(matrix(:, 1)), size(matrix(1, :)))
    real(dp), intent(out) :: eig(size(matrix(:, 1)))

    real(dp) :: metric_copy(size(metric(:, 1)), size(metric(1, :)))

    real(dp), allocatable    :: work(:)
    integer, allocatable     :: iwork(:)
    integer                  :: dim, info, &
                                lwork, liwork, &
                                i
    external                 :: dsygvd

    character(len=1024) :: errormsg

    if (size(matrix(:, 1)) /= size(matrix(1, :))) error stop &
      "Matrix to diagonalize is not square."
    if (size(metric(:, 1)) /= size(metric(1, :))) error stop &
      "Metric is not square."
    if (size(matrix(:, 1)) /= size(metric(:, 1))) error stop &
      "Matrix and metric are different sizes."
    dim = size(matrix(:, 1))

    !Initialization.
    P = matrix
    metric_copy = metric

    !Query optimal workspace.
    lwork = -1; liwork = -1
    allocate (work(1), iwork(1))
    call dsygvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, iwork, liwork, info)
    lwork = nint(real(work(1), dp))
    liwork = nint(real(iwork(1), dp))
    deallocate (work, iwork)

    !Calculation.
    allocate (work(lwork), iwork(liwork))
    call dsygvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, iwork, liwork, info)
    deallocate (work, iwork)

    if (info /= 0) then
      write (errormsg, "(i20)") info
      errormsg = "Subroutine dsygvd failed with info = "//trim(adjustl(errormsg))//"."
      error stop trim(errormsg)
    endif

    D = 0.0_dp
    do i = 1, dim
      D(i, i) = eig(i)
    enddo

  end subroutine dg_sym_gen_dp

  subroutine dg_he_gen_sp(matrix, metric, P, D, eig)
    !===========================================!
    !                                           !
    !  Given a Hermitian (sp) matrix, computes  !
    !  the elements for its diagonalization     !
    !  with respect to a metric, P, D:          !
    !  matrix = metric*P*D*P^dagger, where      !
    !  D_nm = delta_nm eig_n for eig_n real.    !
    !                                           !
    !===========================================!

    complex(sp), intent(in)  :: matrix(:, :), metric(:, :)
    complex(sp), intent(out) :: P(size(matrix(:, 1)), size(matrix(1, :)))
    complex(sp), intent(out) :: D(size(matrix(:, 1)), size(matrix(1, :)))
    real(sp), intent(out)    :: eig(size(matrix(:, 1)))

    complex(sp) :: metric_copy(size(metric(:, 1)), size(metric(1, :)))

    complex(sp), allocatable :: work(:)
    real(sp), allocatable    :: rwork(:)
    integer, allocatable     :: iwork(:)
    integer                  :: dim, info, &
                                lwork, lrwork, liwork, &
                                i
    external                 :: chegvd

    character(len=1024) :: errormsg

    if (size(matrix(:, 1)) /= size(matrix(1, :))) error stop &
      "Matrix to diagonalize is not square."
    if (size(metric(:, 1)) /= size(metric(1, :))) error stop &
      "Metric is not square."
    if (size(matrix(:, 1)) /= size(metric(:, 1))) error stop &
      "Matrix and metric are different sizes."
    dim = size(matrix(:, 1))

    !Initialization.
    P = matrix
    metric_copy = metric

    !Query optimal workspace.
    lwork = -1; lrwork = -1; liwork = -1
    allocate (work(1), rwork(1), iwork(1))
    call chegvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, rwork, lrwork, iwork, liwork, info)
    lwork = nint(real(work(1), dp))
    lrwork = nint(real(rwork(1), dp))
    liwork = nint(real(iwork(1), dp))
    deallocate (work, rwork, iwork)

    !Calculation.
    allocate (work(lwork), rwork(lrwork), iwork(liwork))
    call chegvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, rwork, lrwork, iwork, liwork, info)
    deallocate (work, rwork, iwork)

    if (info /= 0) then
      write (errormsg, "(i20)") info
      errormsg = "Subroutine chegvd failed with info = "//trim(adjustl(errormsg))//"."
      error stop trim(errormsg)
    endif

    D = cmplx_0_sp
    do i = 1, dim
      D(i, i) = cmplx(eig(i), 0.0_sp, sp)
    enddo

  end subroutine dg_he_gen_sp

  subroutine dg_he_gen_dp(matrix, metric, P, D, eig)
    !===========================================!
    !                                           !
    !  Given a Hermitian (dp) matrix, computes  !
    !  the elements for its diagonalization     !
    !  with respect to a metric, P, D:          !
    !  matrix = metric*P*D*P^dagger, where      !
    !  D_nm = delta_nm eig_n for eig_n real.    !
    !                                           !
    !===========================================!

    complex(dp), intent(in)  :: matrix(:, :), metric(:, :)
    complex(dp), intent(out) :: P(size(matrix(:, 1)), size(matrix(1, :)))
    complex(dp), intent(out) :: D(size(matrix(:, 1)), size(matrix(1, :)))
    real(dp), intent(out)    :: eig(size(matrix(:, 1)))

    complex(dp) :: metric_copy(size(metric(:, 1)), size(metric(1, :)))

    complex(dp), allocatable :: work(:)
    real(dp), allocatable    :: rwork(:)
    integer, allocatable     :: iwork(:)
    integer                  :: dim, info, &
                                lwork, lrwork, liwork, &
                                i
    external                 :: zhegvd

    character(len=1024) :: errormsg

    if (size(matrix(:, 1)) /= size(matrix(1, :))) error stop &
      "Matrix to diagonalize is not square."
    if (size(metric(:, 1)) /= size(metric(1, :))) error stop &
      "Metric is not square."
    if (size(matrix(:, 1)) /= size(metric(:, 1))) error stop &
      "Matrix and metric are different sizes."
    dim = size(matrix(:, 1))

    !Initialization.
    P = matrix
    metric_copy = metric

    !Query optimal workspace.
    lwork = -1; lrwork = -1; liwork = -1
    allocate (work(1), rwork(1), iwork(1))
    call zhegvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, rwork, lrwork, iwork, liwork, info)
    lwork = nint(real(work(1), dp))
    lrwork = nint(real(rwork(1), dp))
    liwork = nint(real(iwork(1), dp))
    deallocate (work, rwork, iwork)

    !Calculation.
    allocate (work(lwork), rwork(lrwork), iwork(liwork))
    call zhegvd(1, 'V', 'U', dim, P, dim, metric_copy, dim, eig, &
                work, lwork, rwork, lrwork, iwork, liwork, info)
    deallocate (work, rwork, iwork)

    if (info /= 0) then
      write (errormsg, "(i20)") info
      errormsg = "Subroutine zhegvd failed with info = "//trim(adjustl(errormsg))//"."
      error stop trim(errormsg)
    endif

    D = cmplx_0_dp
    do i = 1, dim
      D(i, i) = cmplx(eig(i), 0.0_dp, dp)
    enddo

  end subroutine dg_he_gen_dp

  function sm_r_sp(mass, bound_a, bound_b) result(u)
    !Parse the mass function to a diagonal matrix
    !while allowing to set boundary conditions.
    real(sp), intent(in) :: mass(:)
    logical, intent(in) :: bound_a, bound_b
    real(sp) :: u(size(mass), size(mass))

    integer :: i

    u = 0.0_sp
    do concurrent(i=1:size(mass))
      u(i, i) = mass(i)
    enddo

    !Do we impose boundary conditions at start and finish?
    !If so, set the corresponding entry to (numerical) zero.
    if (bound_a) u(1, 1) = epsilon(1.0_sp)
    if (bound_b) u(size(mass), size(mass)) = epsilon(1.0_sp)

  end function sm_r_sp

  function sm_r_dp(mass, bound_a, bound_b) result(u)
    !Parse the mass function to a diagonal matrix
    !while allowing to set boundary conditions.
    real(dp), intent(in) :: mass(:)
    logical, intent(in) :: bound_a, bound_b
    real(dp) :: u(size(mass), size(mass))

    integer :: i

    u = 0.0_dp
    do concurrent(i=1:size(mass))
      u(i, i) = mass(i)
    enddo

    !Do we impose boundary conditions at start and finish?
    !If so, set the corresponding entry to (numerical) zero.
    if (bound_a) u(1, 1) = epsilon(1.0_dp)
    if (bound_b) u(size(mass), size(mass)) = epsilon(1.0_dp)

  end function sm_r_dp

  function sm_c_sp(mass, bound_a, bound_b) result(u)
    !Parse the mass function to a diagonal matrix
    !while allowing to set boundary conditions.
    complex(sp), intent(in) :: mass(:)
    logical, intent(in) :: bound_a, bound_b
    complex(sp) :: u(size(mass), size(mass))

    integer :: i

    u = cmplx_0_sp
    do concurrent(i=1:size(mass))
      u(i, i) = mass(i)
    enddo

    !Do we impose boundary conditions at start and finish?
    !If so, set the corresponding entry to (numerical) zero.
    if (bound_a) u(1, 1) = epsilon(1.0_sp)*cmplx_1_sp
    if (bound_b) u(size(mass), size(mass)) = epsilon(1.0_sp)*cmplx_1_sp

  end function sm_c_sp

  function sm_c_dp(mass, bound_a, bound_b) result(u)
    !Parse the mass function to a diagonal matrix
    !while allowing to set boundary conditions.
    complex(dp), intent(in) :: mass(:)
    logical, intent(in) :: bound_a, bound_b
    complex(dp) :: u(size(mass), size(mass))

    integer :: i

    u = cmplx_0_dp
    do concurrent(i=1:size(mass))
      u(i, i) = mass(i)
    enddo

    !Do we impose boundary conditions at start and finish?
    !If so, set the corresponding entry to (numerical) zero.
    if (bound_a) u(1, 1) = epsilon(1.0_dp)*cmplx_1_dp
    if (bound_b) u(size(mass), size(mass)) = epsilon(1.0_dp)*cmplx_1_dp

  end function sm_c_dp

end module SLP_util
