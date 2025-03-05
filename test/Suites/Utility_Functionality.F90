module Utility_Functionality

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: new_unittest, unittest_type, error_type

  use SLP_kinds, only: sp, dp

  implicit none
  private

  public :: collect_Utility_Functionality

  real(sp), parameter :: tol_sp = 1.0E3_sp*epsilon(1.0_sp)
  real(dp), parameter :: tol_dp = 1.0E3_dp*epsilon(1.0_dp)

contains

  subroutine collect_Utility_Functionality(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                new_unittest("test against reference: 3x3 symmetric (sp) matrix.", test_3x3_sym_sp), &
                new_unittest("test against reference: 3x3 symmetric (dp) matrix.", test_3x3_sym_dp), &
                new_unittest("test against reference: 3x3 Hermitian (sp) matrix.", test_3x3_he_sp), &
                new_unittest("test against reference: 3x3 Hermitian (dp) matrix.", test_3x3_he_dp), &
                new_unittest("random test: NxN symmetric (sp) matrix.", test_nxn_rnd_dd_sym_sp), &
                new_unittest("random test: NxN symmetric (dp) matrix.", test_nxn_rnd_dd_sym_dp), &
                new_unittest("random test: NxN Hermitian (sp) matrix.", test_nxn_rnd_dd_he_sp), &
                new_unittest("random test: NxN Hermitian (dp) matrix.", test_nxn_rnd_dd_he_dp), &
                new_unittest("random test: metric setter.", test_set_metric) &
                ]

  end subroutine collect_Utility_Functionality

  subroutine test_3x3_sym_sp(error)
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    real(sp) :: M(3, 3), Me(3, 3), P(3, 3), D(3, 3), eig(3)
    integer :: i
    real(sp), parameter :: ref = -23.6859684_sp
    M(1, :) = [1.0_sp, 5.0_sp, -8.0_sp]
    M(2, :) = [5.0_sp, 3.0_sp, 16.0_sp]
    M(3, :) = [-8.0_sp, 16.0_sp, -9.0_sp]
    Me = 0.0_sp
    do concurrent(i=1:3)
      Me(i, i) = 1.0_sp
    enddo
    call dg_gen(M, Me, P, D, eig)
    if (abs(eig(1) - ref) > tol_sp) allocate (error)
  end subroutine test_3x3_sym_sp

  subroutine test_3x3_sym_dp(error)
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: M(3, 3), Me(3, 3), P(3, 3), D(3, 3), eig(3)
    integer :: i
    real(dp), parameter :: ref = -23.685967260551440_dp
    M(1, :) = [1.0_dp, 5.0_dp, -8.0_dp]
    M(2, :) = [5.0_dp, 3.0_dp, 16.0_dp]
    M(3, :) = [-8.0_dp, 16.0_dp, -9.0_dp]
    Me = 0.0_dp
    do concurrent(i=1:3)
      Me(i, i) = 1.0_dp
    enddo
    call dg_gen(M, Me, P, D, eig)
    if (abs(eig(1) - ref) > tol_dp) allocate (error)
  end subroutine test_3x3_sym_dp

  subroutine test_3x3_he_sp(error)
    use SLP_defs, only: cmplx_1_sp, cmplx_i_sp
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    complex(sp) :: M(3, 3), Me(3, 3), P(3, 3), D(3, 3)
    real(sp) :: eig(3)
    integer :: i
    real(sp), parameter :: ref = -21.2519016_sp
    M(1, :) = [1.0_sp*cmplx_1_sp, 3.0_sp*cmplx_1_sp + 2.0_sp*cmplx_i_sp, -6.0_sp*cmplx_1_sp + 5.0_sp*cmplx_i_sp]
    M(2, :) = [3.0_sp*cmplx_1_sp - 2.0_sp*cmplx_i_sp, 7.0_sp*cmplx_1_sp, 13.0_sp*cmplx_1_sp + 2.5_sp*cmplx_i_sp]
    M(3, :) = [-6.0_sp*cmplx_1_sp - 5.0_sp*cmplx_i_sp, 13.0_sp*cmplx_1_sp - 2.5_sp*cmplx_i_sp, -12.0_sp*cmplx_1_sp]
    Me = 0.0_sp
    do concurrent(i=1:3)
      Me(i, i) = 1.0_sp
    enddo
    call dg_gen(M, Me, P, D, eig)
    if (abs(eig(1) - ref) > tol_sp) allocate (error)
  end subroutine test_3x3_he_sp

  subroutine test_3x3_he_dp(error)
    use SLP_defs, only: cmplx_1_dp, cmplx_i_dp
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    complex(dp) :: M(3, 3), Me(3, 3), P(3, 3), D(3, 3)
    real(dp) :: eig(3)
    integer :: i
    real(dp), parameter :: ref = -21.251901096119539_dp
    M(1, :) = [1.0_dp*cmplx_1_dp, 3.0_dp*cmplx_1_dp + 2.0_dp*cmplx_i_dp, -6.0_dp*cmplx_1_dp + 5.0_dp*cmplx_i_dp]
    M(2, :) = [3.0_dp*cmplx_1_dp - 2.0_dp*cmplx_i_dp, 7.0_dp*cmplx_1_dp, 13.0_dp*cmplx_1_dp + 2.5_dp*cmplx_i_dp]
    M(3, :) = [-6.0_dp*cmplx_1_dp - 5.0_dp*cmplx_i_dp, 13.0_dp*cmplx_1_dp - 2.5_dp*cmplx_i_dp, -12.0_dp*cmplx_1_dp]
    Me = 0.0_dp
    do concurrent(i=1:3)
      Me(i, i) = 1.0_dp
    enddo
    call dg_gen(M, Me, P, D, eig)
    if (abs(eig(1) - ref) > tol_dp) allocate (error)
  end subroutine test_3x3_he_dp

  subroutine test_nxn_rnd_dd_sym_sp(error)
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    real(sp), allocatable :: M(:, :), Me(:, :), P(:, :), D(:, :), eig(:)
    real(sp) :: rndn, rndre, maxvl, rowsum
    integer :: N, Nmx
    integer :: i, j
    character(len=120) :: aux
    Nmx = 1000
    maxvl = 1.0_sp
    call random_number(rndn)
    N = nint(1.0_sp + real(Nmx - 1, sp)*rndn)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)"), "Matrix size = "//trim(adjustl(aux))//"."
    allocate (M(N, N), Me(N, N), P(N, N), D(N, N), eig(N))
    Me = 0.0_sp
    do i = 1, N
      Me(i, i) = 1.0_sp
      do j = 1, N
        call random_number(rndre)
        M(i, j) = (-0.5_sp + rndre)*2.0_sp*maxvl
      enddo
    enddo
    !Symmetrize.
    M = 0.5_sp*(M + transpose(M))
    !Make it diagonally dominant.
    do i = 1, N
      rowsum = 0.0_sp
      do j = 1, N
        if (j == i) cycle
        rowsum = rowsum + abs(M(i, j))
      enddo
      call random_number(rndre)
      M(i, i) = rndre*maxvl + rowsum
      call random_number(rndre)
      if (rndre < 0.5_sp) M(i, i) = -M(i, i)
    enddo
    call dg_gen(M, Me, P, D, eig)
    M = matmul(P, matmul(D, transpose(P))) - M
    if (any(abs(M) > 1.0E2_sp*tol_sp)) allocate (error)
    deallocate (M, Me, P, D, eig)
  end subroutine test_nxn_rnd_dd_sym_sp

  subroutine test_nxn_rnd_dd_sym_dp(error)
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    real(dp), allocatable :: M(:, :), Me(:, :), P(:, :), D(:, :), eig(:)
    real(dp) :: rndn, rndre, maxvl, rowsum
    integer :: N, Nmx
    integer :: i, j
    character(len=120) :: aux
    Nmx = 1000
    maxvl = 1.0_dp
    call random_number(rndn)
    N = nint(1.0_dp + real(Nmx - 1, dp)*rndn)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)"), "Matrix size = "//trim(adjustl(aux))//"."
    allocate (M(N, N), Me(N, N), P(N, N), D(N, N), eig(N))
    Me = 0.0_dp
    do i = 1, N
      Me(i, i) = 1.0_dp
      do j = 1, N
        call random_number(rndre)
        M(i, j) = (-0.5_dp + rndre)*2.0_dp*maxvl
      enddo
    enddo
    !Symmetrize.
    M = 0.5_dp*(M + transpose(M))
    !Make it diagonally dominant.
    do i = 1, N
      rowsum = 0.0_dp
      do j = 1, N
        if (j == i) cycle
        rowsum = rowsum + abs(M(i, j))
      enddo
      call random_number(rndre)
      M(i, i) = rndre*maxvl + rowsum
      call random_number(rndre)
      if (rndre < 0.5_dp) M(i, i) = -M(i, i)
    enddo
    call dg_gen(M, Me, P, D, eig)
    M = matmul(P, matmul(D, transpose(P))) - M
    if (any(abs(M) > 1.0E2_dp*tol_dp)) allocate (error)
    deallocate (M, Me, P, D, eig)
  end subroutine test_nxn_rnd_dd_sym_dp

  subroutine test_nxn_rnd_dd_he_sp(error)
    use SLP_defs, only: cmplx_0_sp, cmplx_1_sp, cmplx_i_sp
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    complex(sp), allocatable :: M(:, :), Me(:, :), P(:, :), D(:, :)
    real(sp), allocatable :: eig(:)
    real(sp) :: rndn, rndre, rndim, maxvl, rowsum
    integer :: N, Nmx
    integer :: i, j
    character(len=120) :: aux
    Nmx = 1000
    maxvl = 1.0_sp
    call random_number(rndn)
    N = nint(1.0_sp + real(Nmx - 1, sp)*rndn)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)"), "Matrix size = "//trim(adjustl(aux))//"."
    allocate (M(N, N), Me(N, N), P(N, N), D(N, N), eig(N))
    Me = cmplx_0_sp
    do i = 1, N
      Me(i, i) = cmplx_1_sp
      do j = 1, N
        call random_number(rndre)
        call random_number(rndim)
        M(i, j) = (-0.5_sp + rndre)*2.0_sp*maxvl*cmplx_1_sp + &
                  (-0.5_sp + rndim)*2.0_sp*maxvl*cmplx_i_sp
      enddo
    enddo
    !Make it Hermitian.
    M = 0.5_sp*(M + conjg(transpose(M)))
    !Make it diagonally dominant.
    do i = 1, N
      rowsum = cmplx_0_sp
      do j = 1, N
        if (j == i) cycle
        rowsum = rowsum + abs(M(i, j))*cmplx_1_sp
      enddo
      call random_number(rndre)
      M(i, i) = rndre*maxvl*cmplx_1_sp + rowsum
      call random_number(rndre)
      if (rndre < 0.5_sp) M(i, i) = -M(i, i)
    enddo
    call dg_gen(M, Me, P, D, eig)
    M = matmul(P, matmul(D, conjg(transpose(P)))) - M
    if (any(abs(M) > 1.0E2_sp*tol_sp)) allocate (error)
    deallocate (M, Me, P, D, eig)
  end subroutine test_nxn_rnd_dd_he_sp

  subroutine test_nxn_rnd_dd_he_dp(error)
    use SLP_defs, only: cmplx_0_dp, cmplx_1_dp, cmplx_i_dp
    use SLP_util, only: dg_gen
    type(error_type), allocatable, intent(out) :: error
    complex(dp), allocatable :: M(:, :), Me(:, :), P(:, :), D(:, :)
    real(dp), allocatable :: eig(:)
    real(dp) :: rndn, rndre, rndim, maxvl, rowsum
    integer :: N, Nmx
    integer :: i, j
    character(len=120) :: aux
    Nmx = 1000
    maxvl = 1.0_dp
    call random_number(rndn)
    N = nint(1.0_dp + real(Nmx - 1, dp)*rndn)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)"), "Matrix size = "//trim(adjustl(aux))//"."
    allocate (M(N, N), Me(N, N), P(N, N), D(N, N), eig(N))
    Me = cmplx_0_dp
    do i = 1, N
      Me(i, i) = cmplx_1_dp
      do j = 1, N
        call random_number(rndre)
        call random_number(rndim)
        M(i, j) = (-0.5_dp + rndre)*2.0_dp*maxvl*cmplx_1_dp + &
                  (-0.5_dp + rndim)*2.0_dp*maxvl*cmplx_i_dp
      enddo
    enddo
    !Make it Hermitian.
    M = 0.5_dp*(M + conjg(transpose(M)))
    !Make it diagonally dominant.
    do i = 1, N
      rowsum = cmplx_0_dp
      do j = 1, N
        if (j == i) cycle
        rowsum = rowsum + abs(M(i, j))*cmplx_1_dp
      enddo
      call random_number(rndre)
      M(i, i) = rndre*maxvl*cmplx_1_dp + rowsum
      call random_number(rndre)
      if (rndre < 0.5_dp) M(i, i) = -M(i, i)
    enddo
    call dg_gen(M, Me, P, D, eig)
    M = matmul(P, matmul(D, conjg(transpose(P)))) - M
    if (any(abs(M) > 1.0E2_dp*tol_dp)) allocate (error)
    deallocate (M, Me, P, D, eig)
  end subroutine test_nxn_rnd_dd_he_dp

  subroutine test_set_metric(error)
    use SLP_defs, only: cmplx_1_sp, cmplx_1_dp
    use SLP_util, only: set_metric
    type(error_type), allocatable, intent(out) :: error
    logical, parameter :: table(2) = [.false., .true.]
    real(sp), allocatable :: mr_sp(:)
    real(dp), allocatable :: mr_dp(:)
    complex(sp), allocatable :: mc_sp(:)
    complex(dp), allocatable :: mc_dp(:)
    real(sp) :: rndn_sp, rndre_sp, rndim_sp
    real(dp) :: rndn_dp, rndre_dp, rndim_dp
    integer :: i, j
    integer :: N, Nmx
    character(len=120) :: aux
    Nmx = 1000

    call random_number(rndn_sp)
    N = nint(1.0_sp + real(Nmx - 1, sp)*rndn_sp)
    allocate (mr_sp(N), mc_sp(N))
    mr_sp = 1.0_sp
    mc_sp = cmplx_1_sp

    call random_number(rndn_dp)
    N = nint(1.0_dp + real(Nmx - 1, dp)*rndn_dp)
    allocate (mr_dp(N), mc_dp(N))
    mr_dp = 1.0_dp
    mc_dp = cmplx_1_dp

    do i = 1, 2
      do j = 1, 2
        if (sum(set_metric(mr_sp, table(i), table(j))) > sum(mr_sp)) then
          allocate (error)
          return
        endif
        if (sum(set_metric(mr_dp, table(i), table(j))) > sum(mr_dp)) then
          allocate (error)
          return
        endif
        if (abs(sum(set_metric(mc_sp, table(i), table(j)))) > abs(sum(mc_sp))) then
          allocate (error)
          return
        endif
        if (abs(sum(set_metric(mc_dp, table(i), table(j)))) > abs(sum(mc_dp))) then
          allocate (error)
          return
        endif
      enddo
    enddo

    deallocate (mr_sp, mc_sp, mr_dp, mc_dp)
  end subroutine test_set_metric

end module Utility_Functionality
