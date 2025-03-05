module Auxiliary_Functionality

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: new_unittest, unittest_type, error_type

  use SLP_kinds, only: sp, dp

  implicit none
  private

  public :: collect_Auxiliary_Functionality

contains

  subroutine collect_Auxiliary_Functionality(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                new_unittest("boundary condition linter test.", test_boundary_condition_linter), &
                new_unittest("get shift to move first N elements to top: fixed (sp) arrays.", test_shift_on_fixed_sp_arrays), &
                new_unittest("get shift to move first N elements to top: fixed (dp) arrays.", test_shift_on_fixed_dp_arrays), &
                new_unittest("get shift to move first N elements to top: random (sp) arrays.", test_shift_on_random_sp_arrays), &
                new_unittest("get shift to move first N elements to top: random (dp) arrays.", test_shift_on_random_dp_arrays) &
                ]

  end subroutine collect_Auxiliary_Functionality

  subroutine test_boundary_condition_linter(error)
    use SLP_auxiliary, only: boundary_condition_linter
    type(error_type), allocatable, intent(out) :: error
    integer :: id
    logical :: bcond
    call boundary_condition_linter("Dirichlet", .true., bcond, id)
    if (.not. bcond) then
      allocate (error)
      return
    endif
    if (id /= 1) then
      allocate (error)
      return
    endif
    call boundary_condition_linter("Neumann", .true., bcond, id)
    if (.not. bcond) then
      allocate (error)
      return
    endif
    if (id /= 2) then
      allocate (error)
      return
    endif
    call boundary_condition_linter("Mixed", .true., bcond, id)
    if (.not. bcond) then
      allocate (error)
      return
    endif
    if (id /= 3) then
      allocate (error)
      return
    endif
    call boundary_condition_linter("Periodic", .true., bcond, id)
    if (.not. bcond) then
      allocate (error)
      return
    endif
    if (id /= 4) then
      allocate (error)
      return
    endif
    call boundary_condition_linter("Free", .true., bcond, id)
    if (bcond) then
      allocate (error)
      return
    endif
    if (id /= 0) then
      allocate (error)
      return
    endif
  end subroutine test_boundary_condition_linter

  subroutine test_shift_on_fixed_sp_arrays(error)
    use SLP_auxiliary, only: cshift_N_largest_to_top
    type(error_type), allocatable, intent(out) :: error
    real(sp) :: array(5)
    array = [-50, -40, 10, 30, 75]*1.0_sp
    if (cshift_N_largest_to_top(1, array) /= 0) then
      allocate (error)
      return
    endif
    if (cshift_N_largest_to_top(2, array) /= 1) then
      allocate (error)
      return
    endif
    if (cshift_N_largest_to_top(3, array) /= 2) then
      allocate (error)
      return
    endif
    if (cshift_N_largest_to_top(4, array) /= 2) then
      allocate (error)
      return
    endif
  end subroutine test_shift_on_fixed_sp_arrays

  subroutine test_shift_on_fixed_dp_arrays(error)
    use SLP_auxiliary, only: cshift_N_largest_to_top
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: array(5)
    array = [-50, -40, 10, 30, 75]*1.0_dp
    if (cshift_N_largest_to_top(1, array) /= 0) then
      allocate (error)
      return
    endif
    if (cshift_N_largest_to_top(2, array) /= 1) then
      allocate (error)
      return
    endif
    if (cshift_N_largest_to_top(3, array) /= 2) then
      allocate (error)
      return
    endif
    if (cshift_N_largest_to_top(4, array) /= 2) then
      allocate (error)
      return
    endif
  end subroutine test_shift_on_fixed_dp_arrays

  subroutine test_shift_on_random_sp_arrays(error)
    use SLP_auxiliary, only: cshift_N_largest_to_top
    type(error_type), allocatable, intent(out) :: error
    real(sp), allocatable :: array(:)
    real(sp) :: rndvl, maxabs_amplitude
    integer :: maxN, minN, N, i, j, Nvalues, shift
    character(len=120) :: aux
    maxabs_amplitude = 10.0_sp
    minN = 3; maxN = 10000

    !Define random sized array with random numbers in
    !ascending order.
    call random_number(rndvl)
    N = nint(minN + (maxN - minN)*rndvl)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)"), "Array size = "//trim(adjustl(aux))//"."
    allocate (array(N))
    call random_number(rndvl)
    array(1) = rndvl*maxabs_amplitude
    do i = 2, N
      call random_number(rndvl)
      array(i) = array(i - 1) + rndvl*maxabs_amplitude
    enddo
    array = array - (sum(array)/real(N, sp))

    !Set a random Nvalues and get the left circular shift
    !required to send the largest Nvalues elements of
    !"array" to the top.
    call random_number(rndvl)
    Nvalues = nint(1.0_sp + real(N - 3, sp)*rndvl)
    write (aux, fmt="(I0)") Nvalues
    write (stderr, fmt="(A)"), "Requested # of values to top = "//trim(adjustl(aux))//"."

    shift = cshift_N_largest_to_top(Nvalues, array)
    write (aux, fmt="(I0)") shift
    write (stderr, fmt="(A)"), "Calculated shift = "//trim(adjustl(aux))//"."

    array = cshift(array, shift)

    !The first (N-Nvalues) elements must be in
    !ascending order.
    do i = 2, N - Nvalues
      if (array(i) < array(i - 1)) then
        allocate (error)
        return
      endif
    enddo

    !The last (Nvalues) elements must be larger
    !than the first (N-Nvalues) elements in
    !order of magnitude.
    do i = N - Nvalues + 1, N
      do j = 1, N - Nvalues
        if (abs(array(i)) < abs(array(j))) then
          allocate (error)
          return
        endif
      enddo
    enddo

    deallocate (array)
  end subroutine test_shift_on_random_sp_arrays

  subroutine test_shift_on_random_dp_arrays(error)
    use SLP_auxiliary, only: cshift_N_largest_to_top
    type(error_type), allocatable, intent(out) :: error
    real(dp), allocatable :: array(:)
    real(dp) :: rndvl, maxabs_amplitude
    integer :: maxN, minN, N, i, j, Nvalues, shift
    character(len=120) :: aux
    maxabs_amplitude = 10.0_dp
    minN = 3; maxN = 10000

    !Define random sized array with random numbers in
    !ascending order.
    call random_number(rndvl)
    N = nint(minN + (maxN - minN)*rndvl)
    write (aux, fmt="(I0)") N
    write (stderr, fmt="(A)"), "Array size = "//trim(adjustl(aux))//"."
    allocate (array(N))
    call random_number(rndvl)
    array(1) = rndvl*maxabs_amplitude
    do i = 2, N
      call random_number(rndvl)
      array(i) = array(i - 1) + rndvl*maxabs_amplitude
    enddo
    array = array - (sum(array)/real(N, dp))

    !Set a random Nvalues and get the left circular shift
    !required to send the largest Nvalues elements of
    !"array" to the top.
    call random_number(rndvl)
    Nvalues = nint(1.0_dp + real(N - 3, dp)*rndvl)
    write (aux, fmt="(I0)") Nvalues
    write (stderr, fmt="(A)"), "Requested # of values to top = "//trim(adjustl(aux))//"."

    shift = cshift_N_largest_to_top(Nvalues, array)
    write (aux, fmt="(I0)") shift
    write (stderr, fmt="(A)"), "Calculated shift = "//trim(adjustl(aux))//"."

    array = cshift(array, shift)

    !The first (N-Nvalues) elements must be in
    !ascending order.
    do i = 2, N - Nvalues
      if (array(i) < array(i - 1)) then
        allocate (error)
        return
      endif
    enddo

    !The last (Nvalues) elements must be larger
    !than the first (N-Nvalues) elements in
    !order of magnitude.
    do i = N - Nvalues + 1, N
      do j = 1, N - Nvalues
        if (abs(array(i)) < abs(array(j))) then
          allocate (error)
          return
        endif
      enddo
    enddo

    deallocate (array)
  end subroutine test_shift_on_random_dp_arrays

end module Auxiliary_Functionality
