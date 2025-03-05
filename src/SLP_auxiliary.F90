module SLP_auxiliary

  use, intrinsic :: Iso_Fortran_ENV, only: stdout => output_unit
  use SLP_kinds, only: sp, dp

  implicit none
  private

  interface cshift_N_largest_to_top
    module procedure :: get_shift_moving_N_largest_to_top_dp
    module procedure :: get_shift_moving_N_largest_to_top_sp
  end interface

  public :: boundary_condition_linter
  public :: cshift_N_largest_to_top

contains

  subroutine boundary_condition_linter(name, not_silent, boundary_condition, boundary_condition_id)
    character(len=*), intent(in) :: name
    logical, intent(in) :: not_silent
    logical, intent(out) :: boundary_condition
    integer, intent(out) :: boundary_condition_id

    select case (name)
    case ("Dirichlet", "First_Kind")
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Setting <<Dirichlet>> boundary conditions..."
      boundary_condition = .true.
      boundary_condition_id = 1
    case ("Neumann", "Second_Kind")
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Setting <<Neumann>> boundary conditions..."
      boundary_condition = .true.
      boundary_condition_id = 2
    case ("Mixed", "Third_Kind")
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Setting <<Mixed>> boundary conditions..."
      boundary_condition = .true.
      boundary_condition_id = 3
    case ("Periodic")
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Setting <<Periodic>> boundary conditions..."
      boundary_condition = .true.
      boundary_condition_id = 4
    case ("Free")
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Setting <<Free>> boundary conditions..."
      boundary_condition = .false.
      boundary_condition_id = 0
    case ("Singular")
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Considering singular problem..."
      boundary_condition = .true.
      boundary_condition_id = -1
    case default
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Boundary condition not recognized."
      if (not_silent) write (unit=stdout, fmt="(A)") "SLP: Options are:"
      if (not_silent) write (unit=stdout, fmt="(A)") "     - For y(boundary) = 0: <<Dirichlet>>, or <<First_Kind>>."
      if (not_silent) write (unit=stdout, fmt="(A)") "     - For y'(boundary) = 0: <<Neumann>>, or <<Second_Kind>>."
      if (not_silent) write (unit=stdout, fmt="(A)") "     - For y(boundary) + c*y'(boundary) = 0: <<Mixed>>, or <<Third_Kind>>."
      if (not_silent) write (unit=stdout, fmt="(A)") "     - For y(a) - y(b) = 0, y'(a) - y'(b) = 0: <<Periodic>>."
      if (not_silent) write (unit=stdout, fmt="(A)") "     - Unspecified (not imposed): <<Free>>."
      if (not_silent) write (unit=stdout, fmt="(A)") "     - For singular problems: <<Singular>>."
      error stop "SLP: Boundary condition not recognized."
    end select

  end subroutine boundary_condition_linter

  function get_shift_moving_N_largest_to_top_sp(N, arr) result(shift)
    !Given an array arr arranged in ascending order, computes the left
    !circular shift required to place the N largest values of the array
    !in absolute magnitude on the top. Defining sarr = cshift(arr, shift):
    !- sarr(1 : size(arr) - N) is an array sorted in ascending order.
    !- sarr(size(arr) - N + 1 : N) contains the N largest values of the
    !array arr in absolute magnitude, but unsorted.

    !(*) Assumes N > size(arr) - 2.
    integer, intent(in) :: N
    real(sp), intent(in) :: arr(:)
    integer :: shift

    integer :: sz, i, b1, b2
    real(sp) :: absmin
    real(sp) :: mem(size(arr))

    sz = size(arr)
    mem = arr

    absmin = minval(abs(arr))

    shift = 0

    b1 = 0; b2 = 0

    do i = 1, N

      !The next condition finds the position of the ith largest value.

      !b1 and b2 allow the algorithm to search concurrently
      !at the start and end of the array starting from a previous
      !maximum. For example, when i=1, the possible
      !positions for the largest values are
      !arr(1) and arr(sz). For i=2, if arr(1) was the largest,
      !arr(sz) and arr(2) are considered. If arr(sz) was the
      !largest, arr(sz-1) and arr(1) are considered, and so on.

      if (maxloc(abs(mem), 1) == i - b1) then
        b2 = b2 + 1
        mem(maxloc(abs(mem), 1)) = absmin
        shift = shift + 1
      elseif (maxloc(abs(mem), 1, back=.true.) == sz - (i - 1) + b2) then
        b1 = b1 + 1
        mem(maxloc(abs(mem), 1, back=.true.)) = absmin
        shift = shift
      endif

    enddo

  end function get_shift_moving_N_largest_to_top_sp

  function get_shift_moving_N_largest_to_top_dp(N, arr) result(shift)
    !Given an array arr arranged in ascending order, computes the left
    !circular shift required to place the N largest values of the array
    !in absolute magnitude on the top. Defining sarr = cshift(arr, shift):
    !- sarr(1 : size(arr) - N) is an array sorted in ascending order.
    !- sarr(size(arr) - N + 1 : N) contains the N largest values of the
    !array arr in absolute magnitude, but unsorted.

    !(*) Assumes N > size(arr) - 2.
    integer, intent(in) :: N
    real(dp), intent(in) :: arr(:)
    integer :: shift

    integer :: sz, i, b1, b2
    real(dp) :: absmin
    real(dp) :: mem(size(arr))

    sz = size(arr)
    mem = arr

    absmin = minval(abs(arr))

    shift = 0

    b1 = 0; b2 = 0

    do i = 1, N

      !The next condition finds the position of the ith largest value.

      !b1 and b2 allow the algorithm to search concurrently
      !at the start and end of the array starting from a previous
      !maximum. For example, when i=1, the possible
      !positions for the largest values are
      !arr(1) and arr(sz). For i=2, if arr(1) was the largest,
      !arr(sz) and arr(2) are considered. If arr(sz) was the
      !largest, arr(sz-1) and arr(1) are considered, and so on.

      if (maxloc(abs(mem), 1) == i - b1) then
        b2 = b2 + 1
        mem(maxloc(abs(mem), 1)) = absmin
        shift = shift + 1
      elseif (maxloc(abs(mem), 1, back=.true.) == sz - (i - 1) + b2) then
        b1 = b1 + 1
        mem(maxloc(abs(mem), 1, back=.true.)) = absmin
        shift = shift
      endif

    enddo

  end function get_shift_moving_N_largest_to_top_dp

end module SLP_auxiliary
