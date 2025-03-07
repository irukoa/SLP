#:set types = ['real']                           #!Type.
#:set tsuffixes = ['']                           #!Suffix for each type, was ['_r'].
#:set tlst = list(zip(types, tsuffixes))
#:set precisions = ['sp', 'dp']
#:set psuffixes = ['_sp', '_dp']
#:set plst = list(zip(precisions, psuffixes))

#: set inp_pp = [0, 1]
#: set inp_w = [0, 1]
#: set bcond = ['Dirichlet', 'Neumann', 'Mixed', 'Periodic', 'Free', 'Singular']
#: set inp_sym = [0, 1]
#: set inp_sil = [0, 1]
module Obj_Functionality

  use, intrinsic :: Iso_Fortran_ENV, only: stderr => error_unit
  use testdrive, only: new_unittest, unittest_type, error_type

  use SLP_kinds, only: sp, dp

  implicit none
  private

  real(sp), parameter :: tol_sp = 0.01_sp
  real(dp), parameter :: tol_dp = 1.0E-7_dp

  public :: collect_Obj_Functionality

contains

  subroutine collect_Obj_Functionality(testsuite)

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                  new_unittest("basic check", test_basic) &
#:for type, tsfx in tlst
#:for prec, psfx in plst
#:for boundcond in bcond
#:for inputing_pp in inp_pp
#:for inputing_w in inp_w
#:for inputing_sym in inp_sym
#:for inputing_sil in inp_sil
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &${type}$ (${prec}$) SLP: ${boundcond}$ (#{if inputing_pp > 0}#Inp. pp #{endif}##{if inputing_w > 0}#Inp. w #{endif}##{if inputing_sym > 0}#Inp. sym #{endif}##{if inputing_sil > 0}#Inp. sil #{endif}#)", t_SLP_out${tsfx}$${psfx}$_pp${inputing_pp}$_w${inputing_w}$${boundcond}$_s${inputing_sym}$_sil${inputing_sil}$) &
#:endfor
#:endfor
#:endfor
#:endfor
#:endfor
#:endfor
#:endfor
                ]

  end subroutine collect_Obj_Functionality

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

  !For each type, precision, optional argument...
#:for type, tsfx in tlst
#:for prec, psfx in plst
#:for inputing_pp in inp_pp
#:for inputing_w in inp_w
#:for boundcond in bcond
#:for inputing_sym in inp_sym
#:for inputing_sil in inp_sil

  subroutine t_SLP_out${tsfx}$${psfx}$_pp${inputing_pp}$_w${inputing_w}$${boundcond}$_s${inputing_sym}$_sil${inputing_sil}$(error)
    use SLP_obj, only: SLP${psfx}$
    type(error_type), allocatable, intent(out) :: error
    type(SLP${tsfx}$${psfx}$) :: prb
    integer, parameter :: N = 100
    real(${prec}$) :: p(N), pp(N), q(N), w(N)

    real(${prec}$) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0${psfx}$
    pp = 1.0${psfx}$
    q = 0.0${psfx}$
    w = 1.0${psfx}$

    if (prb%initialized()) then
      allocate(error)
      return
    endif

    if (prb%solved()) then
      allocate(error)
      return
    endif

#:if inputing_sym > 0
    do i = 1, 2
#:endif
#:if inputing_sil > 0
    do j = 1, 2
#:endif

    call prb%define(name = "Test", &
                    a=0.0${psfx}$, b=1.0${psfx}$, N=N, &
                    p=p, &
#:if inputing_pp > 0
                    pp=pp, &
#:endif
                    q=q, &
#:if inputing_w > 0
                    w=w, &
#:endif
                    bound_cond="${boundcond}$" &
#:if boundcond == 'Mixed'
                    , mixing_param=1.0${psfx}$ &
#:endif
#:if inputing_sym > 0
                    , enforce_self_adjoint=tb(i) &
#:endif
#:if inputing_sil > 0
                    , silent=tb(j) &
#:endif
                    )

    if (prb%name()/="Test") then
      allocate(error)
      return
    endif

    if (prb%relative_error()>tol${psfx}$) then
      allocate(error)
      return
    endif

    if (prb%absolute_error()>tol${psfx}$) then
      allocate(error)
      return
    endif

    if (prb%Ndsc()/=N) then
      allocate(error)
      return
    endif

    if ((prb%spacing() - (real(N-1, ${prec}$))**(-1.0${psfx}$))>tol${psfx}$) then
      allocate(error)
      return
    endif

    if (prb%solved()) then
      allocate(error)
      return
    endif

    call prb%solve(&
#:if inputing_sil > 0
                   silent=tb(j) &
#:endif
                   )

    if (.not.prb%solved()) then
      allocate(error)
      return
    endif

    vl = prb%eivec()

#:if boundcond == 'Dirichlet'
    if (any(abs(vl(1, 1:N-3))>tol${psfx}$)) then
      allocate(error)
      return
    endif
    if (any(abs(vl(N, 1:N-3))>tol${psfx}$)) then
      allocate(error)
      return
    endif
#:endif

#:if boundcond == 'Neumann'
    if (any(abs(vl(1, 1:N-3)-vl(2, 1:N-3))>tol${psfx}$)) then
      allocate(error)
      return
    endif
    if (any(abs(vl(N, 1:N-3)-vl(N-1, 1:N-3))>tol${psfx}$)) then
      allocate(error)
      return
    endif
#:endif

#:if boundcond == 'Mixed'
    if (any(abs(vl(1, 1:N-3)-1.0${psfx}$*(vl(2, 1:N-3)-vl(1, 1:N-3))/prb%spacing())>tol${psfx}$)) then
      allocate(error)
      return
    endif
    if (any(abs(vl(N, 1:N-3)+1.0${psfx}$*(vl(N, 1:N-3)-vl(N-1, 1:N-3))/prb%spacing())>tol${psfx}$)) then
      allocate(error)
      return
    endif
#:endif

#:if boundcond == 'Periodic'
    if (any(abs(vl(1, 1:N-3)-vl(N, 1:N-3))>tol${psfx}$)) then
      allocate(error)
      return
    endif
    if (any(abs(vl(1, 1:N-3)+vl(N, 1:N-3)-(vl(2, 1:N-3)+vl(N-1, 1:N-3)))>tol${psfx}$)) then
      allocate(error)
      return
    endif
#:endif

#:if inputing_sym > 0
    enddo
#:endif
#:if inputing_sil > 0
    enddo
#:endif
  end subroutine t_SLP_out${tsfx}$${psfx}$_pp${inputing_pp}$_w${inputing_w}$${boundcond}$_s${inputing_sym}$_sil${inputing_sil}$
#:endfor
#:endfor
#:endfor
#:endfor
#:endfor
#:endfor
#:endfor

end module Obj_Functionality
