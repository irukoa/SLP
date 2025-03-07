
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
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet ()", t_SLP_out_sp_pp0_w0Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. sil )", t_SLP_out_sp_pp0_w0Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. sym )", t_SLP_out_sp_pp0_w0Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w0Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. w )", t_SLP_out_sp_pp0_w1Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. w Inp. sil )", t_SLP_out_sp_pp0_w1Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. w Inp. sym )", t_SLP_out_sp_pp0_w1Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w1Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp )", t_SLP_out_sp_pp1_w0Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. sil )", t_SLP_out_sp_pp1_w0Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. sym )", t_SLP_out_sp_pp1_w0Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w0Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. w )", t_SLP_out_sp_pp1_w1Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. w Inp. sil )", t_SLP_out_sp_pp1_w1Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. w Inp. sym )", t_SLP_out_sp_pp1_w1Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Dirichlet (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w1Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann ()", t_SLP_out_sp_pp0_w0Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. sil )", t_SLP_out_sp_pp0_w0Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. sym )", t_SLP_out_sp_pp0_w0Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w0Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. w )", t_SLP_out_sp_pp0_w1Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. w Inp. sil )", t_SLP_out_sp_pp0_w1Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. w Inp. sym )", t_SLP_out_sp_pp0_w1Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w1Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp )", t_SLP_out_sp_pp1_w0Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. sil )", t_SLP_out_sp_pp1_w0Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. sym )", t_SLP_out_sp_pp1_w0Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w0Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. w )", t_SLP_out_sp_pp1_w1Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. w Inp. sil )", t_SLP_out_sp_pp1_w1Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. w Inp. sym )", t_SLP_out_sp_pp1_w1Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Neumann (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w1Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed ()", t_SLP_out_sp_pp0_w0Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. sil )", t_SLP_out_sp_pp0_w0Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. sym )", t_SLP_out_sp_pp0_w0Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w0Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. w )", t_SLP_out_sp_pp0_w1Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. w Inp. sil )", t_SLP_out_sp_pp0_w1Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. w Inp. sym )", t_SLP_out_sp_pp0_w1Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w1Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp )", t_SLP_out_sp_pp1_w0Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. sil )", t_SLP_out_sp_pp1_w0Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. sym )", t_SLP_out_sp_pp1_w0Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w0Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. w )", t_SLP_out_sp_pp1_w1Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. w Inp. sil )", t_SLP_out_sp_pp1_w1Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. w Inp. sym )", t_SLP_out_sp_pp1_w1Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Mixed (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w1Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic ()", t_SLP_out_sp_pp0_w0Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. sil )", t_SLP_out_sp_pp0_w0Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. sym )", t_SLP_out_sp_pp0_w0Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w0Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. w )", t_SLP_out_sp_pp0_w1Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. w Inp. sil )", t_SLP_out_sp_pp0_w1Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. w Inp. sym )", t_SLP_out_sp_pp0_w1Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w1Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp )", t_SLP_out_sp_pp1_w0Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. sil )", t_SLP_out_sp_pp1_w0Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. sym )", t_SLP_out_sp_pp1_w0Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w0Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. w )", t_SLP_out_sp_pp1_w1Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. w Inp. sil )", t_SLP_out_sp_pp1_w1Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. w Inp. sym )", t_SLP_out_sp_pp1_w1Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Periodic (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w1Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free ()", t_SLP_out_sp_pp0_w0Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. sil )", t_SLP_out_sp_pp0_w0Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. sym )", t_SLP_out_sp_pp0_w0Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w0Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. w )", t_SLP_out_sp_pp0_w1Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. w Inp. sil )", t_SLP_out_sp_pp0_w1Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. w Inp. sym )", t_SLP_out_sp_pp0_w1Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w1Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp )", t_SLP_out_sp_pp1_w0Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. sil )", t_SLP_out_sp_pp1_w0Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. sym )", t_SLP_out_sp_pp1_w0Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w0Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. w )", t_SLP_out_sp_pp1_w1Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. w Inp. sil )", t_SLP_out_sp_pp1_w1Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. w Inp. sym )", t_SLP_out_sp_pp1_w1Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Free (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w1Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular ()", t_SLP_out_sp_pp0_w0Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. sil )", t_SLP_out_sp_pp0_w0Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. sym )", t_SLP_out_sp_pp0_w0Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w0Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. w )", t_SLP_out_sp_pp0_w1Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. w Inp. sil )", t_SLP_out_sp_pp0_w1Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. w Inp. sym )", t_SLP_out_sp_pp0_w1Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp0_w1Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp )", t_SLP_out_sp_pp1_w0Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. sil )", t_SLP_out_sp_pp1_w0Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. sym )", t_SLP_out_sp_pp1_w0Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w0Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. w )", t_SLP_out_sp_pp1_w1Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. w Inp. sil )", t_SLP_out_sp_pp1_w1Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. w Inp. sym )", t_SLP_out_sp_pp1_w1Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (sp) SLP: Singular (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_sp_pp1_w1Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet ()", t_SLP_out_dp_pp0_w0Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. sil )", t_SLP_out_dp_pp0_w0Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. sym )", t_SLP_out_dp_pp0_w0Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w0Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. w )", t_SLP_out_dp_pp0_w1Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. w Inp. sil )", t_SLP_out_dp_pp0_w1Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. w Inp. sym )", t_SLP_out_dp_pp0_w1Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w1Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp )", t_SLP_out_dp_pp1_w0Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. sil )", t_SLP_out_dp_pp1_w0Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. sym )", t_SLP_out_dp_pp1_w0Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w0Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. w )", t_SLP_out_dp_pp1_w1Dirichlet_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. w Inp. sil )", t_SLP_out_dp_pp1_w1Dirichlet_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. w Inp. sym )", t_SLP_out_dp_pp1_w1Dirichlet_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Dirichlet (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w1Dirichlet_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann ()", t_SLP_out_dp_pp0_w0Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. sil )", t_SLP_out_dp_pp0_w0Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. sym )", t_SLP_out_dp_pp0_w0Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w0Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. w )", t_SLP_out_dp_pp0_w1Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. w Inp. sil )", t_SLP_out_dp_pp0_w1Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. w Inp. sym )", t_SLP_out_dp_pp0_w1Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w1Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp )", t_SLP_out_dp_pp1_w0Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. sil )", t_SLP_out_dp_pp1_w0Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. sym )", t_SLP_out_dp_pp1_w0Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w0Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. w )", t_SLP_out_dp_pp1_w1Neumann_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. w Inp. sil )", t_SLP_out_dp_pp1_w1Neumann_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. w Inp. sym )", t_SLP_out_dp_pp1_w1Neumann_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Neumann (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w1Neumann_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed ()", t_SLP_out_dp_pp0_w0Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. sil )", t_SLP_out_dp_pp0_w0Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. sym )", t_SLP_out_dp_pp0_w0Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w0Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. w )", t_SLP_out_dp_pp0_w1Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. w Inp. sil )", t_SLP_out_dp_pp0_w1Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. w Inp. sym )", t_SLP_out_dp_pp0_w1Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w1Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp )", t_SLP_out_dp_pp1_w0Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. sil )", t_SLP_out_dp_pp1_w0Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. sym )", t_SLP_out_dp_pp1_w0Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w0Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. w )", t_SLP_out_dp_pp1_w1Mixed_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. w Inp. sil )", t_SLP_out_dp_pp1_w1Mixed_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. w Inp. sym )", t_SLP_out_dp_pp1_w1Mixed_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Mixed (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w1Mixed_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic ()", t_SLP_out_dp_pp0_w0Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. sil )", t_SLP_out_dp_pp0_w0Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. sym )", t_SLP_out_dp_pp0_w0Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w0Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. w )", t_SLP_out_dp_pp0_w1Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. w Inp. sil )", t_SLP_out_dp_pp0_w1Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. w Inp. sym )", t_SLP_out_dp_pp0_w1Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w1Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp )", t_SLP_out_dp_pp1_w0Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. sil )", t_SLP_out_dp_pp1_w0Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. sym )", t_SLP_out_dp_pp1_w0Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w0Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. w )", t_SLP_out_dp_pp1_w1Periodic_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. w Inp. sil )", t_SLP_out_dp_pp1_w1Periodic_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. w Inp. sym )", t_SLP_out_dp_pp1_w1Periodic_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Periodic (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w1Periodic_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free ()", t_SLP_out_dp_pp0_w0Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. sil )", t_SLP_out_dp_pp0_w0Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. sym )", t_SLP_out_dp_pp0_w0Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w0Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. w )", t_SLP_out_dp_pp0_w1Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. w Inp. sil )", t_SLP_out_dp_pp0_w1Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. w Inp. sym )", t_SLP_out_dp_pp0_w1Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w1Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp )", t_SLP_out_dp_pp1_w0Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. sil )", t_SLP_out_dp_pp1_w0Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. sym )", t_SLP_out_dp_pp1_w0Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w0Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. w )", t_SLP_out_dp_pp1_w1Free_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. w Inp. sil )", t_SLP_out_dp_pp1_w1Free_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. w Inp. sym )", t_SLP_out_dp_pp1_w1Free_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Free (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w1Free_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular ()", t_SLP_out_dp_pp0_w0Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. sil )", t_SLP_out_dp_pp0_w0Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. sym )", t_SLP_out_dp_pp0_w0Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w0Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. w )", t_SLP_out_dp_pp0_w1Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. w Inp. sil )", t_SLP_out_dp_pp0_w1Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. w Inp. sym )", t_SLP_out_dp_pp0_w1Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp0_w1Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp )", t_SLP_out_dp_pp1_w0Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. sil )", t_SLP_out_dp_pp1_w0Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. sym )", t_SLP_out_dp_pp1_w0Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w0Singular_s1_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. w )", t_SLP_out_dp_pp1_w1Singular_s0_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. w Inp. sil )", t_SLP_out_dp_pp1_w1Singular_s0_sil1) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. w Inp. sym )", t_SLP_out_dp_pp1_w1Singular_s1_sil0) &
                , new_unittest("check constructor, functionality and solver: same condition on both bounds &
                &real (dp) SLP: Singular (Inp. pp Inp. w Inp. sym Inp. sil )", t_SLP_out_dp_pp1_w1Singular_s1_sil1) &
                ]

  end subroutine collect_Obj_Functionality

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

  !For each type, precision, optional argument...

  subroutine t_SLP_out_sp_pp0_w0Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w0Dirichlet_s0_sil0

  subroutine t_SLP_out_sp_pp0_w0Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Dirichlet_s0_sil1

  subroutine t_SLP_out_sp_pp0_w0Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Dirichlet_s1_sil0

  subroutine t_SLP_out_sp_pp0_w0Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w0Dirichlet_s1_sil1

  subroutine t_SLP_out_sp_pp0_w0Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w0Neumann_s0_sil0

  subroutine t_SLP_out_sp_pp0_w0Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Neumann_s0_sil1

  subroutine t_SLP_out_sp_pp0_w0Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Neumann_s1_sil0

  subroutine t_SLP_out_sp_pp0_w0Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w0Neumann_s1_sil1

  subroutine t_SLP_out_sp_pp0_w0Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_sp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w0Mixed_s0_sil0

  subroutine t_SLP_out_sp_pp0_w0Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Mixed_s0_sil1

  subroutine t_SLP_out_sp_pp0_w0Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Mixed_s1_sil0

  subroutine t_SLP_out_sp_pp0_w0Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w0Mixed_s1_sil1

  subroutine t_SLP_out_sp_pp0_w0Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w0Periodic_s0_sil0

  subroutine t_SLP_out_sp_pp0_w0Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Periodic_s0_sil1

  subroutine t_SLP_out_sp_pp0_w0Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Periodic_s1_sil0

  subroutine t_SLP_out_sp_pp0_w0Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w0Periodic_s1_sil1

  subroutine t_SLP_out_sp_pp0_w0Free_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp0_w0Free_s0_sil0

  subroutine t_SLP_out_sp_pp0_w0Free_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Free_s0_sil1

  subroutine t_SLP_out_sp_pp0_w0Free_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Free_s1_sil0

  subroutine t_SLP_out_sp_pp0_w0Free_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w0Free_s1_sil1

  subroutine t_SLP_out_sp_pp0_w0Singular_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp0_w0Singular_s0_sil0

  subroutine t_SLP_out_sp_pp0_w0Singular_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Singular_s0_sil1

  subroutine t_SLP_out_sp_pp0_w0Singular_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w0Singular_s1_sil0

  subroutine t_SLP_out_sp_pp0_w0Singular_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w0Singular_s1_sil1

  subroutine t_SLP_out_sp_pp0_w1Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w1Dirichlet_s0_sil0

  subroutine t_SLP_out_sp_pp0_w1Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Dirichlet_s0_sil1

  subroutine t_SLP_out_sp_pp0_w1Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Dirichlet_s1_sil0

  subroutine t_SLP_out_sp_pp0_w1Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w1Dirichlet_s1_sil1

  subroutine t_SLP_out_sp_pp0_w1Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w1Neumann_s0_sil0

  subroutine t_SLP_out_sp_pp0_w1Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Neumann_s0_sil1

  subroutine t_SLP_out_sp_pp0_w1Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Neumann_s1_sil0

  subroutine t_SLP_out_sp_pp0_w1Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w1Neumann_s1_sil1

  subroutine t_SLP_out_sp_pp0_w1Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_sp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w1Mixed_s0_sil0

  subroutine t_SLP_out_sp_pp0_w1Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Mixed_s0_sil1

  subroutine t_SLP_out_sp_pp0_w1Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Mixed_s1_sil0

  subroutine t_SLP_out_sp_pp0_w1Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w1Mixed_s1_sil1

  subroutine t_SLP_out_sp_pp0_w1Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp0_w1Periodic_s0_sil0

  subroutine t_SLP_out_sp_pp0_w1Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Periodic_s0_sil1

  subroutine t_SLP_out_sp_pp0_w1Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Periodic_s1_sil0

  subroutine t_SLP_out_sp_pp0_w1Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w1Periodic_s1_sil1

  subroutine t_SLP_out_sp_pp0_w1Free_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp0_w1Free_s0_sil0

  subroutine t_SLP_out_sp_pp0_w1Free_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Free_s0_sil1

  subroutine t_SLP_out_sp_pp0_w1Free_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Free_s1_sil0

  subroutine t_SLP_out_sp_pp0_w1Free_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w1Free_s1_sil1

  subroutine t_SLP_out_sp_pp0_w1Singular_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp0_w1Singular_s0_sil0

  subroutine t_SLP_out_sp_pp0_w1Singular_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Singular_s0_sil1

  subroutine t_SLP_out_sp_pp0_w1Singular_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp0_w1Singular_s1_sil0

  subroutine t_SLP_out_sp_pp0_w1Singular_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp0_w1Singular_s1_sil1

  subroutine t_SLP_out_sp_pp1_w0Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w0Dirichlet_s0_sil0

  subroutine t_SLP_out_sp_pp1_w0Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Dirichlet_s0_sil1

  subroutine t_SLP_out_sp_pp1_w0Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Dirichlet_s1_sil0

  subroutine t_SLP_out_sp_pp1_w0Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w0Dirichlet_s1_sil1

  subroutine t_SLP_out_sp_pp1_w0Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w0Neumann_s0_sil0

  subroutine t_SLP_out_sp_pp1_w0Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Neumann_s0_sil1

  subroutine t_SLP_out_sp_pp1_w0Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Neumann_s1_sil0

  subroutine t_SLP_out_sp_pp1_w0Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w0Neumann_s1_sil1

  subroutine t_SLP_out_sp_pp1_w0Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_sp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w0Mixed_s0_sil0

  subroutine t_SLP_out_sp_pp1_w0Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Mixed_s0_sil1

  subroutine t_SLP_out_sp_pp1_w0Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Mixed_s1_sil0

  subroutine t_SLP_out_sp_pp1_w0Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w0Mixed_s1_sil1

  subroutine t_SLP_out_sp_pp1_w0Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w0Periodic_s0_sil0

  subroutine t_SLP_out_sp_pp1_w0Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Periodic_s0_sil1

  subroutine t_SLP_out_sp_pp1_w0Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Periodic_s1_sil0

  subroutine t_SLP_out_sp_pp1_w0Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w0Periodic_s1_sil1

  subroutine t_SLP_out_sp_pp1_w0Free_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp1_w0Free_s0_sil0

  subroutine t_SLP_out_sp_pp1_w0Free_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Free_s0_sil1

  subroutine t_SLP_out_sp_pp1_w0Free_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Free_s1_sil0

  subroutine t_SLP_out_sp_pp1_w0Free_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w0Free_s1_sil1

  subroutine t_SLP_out_sp_pp1_w0Singular_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp1_w0Singular_s0_sil0

  subroutine t_SLP_out_sp_pp1_w0Singular_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Singular_s0_sil1

  subroutine t_SLP_out_sp_pp1_w0Singular_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w0Singular_s1_sil0

  subroutine t_SLP_out_sp_pp1_w0Singular_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w0Singular_s1_sil1

  subroutine t_SLP_out_sp_pp1_w1Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w1Dirichlet_s0_sil0

  subroutine t_SLP_out_sp_pp1_w1Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Dirichlet_s0_sil1

  subroutine t_SLP_out_sp_pp1_w1Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Dirichlet_s1_sil0

  subroutine t_SLP_out_sp_pp1_w1Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w1Dirichlet_s1_sil1

  subroutine t_SLP_out_sp_pp1_w1Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w1Neumann_s0_sil0

  subroutine t_SLP_out_sp_pp1_w1Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Neumann_s0_sil1

  subroutine t_SLP_out_sp_pp1_w1Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Neumann_s1_sil0

  subroutine t_SLP_out_sp_pp1_w1Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w1Neumann_s1_sil1

  subroutine t_SLP_out_sp_pp1_w1Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_sp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w1Mixed_s0_sil0

  subroutine t_SLP_out_sp_pp1_w1Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Mixed_s0_sil1

  subroutine t_SLP_out_sp_pp1_w1Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Mixed_s1_sil0

  subroutine t_SLP_out_sp_pp1_w1Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_sp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_sp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_sp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w1Mixed_s1_sil1

  subroutine t_SLP_out_sp_pp1_w1Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_sp_pp1_w1Periodic_s0_sil0

  subroutine t_SLP_out_sp_pp1_w1Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Periodic_s0_sil1

  subroutine t_SLP_out_sp_pp1_w1Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Periodic_s1_sil0

  subroutine t_SLP_out_sp_pp1_w1Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_sp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_sp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w1Periodic_s1_sil1

  subroutine t_SLP_out_sp_pp1_w1Free_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp1_w1Free_s0_sil0

  subroutine t_SLP_out_sp_pp1_w1Free_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Free_s0_sil1

  subroutine t_SLP_out_sp_pp1_w1Free_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Free_s1_sil0

  subroutine t_SLP_out_sp_pp1_w1Free_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w1Free_s1_sil1

  subroutine t_SLP_out_sp_pp1_w1Singular_s0_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_sp, b=1.0_sp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_sp_pp1_w1Singular_s0_sil0

  subroutine t_SLP_out_sp_pp1_w1Singular_s0_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Singular_s0_sil1

  subroutine t_SLP_out_sp_pp1_w1Singular_s1_sil0(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_sp_pp1_w1Singular_s1_sil0

  subroutine t_SLP_out_sp_pp1_w1Singular_s1_sil1(error)
    use SLP_obj, only: SLP_sp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_sp) :: prb
    integer, parameter :: N = 100
    real(sp) :: p(N), pp(N), q(N), w(N)

    real(sp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_sp
    pp = 1.0_sp
    q = 0.0_sp
    w = 1.0_sp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_sp, b=1.0_sp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, sp))**(-1.0_sp)) > tol_sp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_sp_pp1_w1Singular_s1_sil1

  subroutine t_SLP_out_dp_pp0_w0Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w0Dirichlet_s0_sil0

  subroutine t_SLP_out_dp_pp0_w0Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Dirichlet_s0_sil1

  subroutine t_SLP_out_dp_pp0_w0Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Dirichlet_s1_sil0

  subroutine t_SLP_out_dp_pp0_w0Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w0Dirichlet_s1_sil1

  subroutine t_SLP_out_dp_pp0_w0Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w0Neumann_s0_sil0

  subroutine t_SLP_out_dp_pp0_w0Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Neumann_s0_sil1

  subroutine t_SLP_out_dp_pp0_w0Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Neumann_s1_sil0

  subroutine t_SLP_out_dp_pp0_w0Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w0Neumann_s1_sil1

  subroutine t_SLP_out_dp_pp0_w0Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_dp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w0Mixed_s0_sil0

  subroutine t_SLP_out_dp_pp0_w0Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Mixed_s0_sil1

  subroutine t_SLP_out_dp_pp0_w0Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Mixed_s1_sil0

  subroutine t_SLP_out_dp_pp0_w0Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w0Mixed_s1_sil1

  subroutine t_SLP_out_dp_pp0_w0Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w0Periodic_s0_sil0

  subroutine t_SLP_out_dp_pp0_w0Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Periodic_s0_sil1

  subroutine t_SLP_out_dp_pp0_w0Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Periodic_s1_sil0

  subroutine t_SLP_out_dp_pp0_w0Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w0Periodic_s1_sil1

  subroutine t_SLP_out_dp_pp0_w0Free_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp0_w0Free_s0_sil0

  subroutine t_SLP_out_dp_pp0_w0Free_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Free_s0_sil1

  subroutine t_SLP_out_dp_pp0_w0Free_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Free_s1_sil0

  subroutine t_SLP_out_dp_pp0_w0Free_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w0Free_s1_sil1

  subroutine t_SLP_out_dp_pp0_w0Singular_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp0_w0Singular_s0_sil0

  subroutine t_SLP_out_dp_pp0_w0Singular_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Singular_s0_sil1

  subroutine t_SLP_out_dp_pp0_w0Singular_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w0Singular_s1_sil0

  subroutine t_SLP_out_dp_pp0_w0Singular_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w0Singular_s1_sil1

  subroutine t_SLP_out_dp_pp0_w1Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w1Dirichlet_s0_sil0

  subroutine t_SLP_out_dp_pp0_w1Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Dirichlet_s0_sil1

  subroutine t_SLP_out_dp_pp0_w1Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Dirichlet_s1_sil0

  subroutine t_SLP_out_dp_pp0_w1Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w1Dirichlet_s1_sil1

  subroutine t_SLP_out_dp_pp0_w1Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w1Neumann_s0_sil0

  subroutine t_SLP_out_dp_pp0_w1Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Neumann_s0_sil1

  subroutine t_SLP_out_dp_pp0_w1Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Neumann_s1_sil0

  subroutine t_SLP_out_dp_pp0_w1Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w1Neumann_s1_sil1

  subroutine t_SLP_out_dp_pp0_w1Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_dp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w1Mixed_s0_sil0

  subroutine t_SLP_out_dp_pp0_w1Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Mixed_s0_sil1

  subroutine t_SLP_out_dp_pp0_w1Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Mixed_s1_sil0

  subroutine t_SLP_out_dp_pp0_w1Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w1Mixed_s1_sil1

  subroutine t_SLP_out_dp_pp0_w1Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp0_w1Periodic_s0_sil0

  subroutine t_SLP_out_dp_pp0_w1Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Periodic_s0_sil1

  subroutine t_SLP_out_dp_pp0_w1Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Periodic_s1_sil0

  subroutine t_SLP_out_dp_pp0_w1Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w1Periodic_s1_sil1

  subroutine t_SLP_out_dp_pp0_w1Free_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp0_w1Free_s0_sil0

  subroutine t_SLP_out_dp_pp0_w1Free_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Free_s0_sil1

  subroutine t_SLP_out_dp_pp0_w1Free_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Free_s1_sil0

  subroutine t_SLP_out_dp_pp0_w1Free_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w1Free_s1_sil1

  subroutine t_SLP_out_dp_pp0_w1Singular_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    q=q, &
                    w=w, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp0_w1Singular_s0_sil0

  subroutine t_SLP_out_dp_pp0_w1Singular_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Singular_s0_sil1

  subroutine t_SLP_out_dp_pp0_w1Singular_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp0_w1Singular_s1_sil0

  subroutine t_SLP_out_dp_pp0_w1Singular_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp0_w1Singular_s1_sil1

  subroutine t_SLP_out_dp_pp1_w0Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w0Dirichlet_s0_sil0

  subroutine t_SLP_out_dp_pp1_w0Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Dirichlet_s0_sil1

  subroutine t_SLP_out_dp_pp1_w0Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Dirichlet_s1_sil0

  subroutine t_SLP_out_dp_pp1_w0Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w0Dirichlet_s1_sil1

  subroutine t_SLP_out_dp_pp1_w0Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w0Neumann_s0_sil0

  subroutine t_SLP_out_dp_pp1_w0Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Neumann_s0_sil1

  subroutine t_SLP_out_dp_pp1_w0Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Neumann_s1_sil0

  subroutine t_SLP_out_dp_pp1_w0Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w0Neumann_s1_sil1

  subroutine t_SLP_out_dp_pp1_w0Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_dp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w0Mixed_s0_sil0

  subroutine t_SLP_out_dp_pp1_w0Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Mixed_s0_sil1

  subroutine t_SLP_out_dp_pp1_w0Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Mixed_s1_sil0

  subroutine t_SLP_out_dp_pp1_w0Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w0Mixed_s1_sil1

  subroutine t_SLP_out_dp_pp1_w0Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w0Periodic_s0_sil0

  subroutine t_SLP_out_dp_pp1_w0Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Periodic_s0_sil1

  subroutine t_SLP_out_dp_pp1_w0Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Periodic_s1_sil0

  subroutine t_SLP_out_dp_pp1_w0Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w0Periodic_s1_sil1

  subroutine t_SLP_out_dp_pp1_w0Free_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp1_w0Free_s0_sil0

  subroutine t_SLP_out_dp_pp1_w0Free_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Free_s0_sil1

  subroutine t_SLP_out_dp_pp1_w0Free_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Free_s1_sil0

  subroutine t_SLP_out_dp_pp1_w0Free_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w0Free_s1_sil1

  subroutine t_SLP_out_dp_pp1_w0Singular_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp1_w0Singular_s0_sil0

  subroutine t_SLP_out_dp_pp1_w0Singular_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Singular_s0_sil1

  subroutine t_SLP_out_dp_pp1_w0Singular_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w0Singular_s1_sil0

  subroutine t_SLP_out_dp_pp1_w0Singular_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w0Singular_s1_sil1

  subroutine t_SLP_out_dp_pp1_w1Dirichlet_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Dirichlet" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w1Dirichlet_s0_sil0

  subroutine t_SLP_out_dp_pp1_w1Dirichlet_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Dirichlet_s0_sil1

  subroutine t_SLP_out_dp_pp1_w1Dirichlet_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Dirichlet_s1_sil0

  subroutine t_SLP_out_dp_pp1_w1Dirichlet_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Dirichlet" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w1Dirichlet_s1_sil1

  subroutine t_SLP_out_dp_pp1_w1Neumann_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Neumann" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w1Neumann_s0_sil0

  subroutine t_SLP_out_dp_pp1_w1Neumann_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Neumann_s0_sil1

  subroutine t_SLP_out_dp_pp1_w1Neumann_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Neumann_s1_sil0

  subroutine t_SLP_out_dp_pp1_w1Neumann_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Neumann" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(2, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w1Neumann_s1_sil1

  subroutine t_SLP_out_dp_pp1_w1Mixed_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Mixed" &
                    , mixing_param=1.0_dp &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w1Mixed_s0_sil0

  subroutine t_SLP_out_dp_pp1_w1Mixed_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Mixed_s0_sil1

  subroutine t_SLP_out_dp_pp1_w1Mixed_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Mixed_s1_sil0

  subroutine t_SLP_out_dp_pp1_w1Mixed_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Mixed" &
                      , mixing_param=1.0_dp &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - 1.0_dp*(vl(2, 1:N - 3) - vl(1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(N, 1:N - 3) + 1.0_dp*(vl(N, 1:N - 3) - vl(N - 1, 1:N - 3))/prb%spacing()) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w1Mixed_s1_sil1

  subroutine t_SLP_out_dp_pp1_w1Periodic_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Periodic" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

    if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
      allocate (error)
      return
    endif
    if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
      allocate (error)
      return
    endif

  end subroutine t_SLP_out_dp_pp1_w1Periodic_s0_sil0

  subroutine t_SLP_out_dp_pp1_w1Periodic_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Periodic_s0_sil1

  subroutine t_SLP_out_dp_pp1_w1Periodic_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Periodic_s1_sil0

  subroutine t_SLP_out_dp_pp1_w1Periodic_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Periodic" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

      if (any(abs(vl(1, 1:N - 3) - vl(N, 1:N - 3)) > tol_dp)) then
        allocate (error)
        return
      endif
      if (any(abs(vl(1, 1:N - 3) + vl(N, 1:N - 3) - (vl(2, 1:N - 3) + vl(N - 1, 1:N - 3))) > tol_dp)) then
        allocate (error)
        return
      endif

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w1Periodic_s1_sil1

  subroutine t_SLP_out_dp_pp1_w1Free_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Free" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp1_w1Free_s0_sil0

  subroutine t_SLP_out_dp_pp1_w1Free_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Free_s0_sil1

  subroutine t_SLP_out_dp_pp1_w1Free_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Free_s1_sil0

  subroutine t_SLP_out_dp_pp1_w1Free_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Free" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w1Free_s1_sil1

  subroutine t_SLP_out_dp_pp1_w1Singular_s0_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%define(name="Test", &
                    a=0.0_dp, b=1.0_dp, N=N, &
                    p=p, &
                    pp=pp, &
                    q=q, &
                    w=w, &
                    bound_cond="Singular" &
                    )

    if (prb%name() /= "Test") then
      allocate (error)
      return
    endif

    if (prb%relative_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%absolute_error() > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%Ndsc() /= N) then
      allocate (error)
      return
    endif

    if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    call prb%solve( &
      )

    if (.not. prb%solved()) then
      allocate (error)
      return
    endif

    vl = prb%eivec()

  end subroutine t_SLP_out_dp_pp1_w1Singular_s0_sil0

  subroutine t_SLP_out_dp_pp1_w1Singular_s0_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Singular_s0_sil1

  subroutine t_SLP_out_dp_pp1_w1Singular_s1_sil0(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
  end subroutine t_SLP_out_dp_pp1_w1Singular_s1_sil0

  subroutine t_SLP_out_dp_pp1_w1Singular_s1_sil1(error)
    use SLP_obj, only: SLP_dp
    type(error_type), allocatable, intent(out) :: error
    type(SLP_dp) :: prb
    integer, parameter :: N = 100
    real(dp) :: p(N), pp(N), q(N), w(N)

    real(dp) :: vl(N, N)

    logical :: tb(2) = [.false., .true.]
    integer :: i, j

    p = 1.0_dp
    pp = 1.0_dp
    q = 0.0_dp
    w = 1.0_dp

    if (prb%initialized()) then
      allocate (error)
      return
    endif

    if (prb%solved()) then
      allocate (error)
      return
    endif

    do i = 1, 2
    do j = 1, 2

      call prb%define(name="Test", &
                      a=0.0_dp, b=1.0_dp, N=N, &
                      p=p, &
                      pp=pp, &
                      q=q, &
                      w=w, &
                      bound_cond="Singular" &
                      , enforce_self_adjoint=tb(i) &
                      , silent=tb(j) &
                      )

      if (prb%name() /= "Test") then
        allocate (error)
        return
      endif

      if (prb%relative_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%absolute_error() > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%Ndsc() /= N) then
        allocate (error)
        return
      endif

      if ((prb%spacing() - (real(N - 1, dp))**(-1.0_dp)) > tol_dp) then
        allocate (error)
        return
      endif

      if (prb%solved()) then
        allocate (error)
        return
      endif

      call prb%solve( &
        silent=tb(j) &
        )

      if (.not. prb%solved()) then
        allocate (error)
        return
      endif

      vl = prb%eivec()

    enddo
    enddo
  end subroutine t_SLP_out_dp_pp1_w1Singular_s1_sil1

end module Obj_Functionality
