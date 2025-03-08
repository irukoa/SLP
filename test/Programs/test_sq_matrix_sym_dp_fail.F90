program test_sq_matrix_sym_dp_fail
  use SLP_kinds, only: dp
  use SLP_util, only: dg_gen

  real(dp) :: M(2, 3), Me(3, 3), P(3, 3), D(3, 3), eig(3)

  call dg_gen(M, Me, P, D, eig)

end program test_sq_matrix_sym_dp_fail
