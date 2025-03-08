[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![CI](https://github.com/irukoa/SLP/actions/workflows/CI.yml/badge.svg)](https://github.com/irukoa/SLP/actions/workflows/CI.yml)
[![codecov](https://codecov.io/github/irukoa/SLP/graph/badge.svg?token=2IUGDUA46R)](https://codecov.io/github/irukoa/SLP)
[![DOI](https://zenodo.org/badge/941997789.svg)](https://doi.org/10.5281/zenodo.14961609)

# SLP
Sturm-Liouville problem solver (SLP).

Functional. Only partially tested. Not documented.

Ifort, ifx complain when running tests, SEGFault at valid input. "ulimit -s unlimited" solves the issue. --flag "-heap-arrays" solves it for ifx.
Gfortran does not have such problems.
