message("-- Compiling in Mare-Nostrum...")
if(USE_MPI)
  message(" # Using MPI compiler wrappers...")
  set(CMAKE_C_COMPILER       "/apps/INTEL/2018.4.057/impi/2018.4.274/bin64/mpicc")
  set(CMAKE_CXX_COMPILER     "/apps/INTEL/2018.4.057/impi/2018.4.274/bin64/mpicxx")
  set(CMAKE_Fortran_COMPILER "/apps/INTEL/2018.4.057/impi/2018.4.274/bin64/mpif90")
else()
  message(" # Using NV compiler wrappers...")
  set(CMAKE_C_COMPILER       "/usr/bin/gcc")
  set(CMAKE_CXX_COMPILER     "/usr/bin/g++")
  set(CMAKE_Fortran_COMPILER "/usr/bin/gfortran")
endif()