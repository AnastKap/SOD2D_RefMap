message("-- Selecting compiler Ops...")
if(USE_PCPOWER)
  message("-- Compiling in PCPOWER...")
  if (USE_MPI)
    message(" # Using MPI compiler wrappers...")
    set(CMAKE_C_COMPILER "/apps/NVIDIA-HPC-SDK/22.2/Linux_ppc64le/22.2/comm_libs/mpi/bin/mpicc")
    set(CMAKE_CXX_COMPILER "/apps/NVIDIA-HPC-SDK/22.2/Linux_ppc64le/22.2/comm_libs/mpi/bin/mpic++")
    set(CMAKE_Fortran_COMPILER "/apps/NVIDIA-HPC-SDK/22.2/Linux_ppc64le/22.2/comm_libs/mpi/bin/mpif90")
  else()
    message(" # Using NV compiler wrappers...")
    set(CMAKE_C_COMPILER "/apps/NVIDIA-HPC-SDK/22.2/Linux_ppc64le/22.2/compilers/bin/nvc")
    set(CMAKE_CXX_COMPILER "/apps/NVIDIA-HPC-SDK/22.2/Linux_ppc64le/22.2/compilers/bin/nvc++")
    set(CMAKE_Fortran_COMPILER "/apps/NVIDIA-HPC-SDK/22.2/Linux_ppc64le/22.2/compilers/bin/nvfortran")
  endif()
else()
  message("-- Compiling in Local...")
  if (USE_MPI)
    message(" # Using MPI compiler wrappers...")
    set(CMAKE_C_COMPILER "/apps/INTEL/2018.4.057/impi/2018.4.274/bin64/mpicc")
    set(CMAKE_CXX_COMPILER "/apps/INTEL/2018.4.057/impi/2018.4.274/bin64/mpicxx")
    set(CMAKE_Fortran_COMPILER "/apps/INTEL/2018.4.057/impi/2018.4.274/bin64/mpif90")
  else()
    message(" # Using NV compiler wrappers...")
    set(CMAKE_C_COMPILER       "/opt/nvidia/hpc_sdk/Linux_x86_64/22.5/compilers/bin/nvc")
    set(CMAKE_CXX_COMPILER     "/opt/nvidia/hpc_sdk/Linux_x86_64/22.5/compilers/bin/nvc++")
    set(CMAKE_Fortran_COMPILER "/opt/nvidia/hpc_sdk/Linux_x86_64/22.5/compilers/bin/nvfortran")
  endif()
endif()

set(CMAKE_C_FLAGS "")
set(CMAKE_C_FLAGS_DEBUG "-O0 -g3")
set(CMAKE_C_FLAGS_RELEASE "-O3")
set(CMAKE_CXX_FLAGS "")
set(CMAKE_CXX_FLAGS_DEBUG "-O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

if(USE_GPU)
  message(" # Using GPUs...")
  if(NOT USE_PCPOWER)
    include_directories("/opt/nvidia/hpc_sdk/Linux_x86_64/22.5/cuda/include/")
  endif()
  #add_link_options(-lnvToolsExt)
  if(USE_MEM_MANAGED)
    message(" # Using Memory Managed Option...")
    set(CMAKE_Fortran_FLAGS         "-cpp -lstdc++ -lmpi_cxx -DNOPRED -fast -D_USE_NVTX -gpu=cc70,managed,lineinfo -cuda -acc -lnvToolsExt -Minfo=accel")
    set(CMAKE_Fortran_FLAGS_RELEASE "-cpp -lstdc++ -lmpi_cxx -DNOPRED -fast -D_USE_NVTX -gpu=cc70,managed,lineinfo -cuda -acc -lnvToolsExt -Minfo=accel")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-cpp -lstdc++ -lmpi_cxx -DNOPRED -fast -D_USE_NVTX -gpu=cc70,managed,lineinfo -cuda -acc -lnvToolsExt -Minfo=accel")
  else()
    message(" # NOT Using Memory Managed Option...")
    set(CMAKE_Fortran_FLAGS         "-cpp -lstdc++ -lmpi_cxx -DNOPRED -fast -D_USE_NVTX -gpu=cc70,lineinfo -cuda -acc -lnvToolsExt -Minfo=accel")
    set(CMAKE_Fortran_FLAGS_RELEASE "-cpp -lstdc++ -lmpi_cxx -DNOPRED -fast -D_USE_NVTX -gpu=cc70,lineinfo -cuda -acc -lnvToolsExt -Minfo=accel")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-cpp -lstdc++ -lmpi_cxx -DNOPRED -fast -D_USE_NVTX -gpu=cc70,lineinfo -cuda -acc -lnvToolsExt -Minfo=accel")
  endif()
else()
  message(" # NOT Using GPUs...")
  set(CMAKE_Fortran_FLAGS         "-cpp -lstdc++  -DNOPRED -DNOACC -O3 -xCORE-AVX512 -mtune=skylake")
  set(CMAKE_Fortran_FLAGS_RELEASE "-cpp -lstdc++  -DNOPRED -DNOACC -O3 -xCORE-AVX512 -mtune=skylake")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-cpp -lstdc++  -DNOPRED -DNOACC -O3 -xCORE-AVX512 -mtune=skylake")
endif()
