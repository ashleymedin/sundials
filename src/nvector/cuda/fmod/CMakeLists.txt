# ---------------------------------------------------------------
# Programmer(s): Ashley E. Van Beusekom @ USask
# ---------------------------------------------------------------
# CMakeLists.txt file for the F2003 cuda NVECTOR object library
# ---------------------------------------------------------------

set(nveccuda_SOURCES fnvector_cuda_mod.f90 fnvector_cuda_mod.cu)
set_source_files_properties(fnvector_cuda_mod.cu PROPERTIES LANGUAGE CUDA)

sundials_add_f2003_library(sundials_fnveccuda_mod
  SOURCES
    fnvector_cuda_mod.f90 fnvector_cuda_mod.cu
  LINK_LIBRARIES
    PUBLIC sundials_fcore_mod
  OBJECT_LIBRARIES
  OUTPUT_NAME
    sundials_fnveccuda_mod
  VERSION
    ${nveclib_VERSION}
  SOVERSION
    ${nveclib_SOVERSION}
)

message(STATUS "Added NVECTOR_CUDA F2003 interface")
