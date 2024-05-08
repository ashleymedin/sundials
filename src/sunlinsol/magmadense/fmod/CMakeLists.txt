# ----------------------------------------------------------------------
# Programmer(s): Ashley E. Van Beusekom @ USask
# ----------------------------------------------------------------------
# CMakeLists.txt file for the F2003 magma dense SUNLinearSolver
# object library
# ----------------------------------------------------------------------

if(SUNDIALS_MAGMA_BACKENDS MATCHES "CUDA")
  set_source_files_properties(sunlinsol_magmadense.cpp PROPERTIES LANGUAGE CUDA)
  set(_libs_needed sundials_sunmatrixmagmadense sundials_nveccuda)
elseif(SUNDIALS_MAGMA_BACKENDS MATCHES "HIP")
  set_source_files_properties(sunlinsol_magmadense.cpp PROPERTIES LANGUAGE CXX)
  set(_libs_needed sundials_sunmatrixmagmadense sundials_nvechip hip::device)
endif()

sundials_add_f2003_library(sundials_fsunlinsolmagmadense_mod
  SOURCES
    fsunlinsol_magmadense_mod.f90 fsunlinsol_magmadense_mod.c
  LINK_LIBRARIES
    PUBLIC sundials_fcore_mod
  OBJECT_LIBRARIES
  LINK_LIBRARIES
    PUBLIC SUNDIALS::MAGMA ${_libs_needed} sundials_fsunmatrixmagmadense_mod
  OUTPUT_NAME
    sundials_fsunlinsolmagmadense_mod
  VERSION
    ${sunlinsollib_VERSION}
  SOVERSION
    ${sunlinsollib_SOVERSION}
)

message(STATUS "Added SUNLINSOL_MAGMADENSE F2003 interface")
