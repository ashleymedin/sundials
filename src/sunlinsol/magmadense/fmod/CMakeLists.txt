# ----------------------------------------------------------------------
# Programmer(s): Ashley E. Van Beusekom @ USask
# ----------------------------------------------------------------------
# CMakeLists.txt file for the F2003 magma dense SUNLinearSolver
# object library
# ----------------------------------------------------------------------

if(SUNDIALS_MAGMA_BACKENDS MATCHES "CUDA")
  set_source_files_properties(fsunlinsol_magmadense_mod.cpp PROPERTIES LANGUAGE CUDA)
  set(_libs_needed sundials_fsunmatrixmagmadense_mod sundials_fnveccuda_mod)
elseif(SUNDIALS_MAGMA_BACKENDS MATCHES "HIP")
  message(STATUS "F2003 interface won't yet work with SUNDIALS_MAGMA_BACKENDS=HIP ")
  set_source_files_properties(fsunlinsol_magmadense_mod.cpp PROPERTIES LANGUAGE CXX)
  set(_libs_needed sundials_fsunmatrixmagmadense_mod sundials_fnvechip_mod hip::device)
endif()

sundials_add_f2003_library(sundials_fsunlinsolmagmadense_mod
  SOURCES
    fsunlinsol_magmadense_mod.f90 fsunlinsol_magmadense_mod.cpp
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
