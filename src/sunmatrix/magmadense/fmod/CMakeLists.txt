# ---------------------------------------------------------------
# Programmer(s): Ashley E. Van Beusekom @ USask
# ---------------------------------------------------------------
# CMakeLists.txt file for the F2003 magmadense SUNMatrix object library
# ---------------------------------------------------------------

if(SUNDIALS_MAGMA_BACKENDS MATCHES "CUDA")
  set_source_files_properties(fsunmatrix_magmadense_mod.cpp PROPERTIES LANGUAGE CUDA)
  set(_libs_needed sundials_fnveccuda_mod)
elseif(SUNDIALS_MAGMA_BACKENDS MATCHES "HIP")
  message(STATUS "F2003 interface won't yet work with SUNDIALS_MAGMA_BACKENDS=HIP ")
  set_source_files_properties(fsunmatrix_magmadense_mod.cpp PROPERTIES LANGUAGE CXX)
  set(_libs_needed sundials_fnvechip_mod hip::device)
endif()

sundials_add_f2003_library(sundials_fsunmatrixmagmadense_mod
  SOURCES
    fsunmatrix_magmadense_mod.f90 fsunmatrix_magmadense_mod.cpp
  INCLUDE_DIRECTORIES
    PUBLIC ${MAGMA_INCLUDE_DIR}
  LINK_LIBRARIES
    PUBLIC SUNDIALS::MAGMA ${_libs_needed} sundials_fcore_mod
  OBJECT_LIBRARIES
  OUTPUT_NAME
    sundials_fsunmatrixmagmadense_mod
  VERSION
    ${sunmatrixlib_VERSION}
  SOVERSION
    ${sunmatrixlib_SOVERSION}
)

message(STATUS "Added SUNMATRIX_MAGMADENSE F2003 interface")
