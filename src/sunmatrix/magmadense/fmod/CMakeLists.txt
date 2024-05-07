# ---------------------------------------------------------------
# Programmer(s): Ashley E. Van Beusekom @ USask
# ---------------------------------------------------------------
# CMakeLists.txt file for the F2003 magmadense SUNMatrix object library
# ---------------------------------------------------------------

sundials_add_f2003_library(sundials_fsunmatrix_magmadense_mod
  SOURCES
    fsunmatrix_magmadense_mod.f90 fsunmatrix_magmadense_mod.c
  LINK_LIBRARIES
    PUBLIC sundials_fcore_mod
  OBJECT_LIBRARIES
  OUTPUT_NAME
    sundials_fsunmatrixmagmadense_mod
  VERSION
    ${sunmatrixlib_VERSION}
  SOVERSION
    ${sunmatrixlib_SOVERSION}
)

message(STATUS "Added SUNMATRIX_MAGMADENSE F2003 interface")
