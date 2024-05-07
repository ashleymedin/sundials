# ----------------------------------------------------------------------
# Programmer(s): Ashley E. Van Beusekom @ USask
# ----------------------------------------------------------------------
# CMakeLists.txt file for the F2003 magma dense SUNLinearSolver
# object library
# ----------------------------------------------------------------------

sundials_add_f2003_library(sundials_fsunlinsolmagmadense_mod
  SOURCES
    fsunlinsol_magmadense_mod.f90 fsunlinsol_magmadense_mod.c
  LINK_LIBRARIES
    PUBLIC sundials_fcore_mod
  OBJECT_LIBRARIES
  LINK_LIBRARIES
    PUBLIC sundials_fsunmatrixdense_mod
  OUTPUT_NAME
    sundials_fsunlinsolmagmadense_mod
  VERSION
    ${sunlinsollib_VERSION}
  SOVERSION
    ${sunlinsollib_SOVERSION}
)
message(STATUS "Added SUNLINSOL_MAGMADENSE F2003 interface")
