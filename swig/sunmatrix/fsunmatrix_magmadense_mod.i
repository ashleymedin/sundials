// ---------------------------------------------------------------
// Programmer: Ashley E. Van Beusekom @ USask
// ---------------------------------------------------------------
// Swig interface file
// ---------------------------------------------------------------

%module fsunmatrix_magmadense_mod

// include code common to all nvector implementations
%include "fsunmatrix.i"

%{
#include "sunmatrix/sunmatrix_magmadense.h"
%}

// sunmatrix_impl macro defines some ignore and inserts with the matrix name appended
%sunmatrix_impl(MagmaDense)

// we ignore the following functions because we manually insert them to ensure the
// the returned arrays have the correct shape
%ignore SUNMatrix_MagmaDense_Data;
%ignore SUNMatrix_MagmaDense_BlockData;
%ignore SUNMatrix_MagmaDense_Block;
%ignore SUNMatrix_MagmaDense_Column;
%ignore SUNMatrix_MagmaDense_BlockColumn;

// Process and wrap functions in the following files
%include "sunmatrix/sunmatrix_magmadense.h"

%insert("wrapper") %{
SWIGEXPORT double * _wrap_FSUNMatrix_MagmaDense_Data(SUNMatrix farg1) {
  double * fresult ;
  SUNMatrix arg1 = (SUNMatrix) 0 ;
  sunrealtype *result = 0 ;
  
  arg1 = (SUNMatrix)(farg1);
  result = (sunrealtype *)SUNMatrix_MagmaDense_Data(arg1);
  fresult = result;
  return fresult;
}

SWIGEXPORT double * _wrap_FSUNMatrix_MagmaDense_BlockData(SUNMatrix farg1) {
  double * fresult ;
  SUNMatrix arg1 = (SUNMatrix) 0 ;
  sunrealtype *result = 0 ;
  
  arg1 = (SUNMatrix)(farg1);
  result = (sunrealtype *)SUNMatrix_MagmaDense_BlockData(arg1);
  fresult = result;
  return fresult;
}

SWIGEXPORT double * _wrap_FSUNMatrix_MagmaDense_Block(SUNMatrix farg1, int64_t const *farg2) {
  double * fresult ;
  SUNMatrix arg1 = (SUNMatrix) 0 ;
  sunindextype arg2 ;
  sunrealtype *result = 0 ;
  
  arg1 = (SUNMatrix)(farg1);
  arg2 = (sunindextype)(*farg2);
  result = (sunrealtype *)SUNMatrix_MagmaDense_Block(arg1,arg2);
  fresult = result;
  return fresult;
}

SWIGEXPORT double * _wrap_FSUNMatrix_MagmaDense_Column(SUNMatrix farg1, int64_t const *farg2) {
  double * fresult ;
  SUNMatrix arg1 = (SUNMatrix) 0 ;
  sunindextype arg2 ;
  sunrealtype *result = 0 ;
  
  arg1 = (SUNMatrix)(farg1);
  arg2 = (sunindextype)(*farg2);
  result = (sunrealtype *)SUNMatrix_MagmaDense_Column(arg1,arg2);
  fresult = result;
  return fresult;
}

SWIGEXPORT double * _wrap_FSUNMatrix_MagmaDense_BlockColumn(SUNMatrix farg1, int64_t const *farg2, int64_t const *farg3) {
  double * fresult ;
  SUNMatrix arg1 = (SUNMatrix) 0 ;
  sunindextype arg2 ;
  sunindextype arg3 ;
  sunrealtype *result = 0 ;
  
  arg1 = (SUNMatrix)(farg1);
  arg2 = (sunindextype)(*farg2);
  arg3 = (sunindextype)(*farg3);
  result = (sunrealtype *)SUNMatrix_MagmaDense_BlockColumn(arg1,arg2,arg3);
  fresult = result;
  return fresult;
}
%}

%insert("fdecl") %{
 public :: FSUNMatrix_MagmaDense_Data
 public :: FSUNMatrix_MagmaDense_BlockData
 public :: FSUNMatrix_MagmaDense_Block
 public :: FSUNMatrix_MagmaDense_Column
 public :: FSUNMatrix_MagmaDense_BlockColumn
%}

%insert("finterfaces") %{
function swigc_FSUNMatrix_MagmaDense_Data(farg1) &
bind(C, name="_wrap_FSUNMatrix_MagmaDense_Data") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
type(C_PTR) :: fresult
end function

function swigc_FSUNMatrix_MagmaDense_BlockData(farg1) &
bind(C, name="_wrap_FSUNMatrix_MagmaDense_BlockData") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
type(C_PTR) :: fresult
end function

function swigc_FSUNMatrix_MagmaDense_Block(farg1, farg2) &
bind(C, name="_wrap_FSUNMatrix_MagmaDense_Block") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
integer(C_INT64_T), intent(in) :: farg2
type(C_PTR) :: fresult
end function

function swigc_FSUNMatrix_MagmaDense_Column(farg1, farg2) &
bind(C, name="_wrap_FSUNMatrix_MagmaDense_Column") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
integer(C_INT64_T), intent(in) :: farg2
type(C_PTR) :: fresult
end function

function swigc_FSUNMatrix_MagmaDense_BlockColumn(farg1, farg2, farg3) &
bind(C, name="_wrap_FSUNMatrix_MagmaDense_BlockColumn") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
integer(C_INT64_T), intent(in) :: farg2
integer(C_INT64_T), intent(in) :: farg3
type(C_PTR) :: fresult
end function
%}

%insert("fsubprograms") %{
function FSUNMatrix_MagmaDense_Data(a) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(SUNMatrix), target, intent(inout) :: a
type(C_PTR) :: fresult 
type(C_PTR) :: farg1 

farg1 = c_loc(a)
fresult = swigc_FSUNMatrix_MagmaDense_Data(farg1)
call c_f_pointer(fresult, swig_result, [FSUNMatrix_MagmaDense_LData(a)])
end function

function FSUNMatrix_MagmaDense_BlockData(a) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(SUNMatrix), target, intent(inout) :: a
type(C_PTR) :: fresult 
type(C_PTR) :: farg1

farg1 = c_loc(a)
fresult = swigc_FSUNMatrix_MagmaDense_BlockData(farg1)
call c_f_pointer(fresult, swig_result, [FSUNMatrix_MagmaDense_NumBlocks(a)])
end function

function FSUNMatrix_MagmaDense_Block(a, k) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(SUNMatrix), target, intent(inout) :: a
integer(C_INT64_T), intent(in) :: k
type(C_PTR) :: fresult 
type(C_PTR) :: farg1 
integer(C_INT64_T) :: farg2 

farg1 = c_loc(a)
farg2 = k
fresult = swigc_FSUNMatrix_MagmaDense_Block(farg1, farg2)
call c_f_pointer(fresult, swig_result,[FSUNMatrix_MagmaDense_LData(a)/FSUNMatrix_MagmaDense_NumBlocks(a)])
end function

function FSUNMatrix_MagmaDense_Column(a, j) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(SUNMatrix), target, intent(inout) :: a
integer(C_INT64_T), intent(in) :: j
type(C_PTR) :: fresult 
type(C_PTR) :: farg1 
integer(C_INT64_T) :: farg2 

farg1 = c_loc(a)
farg2 = j
fresult = swigc_FSUNMatrix_MagmaDense_Column(farg1, farg2)
call c_f_pointer(fresult, swig_result, [FSUNMatrix_MagmaDense_Rows(a)])
end function

function FSUNMatrix_MagmaDense_BlockColumn(a, k, j) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(SUNMatrix), target, intent(inout) :: a
integer(C_INT64_T), intent(in) :: k
integer(C_INT64_T), intent(in) :: j
type(C_PTR) :: fresult
type(C_PTR) :: farg1
integer(C_INT64_T) :: farg2
integer(C_INT64_T) :: farg3

farg1 = c_loc(a)
farg2 = k
farg3 = j
fresult = swigc_FSUNMatrix_MagmaDense_BlockColumn(farg1, farg2, farg3)
call c_f_pointer(fresult, swig_result, [FSUNMatrix_MagmaDense_BlockRows(a)])
end function
%}
