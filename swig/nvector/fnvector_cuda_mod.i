// ---------------------------------------------------------------
// Programmer: Ashley E. Van Beusekom @ USask
// ---------------------------------------------------------------
// Swig interface file
// ---------------------------------------------------------------

%module fnvector_cuda_mod

// include code common to all nvector implementations
%include "fnvector.i"

// include the header file in the swig wrapper
%{
#include "nvector/nvector_cuda.h"
%}

// nvector_impl macro defines some ignore and inserts with the vector name appended
%nvector_impl(Cuda)

// Process and wrap functions in the following files
%include "nvector/nvector_cuda.h"

%insert("wrapper") %{
SWIGEXPORT double * _wrap_FN_VGetHostArrayPointer_Cuda(N_Vector farg1) {
  double * fresult ;
  N_Vector arg1 = (N_Vector) 0 ;
  sunrealtype *result = 0 ;

  arg1 = (N_Vector)(farg1);
  result = (sunrealtype *)N_VGetHostArrayPointer_Cuda(arg1);
  fresult = result;
  return fresult;
}

SWIGEXPORT double * _wrap_FN_VGetDeviceArrayPointer_Cuda(N_Vector farg1) {
  double * fresult ;
  N_Vector arg1 = (N_Vector) 0 ;
  sunrealtype *result = 0 ;

  arg1 = (N_Vector)(farg1);
  result = (sunrealtype *)N_VGetDeviceArrayPointer_Cuda(arg1);
  fresult = result;
  return fresult;
}
%}

%insert("fdecl") %{
 public :: FN_VGetHostArrayPointer_Cuda
 public :: FN_VGetDeviceArrayPointer_Cuda
%}

%insert("finterfaces") %{
function swigc_FN_VGetHostArrayPointer_Cuda(farg1) &
bind(C, name="_wrap_FN_VGetHostArrayPointer_Cuda") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
type(C_PTR) :: fresult
end function

function swigc_FN_VGetDeviceArrayPointer_Cuda(farg1) &
bind(C, name="_wrap_FN_VGetDeviceArrayPointer_Cuda") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
type(C_PTR) :: fresult
end function
%}

%insert("fsubprograms") %{
function FN_VGetHostArrayPointer_Cuda(v) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(N_Vector), target, intent(inout) :: v
type(C_PTR) :: fresult
type(C_PTR) :: farg1

farg1 = c_loc(v)
fresult = swigc_FN_VGetHostArrayPointer_Cuda(farg1)
call c_f_pointer(fresult, swig_result, [FN_VGetLocalLength_Cuda(v)])
end function

function FN_VGetDeviceArrayPointer_Cuda(v) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(N_Vector), target, intent(inout) :: v
type(C_PTR) :: fresult
type(C_PTR) :: farg1

farg1 = c_loc(v)
fresult = swigc_FN_VGetDeviceArrayPointer_Cuda(farg1)
call c_f_pointer(fresult, swig_result, [FN_VGetLocalLength_Cuda(v)])
end function
%}
