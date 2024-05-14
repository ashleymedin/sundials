// ---------------------------------------------------------------
// Programmer: Ashley E. Van Beusekom @ USask
// ---------------------------------------------------------------
// Swig interface file
// ---------------------------------------------------------------

%module fnvector_cuda_mod

%ignore N_VSetKernelExecPolicy_Cuda;
//%apply void* { SUNCudaFExecPolicy * };

// fake interface file so that Swig can generate the correct wrapper code
%import "sundials/fsundials_cuda_policies_mod.i" 

// include code common to all nvector implementations
%include "fnvector.i"

// include the header file in the swig wrapper
%{
#include "sundials/sundials_cuda_policies.hpp"
#include "nvector/nvector_cuda.h"
%}

// nvector_impl macro defines some ignore and inserts with the vector name appended
%nvector_impl(Cuda)

// Process and wrap functions in the following files
%include "nvector/nvector_cuda.h"

%insert("wrapper") %{
SWIGEXPORT double * _wrap_FN_VGetDeviceArrayPointer_Cuda(N_Vector farg1) {
  double * fresult ;
  N_Vector arg1 = (N_Vector) 0 ;
  sunrealtype *result = 0 ;

  arg1 = (N_Vector)(farg1);
  result = (sunrealtype *)N_VGetDeviceArrayPointer_Cuda(arg1);
  fresult = result;
  return fresult;
}

SWIGEXPORT double * _wrap_FN_VGetHostArrayPointer_Cuda(N_Vector farg1) {
  double * fresult ;
  N_Vector arg1 = (N_Vector) 0 ;
  sunrealtype *result = 0 ;

  arg1 = (N_Vector)(farg1);
  result = (sunrealtype *)N_VGetHostArrayPointer_Cuda(arg1);
  fresult = result;
  return fresult;
}

SWIGEXPORT int _wrap_FN_VSetKernelExecPolicy_Cuda(N_Vector farg1, SUNCudaExecPolicy *farg2, SUNCudaExecPolicy *farg3) {
  int fresult ;
  N_Vector arg1 = (N_Vector) 0 ;
  SUNCudaExecPolicy *arg2 = (SUNCudaExecPolicy *) 0 ;
  SUNCudaExecPolicy *arg3 = (SUNCudaExecPolicy *) 0 ;
  SUNErrCode result;
  
  arg1 = (N_Vector)(farg1);
  arg2 = (SUNCudaExecPolicy *)(farg2->cptr);
  arg3 = (SUNCudaExecPolicy *)(farg3->cptr);
  result = (SUNErrCode)N_VSetKernelExecPolicy_Cuda(arg1,arg2,arg3);
  fresult = (SUNErrCode)(result);
  return fresult;
}
%}

%insert("fdecl") %{
 public :: FN_VGetDeviceArrayPointer_Cuda
 public :: FN_VGetHostArrayPointer_Cuda 
 public :: FN_VSetKernelExecPolicy_Cuda
%}

%insert("finterfaces") %{
function swigc_FN_VGetDeviceArrayPointer_Cuda(farg1) &
bind(C, name="_wrap_FN_VGetDeviceArrayPointer_Cuda") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
type(C_PTR) :: fresult
end function

function swigc_FN_VGetHostArrayPointer_Cuda(farg1) &
bind(C, name="_wrap_FN_VGetHostArrayPointer_Cuda") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
type(C_PTR) :: fresult
end function

use fsundials_cuda_policies_mod
function swigc_FN_VSetKernelExecPolicy_Cuda(farg1, farg2, farg3) &
bind(C, name="_wrap_FN_VSetKernelExecPolicy_Cuda") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
type(C_PTR), value :: farg1
class(FExecPolicy), pointer :: farg2
class(FExecPolicy), pointer :: farg3
integer(C_INT) :: fresult
end function
%}

%insert("fsubprograms") %{
function FN_VGetDeviceArrayPointer_Cuda(v) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(N_Vector), target, intent(inout) :: v
type(C_PTR) :: fresult
type(C_PTR) :: farg1

farg1 = c_loc(v)
fresult = swigc_FN_VGetDeviceArrayPointer_Cuda(farg1)
call c_f_pointer(fresult, swig_result, [FN_VGetLength_Cuda(v)])
end function

function FN_VGetHostArrayPointer_Cuda(v) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
real(C_DOUBLE), dimension(:), pointer :: swig_result
type(N_Vector), target, intent(inout) :: v
type(C_PTR) :: fresult
type(C_PTR) :: farg1

farg1 = c_loc(v)
fresult = swigc_FN_VGetHostArrayPointer_Cuda(farg1)
call c_f_pointer(fresult, swig_result, [FN_VGetLength_Cuda(v)])
end function

function FN_VSetKernelExecPolicy_Cuda(x, stream_exec_policy, reduce_exec_policy) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
integer(C_INT) :: swig_result
type(N_Vector), target, intent(inout) :: x
class(FExecPolicy), intent(in) :: stream_exec_policy
class(FExecPolicy), intent(in) :: reduce_exec_policy
integer(C_INT) :: fresult 
type(C_PTR) :: farg1 
class(FExecPolicy), pointer :: farg2
class(FExecPolicy), pointer :: farg3

farg1 = c_loc(x)

select type (stream_exec_policy)
type is (SUNCudaThreadDirectFExecPolicy)
    farg2 => stream_exec_policy
end select

select type (reduce_exec_policy)
type is (SUNCudaBlockReduceFExecPolicy)
    farg3 => reduce_exec_policy
end select

fresult = swigc_FN_VSetKernelExecPolicy_Cuda(farg1, farg2, farg3)
swig_result = fresult
end function
%}
