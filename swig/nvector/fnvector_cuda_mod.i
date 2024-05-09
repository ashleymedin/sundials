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

