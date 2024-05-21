/*
 * -----------------------------------------------------------------
 * Programmer: Ashley E. Van Beusekom @ USask
 * -----------------------------------------------------------------
 * This header file defines the ExecPolicy classes which
 * are utilized to determine CUDA kernel launch parameters.
 * ----------------------------------------------------------------*/

#ifndef _SUNDIALS_CUDA_POLICIES_H
#define _SUNDIALS_CUDA_POLICIES_H

#include <sundials/sundials_types.h>

typedef void* ExecPolicyPtr;

ExecPolicyPtr ExecPolicy_new(cudaStream_t stream);
size_t ExecPolicy_gridSize(ExecPolicyPtr this, size_t numWorkUnits, size_t blockDim);
size_t ExecPolicy_blockSize(ExecPolicyPtr this, size_t numWorkUnits, size_t gridDim);
void ExecPolicy_delete(ExecPolicyPtr this);

#endif