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

ExecPolicyPtr ThreadDirectExecPolicy_new(void* stream);
ExecPolicyPtr GridStrideExecPolicy_new(void* stream);
ExecPolicyPtr BlockReduceExecPolicy_new(void* stream);
ExecPolicyPtr BlockReduceAtomicExecPolicy_new(void* stream);

size_t ExecPolicy_gridSize(ExecPolicyPtr this, size_t numWorkUnits, size_t blockDim);
size_t ExecPolicy_blockSize(ExecPolicyPtr this, size_t numWorkUnits, size_t gridDim);

void ExecPolicy_delete(ExecPolicyPtr this);

#endif
