/* -----------------------------------------------------------------
 * Programmer(s): Cody J. Balos @ LLNL
 * -----------------------------------------------------------------
 * SUNDIALS Copyright Start
 * Copyright (c) 2002-2024, Lawrence Livermore National Security
 * and Southern Methodist University.
 * All rights reserved.
 *
 * See the top-level LICENSE and NOTICE files for details.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 * SUNDIALS Copyright End
 * -----------------------------------------------------------------
 * This header file contains common utility functions.
 * -----------------------------------------------------------------*/

#ifndef _SUNDIALS_UTILS_H
#define _SUNDIALS_UTILS_H

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sundials/sundials_core.h>

#include "sundials/priv/sundials_errors_impl.h"
#include "sundials/sundials_allocator.h"
#include "sundials/sundials_errors.h"

/* ----------------------------------------- *
 * Allocator malloc and free utilities       *
 * ----------------------------------------- */

static inline void* sunMalloc(SUNContext sunctx, size_t mem_size)
{
  return SUNAllocator_Allocate(sunctx->allocator, mem_size, SUNMEMTYPE_HOST);
}

static inline void sunFree(SUNContext sunctx, void* mem_ptr, size_t mem_size)
{
  SUNAllocator_Deallocate(sunctx->allocator, mem_ptr, mem_size, SUNMEMTYPE_HOST);
}

/* ----------------------------------------- *
 * Vector creation and destruction utilities *
 * ----------------------------------------- */

static inline SUNErrCode sunVec_Clone(N_Vector tmpl, N_Vector* v)
{
  if (v == NULL) { return SUN_ERR_ARG_CORRUPT; }
  if (*v != NULL) { return SUN_SUCCESS; }
  *v = N_VClone(tmpl);
  if (*v == NULL) { return SUN_ERR_MEM_FAIL; }
  tmpl->sunctx->vec_count++;
  return SUN_SUCCESS;
}

static inline SUNErrCode sunVec_Destroy(N_Vector* v)
{
  if (v == NULL) { return SUN_SUCCESS; }
  if (*v == NULL) { return SUN_SUCCESS; }
  SUNContext sunctx = (*v)->sunctx;
  N_VDestroy(*v);
  *v = NULL;
  sunctx->vec_count--;
  return SUN_SUCCESS;
}

static inline SUNErrCode sunVecArray_Clone(int count, N_Vector tmpl, N_Vector** v)
{
  if (count < 0) { return SUN_ERR_ARG_OUTOFRANGE; }
  if (v == NULL) { return SUN_ERR_ARG_CORRUPT; }
  if (*v != NULL) { return SUN_SUCCESS; }
  if (count == 0) { return SUN_SUCCESS; }
  *v = N_VCloneVectorArray(count, tmpl);
  if (*v == NULL) { return SUN_ERR_MEM_FAIL; }
  tmpl->sunctx->vec_count += count;
  return SUN_SUCCESS;
}

static inline SUNErrCode sunVecArray_Destroy(int count, N_Vector** v)
{
  if (count < 0) { return SUN_ERR_ARG_OUTOFRANGE; }
  if (v == NULL) { return SUN_SUCCESS; }
  if (*v == NULL) { return SUN_SUCCESS; }
  if (count == 0) { return SUN_SUCCESS; }
  SUNContext sunctx = ((*v)[0])->sunctx;
  N_VDestroyVectorArray(*v, count);
  *v = NULL;
  sunctx->vec_count -= count;
  return SUN_SUCCESS;
}

/* ------------------ *
 * Printing utilities *
 * ------------------ */

static inline char* sunCombineFileAndLine(int line, const char* file)
{
  size_t total_str_len = strlen(file) + 6;
  char* file_and_line  = (char*)malloc(total_str_len * sizeof(char));
  snprintf(file_and_line, total_str_len, "%s:%d", file, line);
  return file_and_line;
}

static inline int sunvsnprintf(char* buffer, size_t bufsz, const char* format,
                               va_list vlist)
{
  int size = 0;
  va_list tmp;
  va_copy(tmp, vlist);
  size = vsnprintf(buffer, bufsz, format, tmp);
  va_end(tmp);
  return size;
}

static inline int sunsnprintf(char* buffer, size_t bufsz, const char* format, ...)
{
  int size = 0;
  va_list args;
  va_start(args, format);
  size = sunvsnprintf(buffer, bufsz, format, args);
  va_end(args);
  return size;
}

/*
 * Implementation of the GNU extension function vasprintf which
 * is itself an analog for vsprintf, except it allocates a string
 * large enough to hold the output byte ('\0').
 */
static inline int sunvasnprintf(char** str, const char* fmt, va_list args)
{
  int size = 0;

  /* compute string length */
  size = sunvsnprintf(NULL, 0, fmt, args);

  if (size < 0) { return -1; }

  /* add one to size for the null terminator*/
  *str = (char*)malloc(size + 1);
  if (NULL == *str) { return -1; }

  size = vsprintf(*str, fmt, args);

  return size;
}

static inline void sunCompensatedSum(sunrealtype base, sunrealtype inc,
                                     sunrealtype* sum, sunrealtype* error)
{
  sunrealtype err           = *error;
  volatile sunrealtype tmp1 = inc - err;
  volatile sunrealtype tmp2 = base + tmp1;
  *error                    = (tmp2 - base) - tmp1;
  *sum                      = tmp2;
}

#endif /* _SUNDIALS_UTILS_H */
