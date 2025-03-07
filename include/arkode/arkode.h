/* -----------------------------------------------------------------
 * Programmer(s): Daniel R. Reynolds @ SMU
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
 * This is the header file for the main ARKode infrastructure.
 * -----------------------------------------------------------------
 * ARKode is used to numerically solve the ordinary initial value
 * problems using one-step methods.  Users do not call ARKode
 * infrastructure routines directly; they instead interact with
 * one of the time stepping modules built on top of ARKode.
 * These time step modules define their supported problem types,
 * solver options, etc.
 *
 * This file serves to define constants and provide function
 * prototypes for use across ARKode-based time integration
 * modules.
 * -----------------------------------------------------------------*/

#ifndef _ARKODE_H
#define _ARKODE_H

#include <arkode/arkode_butcher.h>
#include <stdio.h>
#include <sundials/sundials_core.h>

#ifdef __cplusplus /* wrapper to enable C++ usage */
extern "C" {
#endif

/* -----------------
 * ARKode Constants
 * ----------------- */

/* usage modes (itask) */
#define ARK_NORMAL   1
#define ARK_ONE_STEP 2

/* adaptivity module flags */
#define ARK_ADAPT_CUSTOM   -1
#define ARK_ADAPT_PID      0
#define ARK_ADAPT_PI       1
#define ARK_ADAPT_I        2
#define ARK_ADAPT_EXP_GUS  3
#define ARK_ADAPT_IMP_GUS  4
#define ARK_ADAPT_IMEX_GUS 5

/* Constants for evaluating the full RHS */
#define ARK_FULLRHS_START 0
#define ARK_FULLRHS_END   1
#define ARK_FULLRHS_OTHER 2

/* interpolation module flags */

/*    max allowed degree */
#define ARK_INTERP_MAX_DEGREE 5

/*    interpolation module types */
#define ARK_INTERP_HERMITE  0
#define ARK_INTERP_LAGRANGE 1

/* return values */

#define ARK_SUCCESS      0
#define ARK_TSTOP_RETURN 1
#define ARK_ROOT_RETURN  2

#define ARK_WARNING 99

#define ARK_TOO_MUCH_WORK -1
#define ARK_TOO_MUCH_ACC  -2
#define ARK_ERR_FAILURE   -3
#define ARK_CONV_FAILURE  -4

#define ARK_LINIT_FAIL        -5
#define ARK_LSETUP_FAIL       -6
#define ARK_LSOLVE_FAIL       -7
#define ARK_RHSFUNC_FAIL      -8
#define ARK_FIRST_RHSFUNC_ERR -9
#define ARK_REPTD_RHSFUNC_ERR -10
#define ARK_UNREC_RHSFUNC_ERR -11
#define ARK_RTFUNC_FAIL       -12
#define ARK_LFREE_FAIL        -13
#define ARK_MASSINIT_FAIL     -14
#define ARK_MASSSETUP_FAIL    -15
#define ARK_MASSSOLVE_FAIL    -16
#define ARK_MASSFREE_FAIL     -17
#define ARK_MASSMULT_FAIL     -18

#define ARK_CONSTR_FAIL -19
#define ARK_MEM_FAIL    -20
#define ARK_MEM_NULL    -21
#define ARK_ILL_INPUT   -22
#define ARK_NO_MALLOC   -23
#define ARK_BAD_K       -24
#define ARK_BAD_T       -25
#define ARK_BAD_DKY     -26
#define ARK_TOO_CLOSE   -27

#define ARK_VECTOROP_ERR -28

#define ARK_NLS_INIT_FAIL   -29
#define ARK_NLS_SETUP_FAIL  -30
#define ARK_NLS_SETUP_RECVR -31
#define ARK_NLS_OP_ERR      -32

#define ARK_INNERSTEP_ATTACH_ERR -33
#define ARK_INNERSTEP_FAIL       -34
#define ARK_OUTERTOINNER_FAIL    -35
#define ARK_INNERTOOUTER_FAIL    -36

/* ARK_POSTPROCESS_FAIL equals ARK_POSTPROCESS_STEP_FAIL
   for backwards compatibility */
#define ARK_POSTPROCESS_FAIL       -37
#define ARK_POSTPROCESS_STEP_FAIL  -37
#define ARK_POSTPROCESS_STAGE_FAIL -38

#define ARK_USER_PREDICT_FAIL -39
#define ARK_INTERP_FAIL       -40

#define ARK_INVALID_TABLE -41

#define ARK_CONTEXT_ERR -42

#define ARK_RELAX_FAIL      -43
#define ARK_RELAX_MEM_NULL  -44
#define ARK_RELAX_FUNC_FAIL -45
#define ARK_RELAX_JAC_FAIL  -46

#define ARK_CONTROLLER_ERR -47

#define ARK_UNRECOGNIZED_ERROR -99

/* ------------------------------
 * User-Supplied Function Types
 * ------------------------------ */

typedef int (*ARKRhsFn)(sunrealtype t, N_Vector y, N_Vector ydot,
                        void* user_data);

typedef int (*ARKRootFn)(sunrealtype t, N_Vector y, sunrealtype* gout,
                         void* user_data);

typedef int (*ARKEwtFn)(N_Vector y, N_Vector ewt, void* user_data);

typedef int (*ARKRwtFn)(N_Vector y, N_Vector rwt, void* user_data);

typedef int (*ARKAdaptFn)(N_Vector y, sunrealtype t, sunrealtype h1,
                          sunrealtype h2, sunrealtype h3, sunrealtype e1,
                          sunrealtype e2, sunrealtype e3, int q, int p,
                          sunrealtype* hnew, void* user_data);

typedef int (*ARKExpStabFn)(N_Vector y, sunrealtype t, sunrealtype* hstab,
                            void* user_data);

typedef int (*ARKVecResizeFn)(N_Vector y, N_Vector ytemplate, void* user_data);

typedef int (*ARKPostProcessFn)(sunrealtype t, N_Vector y, void* user_data);

typedef int (*ARKStagePredictFn)(sunrealtype t, N_Vector zpred, void* user_data);

typedef int (*ARKRelaxFn)(N_Vector y, sunrealtype* r, void* user_data);

typedef int (*ARKRelaxJacFn)(N_Vector y, N_Vector J, void* user_data);

/* --------------------------
 * MRIStep Inner Stepper Type
 * -------------------------- */

typedef _SUNDIALS_STRUCT_ _MRIStepInnerStepper* MRIStepInnerStepper;

/* --------------------------
 * Relaxation Solver Options
 * -------------------------- */

typedef enum
{
  ARK_RELAX_BRENT,
  ARK_RELAX_NEWTON
} ARKRelaxSolver;

#ifdef __cplusplus
}
#endif

#endif
