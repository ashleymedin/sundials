/* ----------------------------------------------------------------------------
 * Programmer(s): Cody J. Balos @ LLNL
 * ----------------------------------------------------------------------------
 * SUNDIALS Copyright Start
 * Copyright (c) 2002-2023, Lawrence Livermore National Security
 * and Southern Methodist University.
 * All rights reserved.
 *
 * See the top-level LICENSE and NOTICE files for details.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 * SUNDIALS Copyright End
 * ----------------------------------------------------------------------------
 * The following is a simple example problem based off of ark_brusselator.c.
 *
 * The following test simulates a brusselator problem from chemical
 * kinetics.  This is an ODE system with 3 components, Y = [u,v,w],
 * satisfying the equations,
 *    du/dt = a - (w+1)*u + v*u^2
 *    dv/dt = w*u - v*u^2
 *    dw/dt = (b-w)/ep - w*u
 * for t in the interval [0.0, 10.0], with initial conditions Y0 = [u0,v0,w0].
 * The problem is stiff. We have 3 different testing scenarios:
 *
 * Reactor 0:  u0=3.9,  v0=1.1,  w0=2.8,  a=1.2,  b=2.5,  ep=1.0e-5
 *    Here, all three components exhibit a rapid transient change
 *    during the first 0.2 time units, followed by a slow and
 *    smooth evolution.
 *
 * Reactor 1:  u0=3,  v0=3,  w0=3.5,  a=0.5,  b=3,  ep=5.0e-4
 *    Here, all components undergo very rapid initial transients
 *    during the first 0.3 time units, and all then proceed very
 *    smoothly for the remainder of the simulation.

 * Reactor 2:  u0=1.2,  v0=3.1,  w0=3,  a=1,  b=3.5,  ep=5.0e-6
 *    Here, w experiences a fast initial transient, jumping 0.5
 *    within a few steps.  All values proceed smoothly until
 *    around t=6.5, when both u and v undergo a sharp transition,
 *    with u increaseing from around 0.5 to 5 and v decreasing
 *    from around 6 to 1 in less than 0.5 time units.  After this
 *    transition, both u and v continue to evolve somewhat
 *    rapidly for another 1.4 time units, and finish off smoothly.
 *
 * This program solves the problem with the BDF method, Newton iteration,
 * and either a dense direct linear solver or a Krylov solver.
 * 100 outputs are printed at equal intervals, and run statistics are
 * printed at the end.
 *
 * The program takes three optional arguments, the linear solver type
 * (dense direct, GMRES with the Jacobian computed by difference quotients,
 * or GMRES with analytical Jacobian), and the reactor type.
 *
 *    ./cv_brusselator [solver_type] [reactor_type]
 *
 * Options:
 *    num_batches <int>
 *    solver_type:
 *       0 - SUNDIALS GMRES with difference quotients Jacobian
 *       1 - SUNDIALS GMRES with analytical Jacobian
 *       2 - SUNDIALS dense direct linear solver with analytical Jacobian
 *    reactor_type:
 *       0 - Reactor 0
 *       1 - Reactor 1
 *       2 - Reactor 2
 * --------------------------------------------------------------------------*/

#include <memory>
#include <vector>
#include <cstdio>

#include <cvode/cvode.h>
#include <nvector/nvector_serial.h>
#include <sunlinsol/sunlinsol_spgmr.h>
#include <sundials/sundials_core.hpp>


/* Functions Called by the Solver */
static int f(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data);

static int Jac(sunrealtype t, N_Vector y, N_Vector fy, SUNMatrix J,
               void *user_data, N_Vector tmp1, N_Vector tmp2, N_Vector tmp3);

/* Private function to output results */
static void PrintOutput(sunrealtype t, sunrealtype y1, sunrealtype y2, sunrealtype y3);

/* Private function to print final statistics */
static void PrintFinalStats(void *cvode_mem, SUNLinearSolver LS);

/* Private function to check function return values */
static int check_retval(void *returnvalue, const char *funcname, int opt);

struct UserData {
  sunrealtype u0, v0, w0; /* initial conditions */
  sunrealtype a, b;       /* chemical concentrations that are constant */
  sunrealtype ep;         /* stiffness ratio */
};

/*
 *-------------------------------
 * Main Program
 *-------------------------------
 */

int main(int argc, char *argv[])
{
  const sunrealtype T0 = SUN_RCONST(0.0);     /* initial time                   */
  const sunrealtype Tf = SUN_RCONST(10.0);    /* final time                    */
  const sunrealtype dTout = SUN_RCONST(1.0);  /* time between outputs          */
  const int Nt = (int) ceil(Tf/dTout);    /* number of output times        */
  const sunrealtype reltol = 1.0e-4;      /* relative integrator tolerance */
  int retval;
  N_Vector y, abstol;
  SUNMatrix A;
  SUNLinearSolver LS;
  void *cvode_mem;

  y = abstol = NULL;
  A = NULL;
  LS = NULL;
  cvode_mem = NULL;

  /* Create the SUNDIALS context */
  sundials::Context sunctx;

  /* Set defaults */
  int reactor_type = 2;
  int solver_type = 0;

  /* Parse command line arguments and setup UserData */
  int argi = 0;
  if (argc > 1) {
    solver_type = atoi(argv[++argi]);
  }
  if (argc > 2) {
    reactor_type = atoi(argv[++argi]);
  }

  /* Set the Reaction parameters according to reactor_type */
  UserData udata;
  if (reactor_type == 0) {
    udata.u0 = SUN_RCONST(3.9);
    udata.v0 = SUN_RCONST(1.1);
    udata.w0 = SUN_RCONST(2.8);
    udata.a  = SUN_RCONST(1.2);
    udata.b  = SUN_RCONST(2.5);
    udata.ep = SUN_RCONST(1.0e-5);
  } else if (reactor_type == 1) {
    udata.u0 = SUN_RCONST(3.0);
    udata.v0 = SUN_RCONST(3.0);
    udata.w0 = SUN_RCONST(3.5);
    udata.a  = SUN_RCONST(0.5);
    udata.b  = SUN_RCONST(3.0);
    udata.ep = SUN_RCONST(5.0e-4);
  } else if (reactor_type == 2) {
    udata.u0 = SUN_RCONST(1.2);
    udata.v0 = SUN_RCONST(3.1);
    udata.w0 = SUN_RCONST(3.0);
    udata.a  = SUN_RCONST(1.0);
    udata.b  = SUN_RCONST(3.5);
    udata.ep = SUN_RCONST(5.0e-6);
  }

  /* Create for initial conditions the and absolute tolerance vector */
  y = N_VNew_Serial(3, sunctx);
  abstol = N_VClone(y);

  /* Initialize y */
  sunrealtype* ydata = N_VGetArrayPointer(y);
  ydata[0] = udata.u0;
  ydata[1] = udata.v0;
  ydata[2] = udata.w0;

  /* Set the vector absolute tolerance */
  sunrealtype* abstol_data = N_VGetArrayPointer(abstol);
  abstol_data[0] = 1.0e-10;
  abstol_data[1] = 1.0e-10;
  abstol_data[2] = 1.0e-10;

  /* Call CVodeCreate to create the solver memory and specify the
   * Backward Differentiation Formula */
  cvode_mem = CVodeCreate(CV_BDF, sunctx);
  if (check_retval((void *)cvode_mem, "CVodeCreate", 0)) { return(1); }

  /* Call CVodeInit to initialize the integrator memory and specify the
   * user's right hand side function in y'=f(t,y), the inital time T0, and
   * the initial dependent variable vector y. */
  retval = CVodeInit(cvode_mem, f, T0, y);
  if (check_retval(&retval, "CVodeInit", 1)) { return(1); }

  /* Call CVodeSetUserData to attach the user data structure */
  retval = CVodeSetUserData(cvode_mem, &udata);
  if (check_retval(&retval, "CVodeSetUserData", 1)) { return(1); }

  /* Call CVodeSVtolerances to specify the scalar relative tolerance
   * and vector absolute tolerances */
  retval = CVodeSVtolerances(cvode_mem, reltol, abstol);
  if (check_retval(&retval, "CVodeSVtolerances", 1)) { return(1); }

  /* Create the SUNLinearSolver object for use by CVode */
  if (solver_type == 0) {
    LS = SUNLinSol_SPGMR(y, SUN_PREC_NONE, 0, sunctx);
    if(check_retval((void *)LS, "SUNLinSol_SPGMR", 0)) return(1);

    /* Call CVodeSetLinearSolver to attach the matrix and linear solver to CVode */
    retval = CVodeSetLinearSolver(cvode_mem, LS, NULL);
    if(check_retval(&retval, "CVodeSetLinearSolver", 1)) return(1);
  }

  /* In loop, call CVode, print results, and reactor_type for error.
     Break out of loop when preset output times have been reached.  */
  printf(" \nBrusselator problem\n\n");

  sunrealtype t = T0;
  sunrealtype tout = T0+dTout;
  for (int iout=0; iout<Nt; iout++) {
    retval = CVode(cvode_mem, tout, y, &t, CV_NORMAL);
    PrintOutput(t, ydata[0], ydata[1], ydata[2]);
    if (check_retval(&retval, "CVode", 1)) break;
    if (retval == CV_SUCCESS) {
      tout += dTout;
      tout = (tout > Tf) ? Tf : tout;
    }
  }

  /* Print some final statistics */
  printf("\nFinal Statistics:\n");
  CVodePrintAllStats(cvode_mem, stdout, SUN_OUTPUTFORMAT_TABLE);

  /* Free y and abstol vectors */
  N_VDestroy(y);
  N_VDestroy(abstol);

  /* Free integrator memory */
  CVodeFree(&cvode_mem);

  /* Free the linear solver memory */
  SUNLinSolFree(LS);

  /* Free the matrix memory */
  if (A) { SUNMatDestroy(A); }

  return(0);
}


/*
 *-------------------------------
 * Functions called by the solver
 *-------------------------------
 */

/* Right hand side function. This just launches the CUDA or HIP kernel
   to do the actual computation. At the very least, doing this
   saves moving the vector data in y and ydot to/from the device
   every evaluation of f. */
int f(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data)
{
  UserData *udata;
  sunrealtype *ydata, *ydotdata;
  sunrealtype u, v, w, a, b, ep;

  udata = (UserData*) user_data;
  ydata = N_VGetArrayPointer(y);
  ydotdata = N_VGetArrayPointer(ydot);

  a = udata->a; b = udata->b, ep = udata->ep;

  u = ydata[0];
  v = ydata[1];
  w = ydata[2];

  ydotdata[0] = a - (w+1.0)*u + v*u*u;
  ydotdata[1] = w*u - v*u*u;
  ydotdata[2] = (b-w)/ep - w*u;

  return(0);
}

/*
 * Jacobian routine. Compute J(t,y) = df/dy.
 * This is done on the GPU.
 */

int Jac(sunrealtype t, N_Vector y, N_Vector fy, SUNMatrix J,
        void *user_data, N_Vector tmp1, N_Vector tmp2, N_Vector tmp3)
{
  UserData *udata = (UserData*) user_data;
  sunrealtype *ydata, *Jdata;
  sunrealtype u, v, w, a, b, ep;

  ydata = N_VGetArrayPointer(y);
  // Jdata = ?

  a = udata->a; b = udata->b, ep = udata->ep;

  /* get y values */
  u = ydata[0];
  v = ydata[1];
  w = ydata[2];

  /* first col of block */
  Jdata[0]       = -(w+1.0) + 2.0*u*v;
  Jdata[1]   = u*u;
  Jdata[2]   = -u;

  /* second col of block */
  Jdata[3]   = u*u;
  Jdata[4]   = -u*u;
  Jdata[5]   = u;

  /* third col of block */
  Jdata[6]   = -w;
  Jdata[7]   = 0.0;
  Jdata[8]   = -1.0/ep - u;

  return(0);
}

/*
 *-------------------------------
 * Private helper functions
 *-------------------------------
 */

void PrintOutput(sunrealtype t, sunrealtype y1, sunrealtype y2, sunrealtype y3)
{
#if defined(SUNDIALS_DOUBLE_PRECISION)
  printf("At t = %0.4e      y =%14.6e  %14.6e  %14.6e\n", t, y1, y2, y3);
#else
  printf("At t = %0.4e      y =%14.6e  %14.6e  %14.6e\n", t, y1, y2, y3);
#endif

  return;
}

/*
 * Check function return value...
 *   opt == 0 means SUNDIALS function allocates memory so check if
 *            returned NULL pointer
 *   opt == 1 means SUNDIALS function returns an integer value so check if
 *            retval < 0
 *   opt == 2 means function allocates memory so check if returned
 *            NULL pointer
 */

int check_retval(void *returnvalue, const char *funcname, int opt)
{
  int *retval;

  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && returnvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s(failed - returned NULL pointer\n\n",
	    funcname);
    return(1); }

  /* Check if retval < 0 */
  else if (opt == 1) {
    retval = (int *) returnvalue;
    if (*retval < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with retval = %d\n\n",
	      funcname, *retval);
      return(1); }}

  /* Check if function returned NULL pointer - no memory allocated */
  else if (opt == 2 && returnvalue == NULL) {
    fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return(1); }

  return(0);
}
