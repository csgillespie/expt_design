#ifndef ST_APHIDS_H_INCLUDED
#define ST_APHIDS_H_INCLUDED
#include <gsl/gsl_odeiv2.h>

typedef struct
{
  double *n; //Length D
  double *c; //Length D
  double *t; //Length D
  double *t_diff; //Length D-1: time differences
  double *pars; // Length 2

  double *lna_sps; // Length 5
  int no_d;
} st_sim_data;

typedef struct
{
  const gsl_odeiv2_step_type *T;
  gsl_odeiv2_step *stp;
  gsl_odeiv2_control *c;
  gsl_odeiv2_evolve *e;
  gsl_odeiv2_system sys;
} st_gsl_ode;

typedef struct
{
  st_sim_data *sim_data; /*Struct for storing simulations */
  st_gsl_ode *gsl_ode_pt;
} st_aphids_data;


typedef struct
{
  int n;
  double lam_m1;
  double mu_m1;
  double lam_m2;
  double mu_m2;
  double cov;
} st_moments;
#endif
