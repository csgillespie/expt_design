/*
  Struct used to store the ODE initialisation.
  More efficient to create once and re-use
*/
typedef struct
{
  const gsl_odeiv2_step_type *T;
  gsl_odeiv2_step *stp;
  gsl_odeiv2_control *c;
  gsl_odeiv2_evolve *e;
  gsl_odeiv2_system sys;
} st_gsl_ode;
