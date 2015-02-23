/*
  Struct used to store the ODE initialisation.
  More efficient to create once and re-use
*/
typedef struct
{
  const gsl_odeiv_step_type *T;
  gsl_odeiv_step *stp;
  gsl_odeiv_control *c;
  gsl_odeiv_evolve *e;
  gsl_odeiv_system sys;
} st_gsl_ode;
