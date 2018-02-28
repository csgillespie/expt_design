#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/


#include "st_aphids.h"
#include "aphids_init.h"

/*****************
Private functions
*******************/


/*GSL ODES Solver*/
int func(double t, const double y[], double f[], void *params)
{
  double *pars = (double *) params;
  double lambda = pars[0];  
  double mu = pars[1];

  f[0] = lambda*y[0] - mu*y[0]*y[1];
  f[1] = lambda*y[0];
    
  /* Variances and covariance with the Normal approximation */
  f[2] = lambda*(y[0]+2*y[2]) + mu*(y[3] -2*y[0]*y[3] +y[1]*(y[0]-2*y[2]));
  f[3] = lambda*(y[0]+y[2] +y[3]) - mu*(y[0]*y[4]+y[1]*y[3]);
  f[4] = lambda*(y[0]+2*y[3]);

  return(GSL_SUCCESS);
}



/* Public functions */
st_sim_data *init_aphids_data(int no_d) {
  st_sim_data *sim_data;
  
  sim_data = (st_sim_data *) malloc(sizeof(st_sim_data));
  sim_data->n = (double *) malloc(no_d*sizeof(double));
  sim_data->c = (double *) malloc(no_d*sizeof(double));
  sim_data->t = (double *) malloc(no_d*sizeof(double));
  sim_data->t_diff = (double *) malloc((no_d-1)*sizeof(double));
  sim_data->pars = (double *) malloc(2*sizeof(double));
  sim_data->lna_sps = (double *) malloc(5*sizeof(double));
  sim_data->no_d = no_d;
  return(sim_data);
}

st_gsl_ode *init_gsl_ode(st_sim_data *sim_data) 
{
  st_gsl_ode *gsl_ode;

  gsl_ode = (st_gsl_ode *) malloc(sizeof(st_gsl_ode));
  gsl_ode->T = gsl_odeiv2_step_rkf45;
  gsl_ode->stp =  gsl_odeiv2_step_alloc(gsl_ode->T, 5);
  gsl_ode->c =  gsl_odeiv2_control_y_new(1e-5, 0.0);
  gsl_ode->e = gsl_odeiv2_evolve_alloc(5);
  gsl_odeiv2_system sys = {func, 0, 5, sim_data->pars};
 
  gsl_ode->sys = sys;
  return(gsl_ode);
}
st_moments *init_moments() {
  st_moments *moments;
  moments = (st_moments*) malloc(sizeof(st_moments));
  moments->n = 0;
  moments->lam_m1 = 0;
  moments->mu_m1 = 0;
  moments->lam_m2 = 0;
  moments->mu_m2 = 0;
  moments->cov = 0;
  return(moments);
}

