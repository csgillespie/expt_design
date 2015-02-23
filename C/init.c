#include <stdlib.h>
/* Used for random seeds */
#include <time.h> 
#include <unistd.h>

#include <gsl/gsl_math.h> /*GSL_POSINF*/
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>
#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/st_sim_data.h"
#include "include/st_gsl_ode.h"
#include "include/st_parallel.h"
#include "include/st_moments.h"
#include "include/st_mcmc_par.h"

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

st_gsl_ode *initGSLODE(st_sim_data *sim_data) 
{
  st_gsl_ode *gsl_ode;

  gsl_ode = (st_gsl_ode *) malloc(sizeof(st_gsl_ode));
  gsl_ode->T = gsl_odeiv_step_gear2;
  gsl_ode->stp =  gsl_odeiv_step_alloc(gsl_ode->T, 5);
  gsl_ode->c =  gsl_odeiv_control_y_new(1e-4, 0.0);
  gsl_ode->e = gsl_odeiv_evolve_alloc(5);
  gsl_odeiv_system sys = {func, 0, 5, sim_data->pars};
 
  gsl_ode->sys = sys;
  return(gsl_ode);
}


st_sim_data *initSimData(int no_d) {
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



st_parallel *init_parallel(int no_threads, int no_d) 
{
  int i;
  st_parallel *parallel = (st_parallel *)  malloc(sizeof(st_parallel)); 
  st_thread **thread_pts =  (st_thread **) malloc(no_threads*sizeof(st_thread *));

  for(i=0; i<no_threads; i++) {
    /*Init*/
    thread_pts[i] = (st_thread *) malloc(sizeof(st_thread));
    
    /*Threads */
    thread_pts[i]->thread_id = i;
    thread_pts[i]->no_sims = 0;
    thread_pts[i]->thread_sem = (sem_t  *) malloc(sizeof(sem_t));
    sem_init(thread_pts[i]->thread_sem, 0, 0);
    thread_pts[i]->a_thread = (pthread_t  *) malloc(sizeof(pthread_t));
   
    /*RNG */
    thread_pts[i]->gsl_rng_pt =  gsl_rng_alloc(gsl_rng_mt19937); 
    gsl_rng_set(thread_pts[i]->gsl_rng_pt, i);

    /*Init sim data struct */
    thread_pts[i]->sim_data = initSimData(no_d);
    /* ODE */
    thread_pts[i]->gsl_ode_pt = initGSLODE(thread_pts[i]->sim_data);

    /* Utility */
    thread_pts[i]->utility = GSL_NEGINF;
  }
  
  parallel->no_threads = no_threads;
  parallel->thread_pts = thread_pts;
  return(parallel);
}

st_mcmc_npar *initMCMCnPar(int n) {

  int i;
  st_mcmc_npar *mcmc_par;
  mcmc_par = (st_mcmc_npar *) malloc(sizeof(st_mcmc_npar));
  mcmc_par->prop = (double *) malloc(n*sizeof(double));
  mcmc_par->cur = (double *) malloc(n*sizeof(double));
  mcmc_par->n = n;

  for(i=0; i<n; i++) {
    mcmc_par->prop[i] = 0;
    mcmc_par->cur[i] = 0;
  }

  return(mcmc_par);
}

void free_mcmc_n_par(st_mcmc_npar * mcmc_par){
  free(mcmc_par->prop);
  free(mcmc_par->cur);
  free(mcmc_par);
}



st_mcmc_1par *initMCMC1Par() {

  st_mcmc_1par *mcmc_par;
  mcmc_par = (st_mcmc_1par *) malloc(sizeof(st_mcmc_1par));
  mcmc_par->prop = 0;
  mcmc_par->cur = 0;

  return(mcmc_par);
}

st_moments *initMoments() {
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
