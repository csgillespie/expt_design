#include <stdlib.h>
/* Used for random seeds */

#include <gsl/gsl_math.h> /*GSL_POSINF*/
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include "aphids/aphids.h"

#include "init.h"


st_parallel *init_parallel(int no_threads, int no_d) 
{
  int i;
  st_parallel *parallel = (st_parallel *)  malloc(sizeof(st_parallel)); 
  st_thread **thread_pts = (st_thread **)malloc(no_threads*sizeof(st_thread *)); 
  time_t t;

  srand((unsigned) time(&t));
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
    gsl_rng_set(thread_pts[i]->gsl_rng_pt, rand());

    /*Approximation level */
    //    thread_pts[i]->level = 0;
    /* Utility */
    thread_pts[i]->utility = GSL_NEGINF;

    thread_pts[i]->data = aphids_init_data(no_d);

    //    thread_pts[i]->update_sim_times = aphids_init_update_sim_times;
    /* /\*Init sim data struct *\/
     * thread_pts[i]->sim_data = initSimData(no_d);
     * /\* ODE *\/
     * thread_pts[i]->gsl_ode_pt = initGSLODE(thread_pts[i]->sim_data); */


    /*Model functions */
    //    thread_pts[i]->funcs = aphids_init_funcs();
  }
  
  parallel->no_threads = no_threads;
  parallel->init_times = aphids_init_times;
  parallel->propose_times = aphids_propose_times;
  parallel->update_sim_times =  aphids_update_sim_times;
  parallel->get_utility =  aphids_get_utility;
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
