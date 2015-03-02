#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_matrix.h>

#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/st_moments.h"
#include "include/st_mcmc_par.h"
#include "include/st_gsl_ode.h"
#include "include/st_sim_data.h"
#include "include/st_parallel.h"


#include "include/simulation.h"
#include "include/update.h"
#include "include/likelihood.h"
#include "include/ODE.h"
#include "include/init.h"
#include "include/propose.h"
#include "include/prior.h"
#include "include/print.h"
#include "include/particles.h"

/* Private */
int get_num_sims(int thread_id, int no_threads, int no_of_sims, int level) {
  int sim_per_thread, left_over;
  if(level == 0) {
    if(thread_id == 0) {
      return(no_of_sims);
    } else {
      return(0);
    }
  }

  sim_per_thread = (int) no_of_sims/no_threads;
  left_over = no_of_sims % no_threads;
  if((left_over - thread_id - 0.5) > 0) {
    sim_per_thread++;
  }
  return(sim_per_thread);
}

st_mcmc_npar *init_times(int no_d) 
{
  int i;
  st_mcmc_npar *times = initMCMCnPar(no_d);
  times->cur[0] = 0;  times->cur[1] = 17; times->cur[2] = 30;
  //  times->cur[3] = 42;// times->cur[4] = 42; //times->cur[5] = 42;

  for(i=0; i<no_d; i++)
    times->prop[i] = times->cur[i];
  return(times);
}

void *update_thread_times(st_thread **threads, double *times, int no_threads)
{
  int k;
  for(k=0; k<no_threads; k++){
    updateSimICs(threads[k]->sim_data, 28, 28);
    updateSimTimes(threads[k]->sim_data, times);
  }
}

/* Public */
int mcmc(int no_d, int no_threads, int N) {

  int i, j, k;
  int rc, no_sims, accept = 1;
  int no_accept, level=0;
  double u, compare_tp, power_up;

  st_parallel *parallel = init_parallel(no_threads, no_d);
  st_thread *thread0 = parallel->thread_pts[0];
  st_thread **threads = parallel->thread_pts;
  st_thread *thread;

  //Borrow the RNG from thread 0 for main loop 
  gsl_rng *r = thread0->gsl_rng_pt;
  gsl_rng_set(thread0->gsl_rng_pt, 0);

  gsl_matrix *prop_particles = gsl_matrix_alloc(N, no_d+1);
  gsl_matrix *cur_particles = gsl_matrix_alloc(N, no_d+1);
  st_mcmc_npar *times = init_times(no_d);
  st_mcmc_1par *util = initMCMC1Par();

  /* Initialise var_prop */
  update_thread_times(threads, times->cur, no_threads);
 

  int max_power_up=7;
  int levels[8] = {0, 0, 1, 2, 2, 2, 2};

  for(i=0; i<max_power_up; i++){
    util->cur = GSL_NEGINF;
    power_up = i+3;//pow(2, i);
    
    level = levels[i];
    /*Distribute sims among threads */
    for(k=0; k<no_threads; k++) {
      thread = parallel->thread_pts[k];
      thread->no_sims = get_num_sims(k, no_threads, power_up, level);
    }
    no_accept = 0;

    for(j=0; j<N; j++) {
      gsl_rng_set(thread0->gsl_rng_pt, j+N*i);
      accept = propose_tps(r, times, prop_particles, i);
      if(accept > 0.5){
        k=0; no_sims = threads[k]->no_sims;
        while(k<no_threads && no_sims > 0){
	  updateSimTimes(threads[k]->sim_data, times->prop);
	  pthread_create(threads[k]->a_thread, NULL, 
			 threadSimulation, (void *) threads[k]);
          k++;
          no_sims = threads[k]->no_sims;
	}
      

	util->prop = 0;       
        k=0; no_sims = threads[k]->no_sims;
        while(k<no_threads && no_sims > 0){
	  rc = pthread_join(*(threads[k]->a_thread), NULL);
	  if(rc != 0){
	    printf("1.join error %d\n", rc);
	    exit(GSL_FAILURE);
	  }
	  util->prop += threads[k]->utility;
          k++;
          no_sims = threads[k]->no_sims;
	}
      }
      u = gsl_rng_uniform(r);  
      compare_tp = log(u) + util->cur;
      if(util->prop > compare_tp && accept > 0.5) {
        util->cur = util->prop;
        for(k=1; k<(no_d); k++) {
          times->cur[k] = times->prop[k];
        }
        no_accept++;
      }
      update_particles(j, cur_particles, times->cur, util->cur);
      
      print_mcmc_status(j, no_d, power_up, times->cur,
                        util, no_accept);

    }
    save_particles(cur_particles, i);
    swap_particles(&prop_particles, &cur_particles);
   }
  gsl_matrix_free(cur_particles);
  gsl_matrix_free(prop_particles);
 
  return(GSL_SUCCESS); 
}