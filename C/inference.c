#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>
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

/* TODO:
 * Implement particle filter
 */

int get_num_sims(int thread_id, int no_threads, int no_of_sims) {
  int sim_per_thread, left_over;
  sim_per_thread = (int) no_of_sims/no_threads;
  left_over = no_of_sims % no_threads;
  if((left_over - thread_id - 0.5) > 0) {
    sim_per_thread++;
  }
  return(sim_per_thread);
}



int inferTime(int no_d, int no_threads) {

  int i, j, k;
  int rc;
  double u, compare_tp;

  st_parallel *parallel = init_parallel(no_threads, no_d);
  st_thread *thread0 = parallel->thread_pts[0];
  st_thread **threads = parallel->thread_pts;
  st_thread *thread;

  //Borrow the RNG from thread 0 for main loop 
  gsl_rng *r = thread0->gsl_rng_pt;

  st_mcmc_npar *times = initMCMCnPar(no_d);
  times->cur[0] = 0;  times->cur[1] = 8; times->cur[2] = 16;
  times->cur[3] = 25; times->cur[4] = 32; times->cur[5] = 42;


  /* times->cur[0] = 0;  times->cur[1] = 1; times->cur[2] = 13;
   * times->cur[3] = 16; times->cur[4] = 31; times->cur[5] = 45; */

  for(i=0; i<no_d; i++) {
    times->prop[i] = times->cur[i];
  }

  st_mcmc_1par *util = initMCMC1Par();
  st_mcmc_npar *pars = initMCMCnPar(2);
  sample_priors(r, pars->cur);

  /* Initialise var_prop */
  for(k=0; k<no_threads; k++){
    updateSimICs(threads[k]->sim_data, 28, 28);
    updateSimTimes(threads[k]->sim_data, times->cur);
    odeSim(threads[k]->gsl_ode_pt, threads[k]->sim_data);
  }
  //get_utility(r, gsl_ode, sim_data, GSL_POSINF);  //GSL_POSINF
  int accept = 1;
  int N = 10000;
  int power_up;
  int no_tps =2;
  times->n = no_tps;
 
  gsl_matrix *prop_particles = gsl_matrix_alloc(N, no_d);
  gsl_matrix *cur_particles = gsl_matrix_alloc(N, no_d);
  
  for(i=0; i<10; i++){
    util->cur = GSL_NEGINF;
    power_up = pow(i+7, 2);
    /*Distribute sims among threads */
    for(k=0; k<no_threads; k++) {
      thread = parallel->thread_pts[k];
      thread->no_sims = get_num_sims(k, no_threads, power_up);
    }

    for(j=0; j<N; j++) {
      accept = 1;
      accept = propose_tps(r, times, prop_particles, i);

      if(accept > 0.5){
	for(k=0; k<no_threads; k++) {
	  updateSimTimes(threads[k]->sim_data, times->prop);
	  pthread_create(threads[k]->a_thread, NULL, 
			 threadSimulation, (void *) threads[k]);
	}

	util->prop = 1;       
	for(k=0; k<no_threads; k++) {
	  rc = pthread_join(*(threads[k]->a_thread), NULL);
	  if(rc != 0){
	    printf("1.join error %d\n", rc);
	    exit(1);
	  }
	  util->prop *= threads[k]->utility;
	  //        printf("util %f\n", util->prop);
	}
      }

      u = gsl_rng_uniform(r);  
      compare_tp = u*util->cur;
      if(util->prop > compare_tp && accept > 0.5) {
        util->cur = util->prop;
        for(k=1; k<(no_tps+1); k++) {
          times->cur[k] = times->prop[k];
        }
      }
      for(k=0; k<no_d; k++) {
        gsl_matrix_set(cur_particles, j, k, times->cur[k]);
      }

      printf("%d,%f,%f,%f,%f,%f,%f\n", 
             j, 
             times->cur[1], times->cur[2], times->cur[3], times->cur[4],times->cur[5],util->cur);
      fflush(stdout);
    }
    saveParticles(cur_particles, i);
    swapParticles(&prop_particles, &cur_particles);
  }
  gsl_matrix_free(cur_particles);
  gsl_matrix_free(prop_particles);
 
  return(GSL_SUCCESS); 
}
