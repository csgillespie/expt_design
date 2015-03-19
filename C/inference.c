#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_matrix.h>

#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/st_main.h"

#include "init.h"
#include "print.h"
#include "particles.h"
#include "inference.h"

/* Private */
int get_num_sims(int thread_id, int no_threads, int no_of_sims) {
  int sim_per_thread, left_over;

  sim_per_thread = (int) no_of_sims/no_threads;
  left_over = no_of_sims % no_threads;
  if((left_over - thread_id - 0.5) > 0) {
    sim_per_thread++;
  }
  return(sim_per_thread);
}

double *get_wts(int N) 
{
  double *wts = malloc(N*sizeof(double));
  int i;
  for(i=0; i<N; i++) wts[i] = 1;
  return(wts);
}

void update_proposal_wts(int N, double max_wts, 
                        double *wts_prop, double *wts_cur, 
                        double power_up) 
{
  int j;
  for(j=0; j<N; j++) {
      wts_prop[j] = wts_prop[j] - max_wts;
      wts_prop[j] = exp(wts_prop[j]);
      wts_prop[j] = pow(wts_prop[j], (power_up + 1.0)/power_up);
      wts_cur[j] = wts_prop[j];
    }
}


/* Public */
int mcmc(int no_d, int no_threads, int N, 
         int max_levels, double *levels, int verbose) 
{

  int i, j, k;
  int rc, no_sims, accept = 1;
  int no_accept;
  int row_prop=0, row_cur=0;
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
  double max_wts = 1;
  double *wts_cur = get_wts(N);
  double *wts_prop = get_wts(N);

  gsl_ran_discrete_t *g = gsl_ran_discrete_preproc(N, (const double *) wts_cur);
  st_mcmc_npar *times = parallel->init_times(no_d);
  st_mcmc_1par *util = initMCMC1Par();

  /* Initialise var_prop */
  for(k=0; k<no_threads; k++) {
    parallel->update_sim_times(threads[k]->data, times->cur);
  }
  
  for(i=0; i<max_levels; i++){
    util->cur = GSL_NEGINF;
    power_up = i+1;//pow(2, i);
    
    /*Distribute sims among threads */
    for(k=0; k<no_threads; k++) {
      thread = parallel->thread_pts[k];
      thread->no_sims = get_num_sims(k, no_threads, power_up);
    }
    no_accept = 0;
  
    for(j=0; j<N; j++) {
      gsl_rng_set(thread0->gsl_rng_pt, j+N*i);
      accept = parallel->propose_times(r, times, prop_particles, g, i, &row_prop);
      if(accept > 0.5){
        k=0; no_sims = threads[k]->no_sims;
        while(k<no_threads && no_sims > 0){
          no_sims = threads[k]->no_sims;
          parallel->update_sim_times(threads[k]->data, times->prop);
          pthread_create(threads[k]->a_thread, NULL, 
                	 parallel->get_utility, (void *) threads[k]);
          k++;
        }

        util->prop = 0;       
        k=0; no_sims = threads[k]->no_sims;
        while(k<no_threads && no_sims > 0){
          no_sims = threads[k]->no_sims;
          rc = pthread_join(*(threads[k]->a_thread), NULL);
          if(rc != 0){
            printf("Join error %d\n", rc);
            exit(GSL_FAILURE);
          }
          util->prop += threads[k]->utility;
          k++;
        }
      }

      u = gsl_rng_uniform(r);  
      compare_tp = log(u) + util->cur + log(wts_cur[row_prop]);
     
      if(util->prop + log(wts_cur[row_cur]) > compare_tp && accept > 0.5) {
        util->cur = util->prop; row_cur = row_prop;
        for(k=1; k<(no_d); k++) {
          times->cur[k] = times->prop[k];
        }
        no_accept++;
      }
     
      update_particles(j, cur_particles, times->cur, util->cur);
      wts_prop[j] = util->cur;
      if(wts_prop[j] > max_wts) max_wts = wts_prop[j];
      if(verbose)
        print_mcmc_status(j, no_d, power_up, times->cur,
                          util, no_accept);
    }
  
    save_particles(cur_particles, i);
    gsl_ran_discrete_free(g);
    update_proposal_wts(N, max_wts, wts_prop, wts_cur, power_up);
   
    g = gsl_ran_discrete_preproc(N, (const double *) wts_cur);
  
    swap_particles(&prop_particles, &cur_particles);
   }
  
  /* Memory management is almost non-existant :(
   * Just clean up after the programme ends 
   */
  gsl_ran_discrete_free(g);
  gsl_matrix_free(cur_particles);
  gsl_matrix_free(prop_particles);
  free(levels);
  free(wts_prop);
  free(wts_cur);
  return(GSL_SUCCESS); 
}
