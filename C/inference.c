#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sort_vector.h>


#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/st_main.h"

#include "hash.h"
#include "init.h"
#include "print.h"
#include "particles.h"
#include "inference.h"

/* Private */
/* int get_num_sims(int thread_id, int no_threads, int no_of_sims) {
 *   int sim_per_thread, left_over;
 * 
 *   sim_per_thread = (int) no_of_sims/no_threads;
 *   left_over = no_of_sims % no_threads;
 *   if((left_over - thread_id - 0.5) > 0) {
 *     sim_per_thread++;
 *   }
 *   return(sim_per_thread);
 * } */



/* void update_proposal_wts(int N, double max_wts, 
 *                         double *wts_prop, double *wts_cur, 
 *                         double power_up) 
 * {
 *   int j;
 *   for(j=0; j<N; j++) {
 *       wts_prop[j] = wts_prop[j] - max_wts;
 *       wts_prop[j] = exp(wts_prop[j]);
 *       wts_prop[j] = pow(wts_prop[j], (power_up + 1.0)/power_up);
 *       wts_cur[j] = wts_prop[j];
 *     }
 * } */





/* Public */
int mcmc(int no_d, int no_threads, int N, 
         int max_levels, double *levels, int verbose) 
{
  int i, j, k;
  int rc;
  int no_of_iters =0;
  st_parallel *parallel = init_parallel(no_threads, no_d);
  st_thread **threads = parallel->thread_pts;
  
  gsl_matrix *particles = gsl_matrix_alloc(N, no_d+1);
  gsl_vector *sample_size = gsl_vector_alloc(N);
  gsl_vector *utilities = gsl_vector_alloc(N);
  double time_pt;
  for(i=0; i<N; i++) {
    gsl_vector_set(sample_size, i, 0);
    gsl_vector_set(utilities, i, 0);
  }
  
  double max_utility = 50;
 
  //st_mcmc_npar *times = parallel->init_times(no_d);
  st_mcmc_1par *util = initMCMC1Par(); /* St with prop and cur elements */

  /*New */
  pthread_mutex_t mutexsum;
  pthread_mutex_init(&mutexsum, NULL);
  no_of_iters = 0;
  i = 0; 
  /* Initialise var_prop */
  for(k=0; k<no_threads; k++) {
    //parallel->update_sim_times(threads[k]->data, times->cur);
    threads[k]->level = i;
    threads[k]->no_of_iters = &no_of_iters;
    // threads[k]->g = g;
    threads[k]->particles = particles;
    threads[k]->sample_size = sample_size;
    threads[k]->utilities = utilities;
    threads[k]->total_mcmc = N;
    threads[k]->mutexsum = mutexsum;
    threads[k]->max_utility = &max_utility;
    threads[k]->particle_row = 0;// Row of the particles matrix
    threads[k]->next_row = 0;
  }

  record_t *records = NULL;
  record_t **records_ptr = &records;
  
  int ii;
  gsl_vector *time_pts = gsl_vector_alloc(no_d+1);
 
  int particle_row =0;
  for(i=0; i<max_levels; i++){
    for(j=0; j<10; j++) { // Should be N/no_threads!
      
      util->cur = GSL_NEGINF;
      // power_up = i+1;//pow(2, i);
      //      no_of_iters = 0;

      if(i == 0) { // Uniform sampling
        for(ii=0; ii<N; ii++) {
          for(k=1; k<(no_d+1); k++) {
            time_pt = gsl_rng_uniform_int(threads[0]->gsl_rng_pt, 49) + 1; // Sample (0, 1, ...48)  + 1
            gsl_vector_set(time_pts, k, time_pt);
          }
          gsl_sort_vector(time_pts);
          add_times(ii, no_d, time_pts->data, records_ptr);
          for(k=1; k<(no_d + 1); k++) {
            gsl_matrix_set (particles, ii, k, gsl_vector_get(time_pts, k));
          }
        }
      } // Else weight sampling


      /* Hash find test */
     
     
      k = find_times(no_d, time_pts->data, records_ptr);

      printf("k = %d\n", k);

      return(GSL_SUCCESS);
     
      for(k=0; k<no_threads; k++) {
        if(i == 0) {
          threads[k]-> particle_row = particle_row; 
          particle_row++;
        }
        pthread_create(threads[k]->a_thread, NULL, 
                       parallel->get_utility, (void *) threads[k]);
      }


      for(k=0; k<no_threads; k++) {
        rc = pthread_join(*(threads[k]->a_thread), NULL);
        if(rc != 0) {
          printf("Join error %d\n", rc);
          exit(GSL_FAILURE);
        }
      }

    
      return(GSL_SUCCESS);
    }
    return(GSL_SUCCESS);
  }
  
  /* Memory management is almost non-existant :(
   * Just clean up after the programme ends 
   */
  gsl_matrix_free(particles);
  return(GSL_SUCCESS); 
}
