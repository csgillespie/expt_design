#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sort_vector.h>

#include "../include/st_main.h"
#include "../include/init.h"
#include "../hash.h"

//#include "st_model_funcs.h"
#include "st_aphids.h"
#include "aphids_inference.h"
#include "aphids_prior.h"
#include "aphids_simulation.h"
#include "aphids_init.h"
#include "aphids_print.h"
#include "aphids_propose.h"
#include "../include/propose_time.h"
#include "aphids.h"



double *get_wts(int N) 
{
  double *wts = malloc(N*sizeof(double));
  int i;
  for(i=0; i<N; i++) wts[i] = 1;
  return(wts);
}


void update_proposal_wts(int N, double max_wts,double *wts, double power_up) 
{
  int j;
  for(j=0; j<N; j++) {
      wts[j] = wts[j] - max_wts;
      wts[j] = exp(wts[j]);
      wts[j] = pow(wts[j], (power_up + 1.0)/power_up);
    }
}

void *aphids_init_data(int no_d)
{
  st_aphids_data *aphids_data;
  aphids_data = (st_aphids_data *) malloc(sizeof(st_aphids_data));
  aphids_data->sim_data = init_aphids_data(no_d);
  aphids_data->gsl_ode_pt = init_gsl_ode(aphids_data->sim_data);
  return((void *)aphids_data);
}

void aphids_initialise_times(gsl_rng *r, gsl_matrix *particles, int row) 
{
  
  int k;
  int no_d = particles->size2; // includes time_0
  gsl_vector *time_pts = gsl_vector_alloc(no_d);
  double time_pt;
  for(k=1; k<no_d; k++) {
    time_pt = gsl_rng_uniform_int(r, 49) + 1; // Sample (0, 1, ...48)  + 1
    gsl_vector_set(time_pts, k, time_pt);
  }
  gsl_sort_vector(time_pts);
  for(k=0; k<no_d; k++) {
     gsl_matrix_set (particles, row, k, gsl_vector_get(time_pts, k));
  }
  gsl_vector_free(time_pts);
}

int aphids_propose_times (gsl_rng *r, st_mcmc_npar *times, 
                 gsl_matrix *prop_particles, gsl_ran_discrete_t *g, 
                 int level, int row) 
{
  int accept=0;
  accept = 0;
  if(level == 0) {
    aphids_initialise_times(r, prop_particles, row);
  } else {
    perturb_tps(r, times);
  }
  //accept = aphids_check_tps(times);
  return(accept);
}

void aphids_update_sim_times(st_sim_data *sim_data, st_thread *thread) 
{
  //  st_aphids_data *aphids_data  = (st_aphids_data *) data;
  // st_sim_data *sim_data = aphids_data->sim_data;

  int particle_row = thread->particle_row;
  gsl_matrix *particles = thread->particles;

  int i;
  int no_d = sim_data->no_d;
  sim_data->t[0] = 0;
  for(i=1; i<(no_d + 1); i++) {
    sim_data->t[i] = gsl_matrix_get(particles, particle_row, i);
    sim_data->t_diff[i-1] = sim_data->t[i] - sim_data->t[i-1];
   }
}

void *aphids_get_utility(void *arg) 
{
  /* Unpack struct */
  st_thread *thread = (st_thread *) arg;
  gsl_rng *r = thread->gsl_rng_pt;
  st_aphids_data *data = (st_aphids_data *) thread->data;
  st_sim_data *sim_data = data->sim_data;
  st_gsl_ode *gsl_ode = data->gsl_ode_pt;
  //  int no_sims = thread->no_sims;

  /* Useful other variables */
  double utility = 0;
  //   int i;
  int no_of_iters = (thread->no_of_iters)[0];
  pthread_mutex_t mutexsum = thread->mutexsum;
  int N = thread->total_mcmc;
  double *wts = get_wts(N);
  gsl_ran_discrete_t *g = gsl_ran_discrete_preproc(N, (const double *) wts);
  gsl_vector *sample_size = thread->sample_size;
  double old_utility, old_sample_size, new_utility;
   while(no_of_iters< N) {
    /* Get time */
    pthread_mutex_lock (&mutexsum);

    aphids_propose_times(r, thread->times, 
                         thread->particles, 
                         g, 
                         thread->level,
                        thread->next_row);
    thread->next_row++;
    pthread_mutex_unlock (&mutexsum);
    aphids_update_sim_times(sim_data, thread);

    /* Update parameter, IC, sim */
    aphids_sample_priors(r, sim_data);
    aphids_sample_ic(r, sim_data);
    aphids_sim(r, sim_data, gsl_ode, 2); 
    aph_print_sim_n(sim_data);
    /* Get utility */
    utility = aphids_calculate_utility(r, gsl_ode, sim_data);

   
    /* Update utility estimate */
    pthread_mutex_lock (&mutexsum);
    if(utility > thread->max_utility[0]) thread->max_utility[0] = utility;
    int particle_row = thread->particle_row;
    old_sample_size = gsl_vector_get(thread->sample_size, particle_row);
    gsl_vector_set(sample_size, particle_row,  
                   old_sample_size + 1);
    old_utility = gsl_vector_get(thread->utilities, particle_row);
      

    new_utility = (old_sample_size*old_utility + utility)/(old_sample_size + 1);
    gsl_vector_set(sample_size, particle_row, old_sample_size + 1);
    gsl_vector_set(thread->utilities, particle_row, new_utility);
    thread->no_of_iters[0] =  thread->no_of_iters[0] + 1;
    no_of_iters = thread->no_of_iters[0];
    pthread_mutex_unlock (&mutexsum);
  
    gsl_ran_discrete_free(g);

    update_proposal_wts(N, thread->max_utility[0], 
                        wts, thread->level);

    g = gsl_ran_discrete_preproc(N, (const double *) wts);
  }

  gsl_ran_discrete_free(g);
  free(wts);
  pthread_exit(NULL);
}

