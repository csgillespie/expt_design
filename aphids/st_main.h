/*
  Struct used the proposed and current value in mcmc loop.
  E.g. Lambda: lambda_prop & lambda_cur
*/
#ifndef ST_MCMC_PAR_H_INCLUDED
#define ST_MCMC_PAR_H_INCLUDED
#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_matrix.h>
typedef struct
{
  double *prop;
  double *cur;
  int n;
} st_mcmc_npar;


typedef struct
{
  double prop;
  double cur;
} st_mcmc_1par;

/* Parallel structs */

typedef struct
{
  int thread_id;
  int no_sims;
  sem_t *thread_sem;
  pthread_t *a_thread;
  gsl_rng *gsl_rng_pt;/*RNG for each thread */
  double utility;
  int level;/*Only updates for each new level of J*/
  int *no_of_iters; /*Needs to be shared */
  int total_mcmc;/*Only updates for each new level of J*/
  pthread_mutex_t mutexsum;
  double *max_utility;

  //  st_gsl_ode *gsl_ode_pt;
  //  st_sim_data *sim_data; /*Struct for storing simulations */
  //  int level; /*approximation level */
  void *data;

  //void (*forwardSimulate)(st_part_at* prop_part, double tstep);;
  //  void update_thread_times;
  //  st_model_funcs *funcs;

  st_mcmc_npar *times;
  gsl_matrix *particles;
  gsl_vector *sample_size;
  gsl_vector *utilities;
  //  gsl_ran_discrete_t *g;
  st_mcmc_npar *(*init_times)(int no_d);
  int (*propose_times)(gsl_rng *r, st_mcmc_npar *times, 
                    gsl_matrix *prop_particles, gsl_ran_discrete_t *g, 
                    int level, int *row) ;
  void (*update_sim_times)(void *data, double *times);

} st_thread;

typedef struct
{
  int no_threads;

  void *(*get_utility)(void *arg);

  st_thread ** thread_pts;
} st_parallel;

#endif
