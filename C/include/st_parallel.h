/* 
 * Data structure for parallel computing
 * Make thread safe
 */


typedef struct
{
  int thread_id;
  int no_sims;
  sem_t *thread_sem;
  pthread_t *a_thread;
  st_gsl_ode *gsl_ode_pt;
  gsl_rng *gsl_rng_pt;/*RNG for each thread */
  st_sim_data *sim_data; /*Struct for storing simulations */
  double utility;
} st_thread;

typedef struct
{
  int no_threads;
  st_thread ** thread_pts;
} st_parallel;



/* typedef struct
 * {
 *   st_gsl_ode **gsl_ode_pt;
 *   gsl_rng **r_pt;
 *   st_thread **thread_pt;
 *   int no_threads;
 * } st_parallel; */



