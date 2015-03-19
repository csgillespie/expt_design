#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_matrix.h>

#include "../include/st_main.h"
#include "../include/init.h"

//#include "st_model_funcs.h"
#include "st_aphids.h"
#include "aphids_likelihood.h"
#include "aphids_prior.h"
#include "aphids_simulation.h"
#include "aphids_init.h"
#include "aphids_print.h"
#include "aphids_propose.h"
#include "aphids.h"




/*Public init function */

/* st_model_funcs *aphids_init_funcs() 
 * {
 * 
 *   st_model_funcs *model_funcs = malloc(sizeof(st_model_funcs));
 *   model_funcs->propose = &aphids_propose_pars;
 *   model_funcs->calculate_utility = &aphids_calculate_utility;
 *   model_funcs->simulate = aphids_simulate;
 *   return(model_funcs);
 * } */

void *aphids_init_data(int no_d)
{
  st_aphids_data *aphids_data;
  aphids_data = (st_aphids_data *) malloc(sizeof(st_aphids_data));
  aphids_data->sim_data = init_aphids_data(no_d);
  aphids_data->gsl_ode_pt = init_gsl_ode(aphids_data->sim_data);
  return((void *)aphids_data);
}




int aphids_propose_times (gsl_rng *r, st_mcmc_npar *times, 
                 gsl_matrix *prop_particles, gsl_ran_discrete_t *g, 
                 int level, int *row) 
{
  int accept=0;
  accept = 0;
  if(level > 0) {
    row[0] = gsl_ran_discrete(r, g);
    sample_row(row[0], times, prop_particles);
  } else {
    perturb_tps(r, times);
    accept = check_tps(times);
  }
  accept = check_tps(times);
  return(accept);
}

st_mcmc_npar *aphids_init_times(int no_d) 
{
  int i;
  st_mcmc_npar *times = initMCMCnPar(no_d);

  for(i=0; i<no_d; i++) {
    times->cur[i] = (int) (i*49.0/no_d);
    times->prop[i] = times->cur[i];
  }
  return(times);
}



void aphids_update_sim_times(void *data, double *times) 
{
  st_aphids_data *aphids_data  = (st_aphids_data *) data;
  st_sim_data *sim_data = aphids_data->sim_data;

  int i;
  int no_d = sim_data->no_d;
  sim_data->t[0] = times[0];
  for(i=1; i<no_d; i++) {
    sim_data->t[i] = times[i];
    sim_data->t_diff[i-1] = times[i] - times[i-1];
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
  int no_sims = thread->no_sims;

  /* Useful other variables */
  double utility = 0;
  int i;
 
  for(i=0; i<no_sims; i++) {
    aphids_sample_priors(r, sim_data); /*Sample and update */
    aphids_sample_ic(r, sim_data);
    aphids_sim(r, sim_data, gsl_ode, 2); /*New simulation */
    utility += aphids_calculate_utility(r, gsl_ode, sim_data);
  }

  thread->utility = utility;
  pthread_exit(NULL);
}

