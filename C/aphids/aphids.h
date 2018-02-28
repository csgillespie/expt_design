#ifndef APHIDS_H_INCLUDED
#define APHIDS_H_INCLUDED
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_rng.h>
#include "../include/st_main.h"
#include "st_aphids.h"
int aphids_propose_times (gsl_rng *r, st_mcmc_npar *times, 
                 gsl_matrix *prop_particles, gsl_ran_discrete_t *g, 
                 int level, int row);
//st_mcmc_npar *aphids_init_times(int no_d);
void *aphids_init_data(int no_d);
void aphids_update_sim_times(st_sim_data *sim_data, st_thread *thread);
void *aphids_get_utility(void *arg);


#endif
