#ifndef APHIDS_SIMULATION_H_INCLUDED
#define APHIDS_SIMULATION_H_INCLUDED
#include <gsl/gsl_rng.h>
#include "st_aphids.h"
void aphids_sim(gsl_rng *r, st_sim_data *sim_data, 
                st_gsl_ode *gsl_ode, int level);
int aphids_ode(st_gsl_ode *gsl_ode, st_sim_data *sim_data, int i);
#endif
