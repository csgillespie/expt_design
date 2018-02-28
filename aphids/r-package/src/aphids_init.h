#ifndef APHIDS_INIT_H_INCLUDED
#define APHIDS_INIT_H_INCLUDED
#include "st_aphids.h"
st_sim_data *init_aphids_data(int no_d);
st_gsl_ode *init_gsl_ode(st_sim_data *sim_data);
st_moments *init_moments();
#endif
