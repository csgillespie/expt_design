#ifndef APHIDS_PRIOR_H_INCLUDED
#define APHIDS_PRIOR_H_INCLUDED
#include <gsl/gsl_rng.h>
void aphids_init_pars(double *pars);
void aphids_sample_ic(gsl_rng * r, st_sim_data *sim_data);
void aphids_sample_priors(gsl_rng * r, st_sim_data *sim_data);
double aphids_eval_priors(double *pars);

#endif
