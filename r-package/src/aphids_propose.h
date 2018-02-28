#ifndef APHIDS_PROPOSE_H_INCLUDED
#define APHIDS_PROPOSE_H_INCLUDED
#include <gsl/gsl_rng.h>
#include "st_main.h"
int aphids_check_tps (st_mcmc_npar *times);
void aphids_propose_pars(gsl_rng *r, double *pars);
#endif
