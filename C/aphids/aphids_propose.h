#ifndef APHIDS_PROPOSE_H_INCLUDED
#define APHIDS_PROPOSE_H_INCLUDED
#include "../include/st_main.h"
#include <gsl/gsl_matrix.h>
double move (gsl_rng *r);
void perturb_tps (gsl_rng *r, st_mcmc_npar *times);
void sample_row(int row, st_mcmc_npar *times, gsl_matrix *prop_particles);
int check_tps (st_mcmc_npar *times);
void aphids_propose_pars(gsl_rng *r, double *pars);
#endif
