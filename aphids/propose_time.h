#ifndef PROPOSE_TIME_H_INCLUDED
#define PROPOSE_TIME_H_INCLUDED
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_rng.h>
#include "st_main.h"
double move (gsl_rng *r);
void sample_row(int row, st_mcmc_npar *times, gsl_matrix *prop_particles);
void perturb_tps (gsl_rng *r, st_mcmc_npar *times);
#endif
