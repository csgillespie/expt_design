#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include "../include/st_main.h"
#include "aphids_propose.h"

void aphids_propose_pars(gsl_rng *r, double *pars) 
{
  double p0, p1;
  p0 = pars[0]; p1 = pars[1];
  gsl_ran_bivariate_gaussian (r, 0.0009, 0.000004, 0.36, 
                              &pars[0], &pars[1]);
  pars[0] += p0; pars[1] += p1;
}


double move (gsl_rng *r) 
{
  int state = 0;
  double u = gsl_rng_uniform(r);
  if(u < 1.0/3) {
    state++;
  } else if(u < 2.0/3) {
    state--;
  }
  return(state);
}

int check_tps (st_mcmc_npar *times) 
{
  int k, no_tps, accept;
  no_tps = times->n;
  accept = 1;

  /* Reject if time points are equal or are above 7 weeks  */
  for(k=1; k<no_tps; k++) {
    if((times->prop[k] - times->prop[k-1]) < 0.5 || times->prop[k] > 49.5) {
      accept = 0;
    }
  }
  return(accept);
}


void sample_row(int row, st_mcmc_npar *times, gsl_matrix *prop_particles)
{
  int k, no_tps;
  no_tps = times->n;
  for(k=0; k<no_tps; k++) {
    times->prop[k] = gsl_matrix_get (prop_particles, row, k);
  }
}

void perturb_tps (gsl_rng *r, st_mcmc_npar *times)
{
  int k, no_tps;
  no_tps = times->n;
  for(k=1; k<no_tps; k++) {
    times->prop[k] = times->cur[k] + move(r);
  }
}
