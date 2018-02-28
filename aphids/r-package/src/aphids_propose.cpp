#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include "st_main.h"
//#include "propose_time.h"
#include "aphids_propose.h"

void aphids_propose_pars(gsl_rng *r, double *pars) 
{
  double p0, p1;
  p0 = pars[0]; p1 = pars[1];
  gsl_ran_bivariate_gaussian (r, 0.0009, 0.000004, 0.36, 
                              &pars[0], &pars[1]);
  pars[0] += p0; pars[1] += p1;
}

int aphids_check_tps (st_mcmc_npar *times) 
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

