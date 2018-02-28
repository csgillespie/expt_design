#include "propose_time.h"

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

void sample_row(int row, st_mcmc_npar *times, gsl_matrix *prop_particles)
{
  int k, no_tps;
  no_tps = times->n;
  for(k=0; k<no_tps; k++) {
    times->prop[k] = gsl_matrix_get (prop_particles, row, k);
    printf("Time: %f\n", times->prop[k]);
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
