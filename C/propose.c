#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>
#include <gsl/gsl_matrix.h>

#include "include/st_moments.h"
#include "include/st_mcmc_par.h"
#include "include/st_gsl_ode.h"
#include "include/st_sim_data.h"



/*
 * Proposal functions
 */


double move (gsl_rng *r) 
{
  int state = 0;
  double u = gsl_rng_uniform(r);
  
  if(u < 0.5) {
    state++;
  } else {
    state--;
  }
  return(state);
}

int check_tps (st_mcmc_npar *times) 
{
  int k, no_tps;
  no_tps = times->n;
  int accept = 1;
  for(k=1; k<no_tps; k++) {
    if((times->prop[k] - times->prop[k-1]) < 0.5 ) {
      accept = 0;
      //  printf("BAD: %d,%f,%f \n", k, times->prop[k], times->prop[k-1]);
    }
  }
  return(accept);
}

void perturb_tps (gsl_rng *r, st_mcmc_npar *times)
{
  int k, no_tps;
  no_tps = times->n;

  for(k=1; k<no_tps; k++) {
    times->prop[k] = times->cur[k] + move(r);
  }
}

int pick_row(gsl_rng *r, int N) 
{
  int i;
  double u = gsl_rng_uniform(r);
  double total = 0;
  for(i=0; i<N; i++) {
    total += 1.0/N;
    if(u < total) {
      return(i);
    }
  }
  exit(GSL_FAILURE);
}

void sample_row(int row, 
                st_mcmc_npar *times, 
                gsl_matrix *prop_particles)
{
 int k, no_tps;
  no_tps = times->n;
  for(k=0; k<no_tps; k++) {
    times->prop[k] = gsl_matrix_get (prop_particles, row, k);
  }
}

int propose_tps (gsl_rng *r, 
                   st_mcmc_npar *times, 
                   gsl_matrix *prop_particles, 
                   int i) 
{
  int accept, row;
  double u = gsl_rng_uniform(r);
  
  if(i > 0 && u > 0.8) {
    row = pick_row(r, prop_particles->size1);
    sample_row(row, times, prop_particles);
  } else {
    perturb_tps(r, times);
  }
  accept = check_tps(times);
  return(accept);
}

void swapParticles(gsl_matrix **m1, gsl_matrix **m2) {
  gsl_matrix *tmp = *m1;
  *m1 = *m2;
  *m2 = tmp;
}

 
