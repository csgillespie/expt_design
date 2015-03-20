#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include "st_aphids.h"
#include "aphids_prior.h"

/* 
 * Prior from G & G Aphid paper
 * Means: 0.246, 0.000134
 * SDs, cor: 0.00079, 0.000002, 0.356
 */

/* Public functions */


void aphids_sample_ic(gsl_rng * r, st_sim_data *sim_data) 
{
  sim_data->n[0] = 28;
  sim_data->c[0] = 28;
}

void aphids_init_pars(double *pars)
{

  double lambda = 0.246, mu = 0.000134;
  pars[0] = lambda; pars[1] = mu;
}

void aphids_sample_priors(gsl_rng * r, st_sim_data *sim_data) 
{
  double lambda = 0.246, mu = 0.000134;
  double lambda_sd = 0.00079, mu_sd = 0.000002;
  double cor = 0.356;
  double *pars = sim_data->pars;

    gsl_ran_bivariate_gaussian (r, lambda_sd*10, mu_sd*10, cor, &pars[0], &pars[1]);
    pars[0] += lambda;
    pars[1] += mu;
  
  if(pars[0] < 0 || pars[1] < 0 ) {
    printf("2. BAD SAMPLE\n");
    exit(GSL_FAILURE);
  }
}

double aphids_eval_priors(double *pars)
{
  double lambda = 0.246, mu = 0.000134;
  double lambda_sd = 0.00079, mu_sd = 0.000002;
  double cor = 0.356;
  double density = GSL_NEGINF;
  if(pars[0] > 0 && pars[1] > 0 ) {
    density = gsl_ran_bivariate_gaussian_pdf(pars[0] - lambda, pars[1] - mu, 
                                             lambda_sd*10, mu_sd*10, cor);
    density = log(density);
  }
  return(density);
}

