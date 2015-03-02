#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/* 
 * Prior from G & G Aphid paper
 * Means: 0.246, 0.000134
 * SDs, cor: 0.00079, 0.000002, 0.356
 */

/* Public functions */

//  gsl_ran_bivariate_gaussian (const gsl_rng * r, double sigma_x, double sigma_y, double rho, double * x, double * y)
void sample_priors(gsl_rng * r, double *pars, int level) 
{
  double lambda = 0.246, mu = 0.000134;
  double lambda_sd = 0.00079, mu_sd = 0.000002;
  double cor = 0.356;

  if(level < 1.5) {
    pars[0] = lambda; pars[1] =mu ;
  } else {
    gsl_ran_bivariate_gaussian (r, lambda_sd*10, mu_sd*10, cor, &pars[0], &pars[1]);
    pars[0] += lambda;
    pars[1] += mu;
  }
  
  if(pars[0] < 0 || pars[1] < 0 ) {
    printf("2. BAD SAMPLE\n");
    exit(GSL_FAILURE);
  }
}

//double gsl_ran_bivariate_gaussian_pdf (double x, double y, double sigma_x,
//double sigma_y, double rho)

double eval_priors(double *pars)
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

