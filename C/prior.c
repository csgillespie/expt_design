#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/* 
 * Prior from G & G Aphid paper
 * Means: 0.4102, 0.009718
 * SDs, cor: 0.03622, 0.0005893, 0.1233
 */


//  gsl_ran_bivariate_gaussian (const gsl_rng * r, double sigma_x, double sigma_y, double rho, double * x, double * y)
void sample_priors(gsl_rng * r, double *pars) 
{
  gsl_ran_bivariate_gaussian (r, 0.03622, 0.0005893, 0.1233, &pars[0], &pars[1]);
  pars[0] += 0.4102;
  pars[1] += 0.009718;

  if(pars[0] < 0 || pars[1] < 0 ) {
    printf("BAD SAMPLE\n");
    exit(1);
  }
}



//double gsl_ran_bivariate_gaussian_pdf (double x, double y, double sigma_x,
//double sigma_y, double rho)

double eval_priors(double *pars)
{
  double density = GSL_NEGINF;
  if(pars[0] > 0 && pars[1] > 0 ) {
    density = gsl_ran_bivariate_gaussian_pdf(
                                             pars[0] -  0.4102, pars[1] - 0.009718, 
                                             0.03622, 0.0005893, 0.1233);
    density = log(density);
  } else {
    printf("BAD SAMPLE\n");
    exit(1);
  }

  // return(0);
  return(density);

}

