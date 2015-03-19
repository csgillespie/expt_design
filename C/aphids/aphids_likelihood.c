#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_matrix.h>

#include "../include/st_main.h"
#include "../include/init.h"

#include "st_aphids.h"
#include "aphids_init.h"
#include "aphids_propose.h"
#include "aphids_simulation.h"
#include "aphids_prior.h"
#include "aphids_update.h"
#include "aphids_likelihood.h"
/* 
 * Private functions
 * However, most are exported as functions pointers
 */

/* 
 * See http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
 */
void updateMoments(st_moments *moments, double *pars) {

  double delta, lambda, mu;
  lambda = pars[0]; mu = pars[1];
  int n = ++(moments->n);

  /*Update lambda moments*/
  delta = lambda - moments->lam_m1;
  moments->lam_m1 += delta/n;
  if(n > 1)  moments->lam_m2 = moments->lam_m2*(n-2)/(n-1) + delta*delta/n;

  /* Covariance: note that the mean for mu has yet been updated */
  moments->cov += (lambda - moments->lam_m1)*(mu - moments->mu_m1);

  /*Update mu moments*/
  delta = mu - moments->mu_m1;
  moments->mu_m1 += delta/n;
  if(n > 1)  moments->mu_m2 = moments->mu_m2*(n-2)/(n-1) + delta*delta/n;
}    

double get_log_ll(st_gsl_ode *gsl_ode, 
                  st_sim_data *sim_data, 
                  double compare) 
{
  int i=0, accept=1;
  int no_d = sim_data->no_d;
  double log_ll = 0.0;
  double mean, obs, std_dev;

  if(sim_data->pars[0] < 0 || sim_data->pars[1] < 0) {
    accept = 0;
  }

  while(i<(no_d-1) && accept) {
    aphids_ode(gsl_ode, sim_data, i);
    obs = sim_data->n[i+1];
    mean = sim_data->lna_sps[0];
    std_dev = pow(sim_data->lna_sps[2], 0.5);
    log_ll += log(gsl_ran_gaussian_pdf (obs - mean, std_dev));

    if(log_ll < compare || obs < 0.5){
      accept = 0;
    }
    i++;
  }
  return(log_ll);
}

double get_moment(int i, st_moments *moments) {
  double m = 0;
  int n = moments->n;

  if(i==1) {
    m = moments->lam_m1;
  } else if(i == 2) {
    m = moments->mu_m1;
  } else if(i == 3) {
    m = moments->lam_m2;
  } else if(i == 4) {
    m = moments->mu_m2;
  } else if(i == 5) {
    m = moments->cov/(n-1);
  } else {
    exit(EXIT_FAILURE);
  }
  return(m);
}

double aphids_calculate_utility(gsl_rng *r, st_gsl_ode *gsl_ode, st_sim_data *sim_data)
{
  int i=0, accept=1, N=10000;
  double log_u, utilty, compare;

  st_mcmc_1par *ll = initMCMC1Par();
  st_mcmc_npar *pars = initMCMCnPar(2);
  aphids_sample_priors(r, sim_data);
  aphids_sample_priors(r, sim_data);

  ll->cur = GSL_NEGINF;
  st_moments *moments = init_moments();
  
  /* sample from Prior */
  int no_of_accepts = 0;
  while(i<N && accept> 0.5) {
    aphids_propose_pars(r, pars->prop);
    aphids_update_sim_pars(pars->cur, pars->prop);
    //    aphids_update_sim_pars(sim_data, pars->prop);
    log_u = log(gsl_rng_uniform(r));
    compare = log_u + ll->cur + aphids_eval_priors(pars->cur) -  aphids_eval_priors(pars->prop);
    ll->prop = get_log_ll(gsl_ode, sim_data, compare);

    if(ll->prop  > compare) {
      ll->cur = ll->prop;
      aphids_update_sim_pars(pars->cur, pars->prop);
      no_of_accepts++;
    } else {
      aphids_update_sim_pars(pars->prop, pars->cur);
    }
   
    if(i % 10 == 0) updateMoments(moments, pars->cur);
    i++;    
  }
  fflush(stdout);
  utilty = get_moment(3, moments)*get_moment(4, moments)-
                  get_moment(5,moments)*get_moment(5,moments);
  /* printf("%f, %f, %f\n", pow(get_moment(3, moments),0.5),
   *        pow(get_moment(4, moments),0.5), get_moment(5, moments));*/
  //printf("Accept: %f\n", 1.0*no_of_accepts/i*100);
  free(pars);
  free(moments);
  return(-log(utilty));
}

