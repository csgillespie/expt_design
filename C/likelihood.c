#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>
#include <gsl/gsl_math.h>

#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/st_sim_data.h"
#include "include/st_mcmc_par.h"
#include "include/st_moments.h"
#include "include/st_gsl_ode.h"
#include "include/st_parallel.h"

#include "include/ODE.h"
#include "include/update.h"
#include "include/init.h"
#include "include/prior.h"
#include "include/simulation.h"

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
  int i=0;
  int no_d = sim_data->no_d;
  int accept = 1;
  double log_ll = 0.0;
  double mean, obs, std_dev;

  while(i<(no_d-1) && accept) {
    ode(gsl_ode, sim_data, i);
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

double get_utility(gsl_rng *r, 
                   st_gsl_ode *gsl_ode, 
                   st_sim_data *sim_data)
{
  int i=0, accept =1;
  int N = 100000;
  double log_u;

  st_mcmc_1par *ll = initMCMC1Par();
  st_mcmc_npar *pars = initMCMCnPar(2);
  sample_priors(r, pars->cur);
  double utilty;
  double compare;//, s2, std_err, rel_std_err_l,rel_std_err_m;
  ll->cur = GSL_NEGINF;
  st_moments *moments = initMoments();

  /* printf("\t%f,%f,%f,%f,%f\n", 
   *        sim_data->n[0],
   *        sim_data->n[1],
   *        sim_data->n[2],
   *        sim_data->n[3],
   *        sim_data->n[4]);
   * printf("\t%f,%f,%f,%f,%f\n", 
   *        sim_data->c[0],
   *        sim_data->c[1],
   *        sim_data->c[2],
   *        sim_data->c[3],
   *        sim_data->c[4]);
   * printf("\t%f,%f,%f,%f,%f\n", 
   *        sim_data->t[0],
   *        sim_data->t[1],
   *        sim_data->t[2],
   *        sim_data->t[3],
   *        sim_data->t[4]);
   * fflush(stdout); */

  /* Sample from Prior */
  int no_of_accepts = 0;
  while(i<N && accept> 0.5) {
    sample_priors(r, pars->prop);
    updateSimPars(sim_data, pars->prop);
    log_u = log(gsl_rng_uniform(r));
    compare = log_u + ll->cur;
    ll->prop = get_log_ll(gsl_ode, sim_data, compare);      

    if(ll->prop  > compare) {
      ll->cur = ll->prop;
      pars->cur[0] = pars->prop[0];
      pars->cur[1] = pars->prop[1];
      no_of_accepts++;
    }
    updateMoments(moments, pars->cur);
    i++;    
  }
  fflush(stdout);
  utilty = get_moment(3, moments)*get_moment(4, moments)-
                  get_moment(5,moments)*get_moment(5,moments);

  //  printf("%f\n", 1.0*no_of_accepts/i);
  free(pars);
  free(moments);
    
  return(-log(utilty));
}

void *threadSimulation(void *arg) {

  /* Unpack struct */
  st_thread *thread = (st_thread *) arg;
  gsl_rng *r = thread->gsl_rng_pt;
  st_sim_data *sim_data = thread->sim_data;
  st_gsl_ode *gsl_ode = thread->gsl_ode_pt;
  double no_sims = thread->no_sims;

  /* other variables */
  int i;
  thread->utility = 1;
  for(i=0; i<no_sims; i++) {
    sample_priors(r, sim_data->pars); /*Sample and update */
    //    forwardSim(r, sim_data); /*New simulation */
    odeSim(gsl_ode, sim_data);
    thread->utility *= get_utility(r, gsl_ode, sim_data);
  }
  //  printf("U: %d, %f\n", thread->thread_id, thread->utility);
  pthread_exit(NULL);
}
