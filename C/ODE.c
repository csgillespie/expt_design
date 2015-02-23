#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>
#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/st_gsl_ode.h"
#include "include/st_sim_data.h"
#include "include/st_parallel.h"

#include "include/ODE.h"
#include "include/update.h"



/* Free 
 gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (stp);
*/

/*Public functions*/


int ode(st_gsl_ode *gsl_ode, st_sim_data *sim_data, int i)
{

  //  const gsl_odeiv_step_type *T = gsl_ode->T;
  gsl_odeiv_step *stp = gsl_ode->stp;
  gsl_odeiv_control *c = gsl_ode->c;
  gsl_odeiv_evolve *e = gsl_ode->e;
  gsl_odeiv_system sys = gsl_ode->sys;

  double t = 0.0;
  double maxtime = sim_data->t_diff[i];

  updateLNA(sim_data, i);

  double *sps = sim_data->lna_sps;
  double h = 1e-6;

  while (t < maxtime) {
    gsl_odeiv_evolve_apply (e, c, stp, &sys, &t, maxtime, &h, sps);
  }

  //printf("%f, %f, %f, %f, %f\n", sps[0], sps[1], sps[2], sps[3], sps[4]);
  return(GSL_SUCCESS);
}



