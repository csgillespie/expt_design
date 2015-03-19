#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_odeiv2.h>

#include "aphids_init.h"
#include "aphids_simulation.h"
#include "aphids_update.h"

/* 
   Private functions
*/ 

void stepSim(gsl_rng *r, st_sim_data *sim_data, int i) 
{

  double t=0;
  double u, total_haz;
  double *pars = sim_data->pars;
  double maxtime = sim_data->t_diff[i];
  double n = sim_data->n[i];
  double c = sim_data->c[i];
  while(n > 0) {
    total_haz = pars[0]*n + pars[1]*n*c;
    t -= log(gsl_rng_uniform(r))/total_haz;
    if(t > maxtime) break;

    u = gsl_rng_uniform(r);
    if(pars[0]*n/total_haz > u) {
      n++;
      c++;
    } else {
      n--;
    }
  }
  sim_data->n[i+1] = n;
  sim_data->c[i+1] = c;
}

void gillespieSim(gsl_rng *r, st_sim_data *sim_data) {
  int i;
  int no_d = sim_data->no_d;
  for(i=0; i< (no_d-1); i++) {
    stepSim(r, sim_data, i);
  }
}



void odeSim(st_sim_data *sim_data,st_gsl_ode *gsl_ode) {

  int i;
  int no_d = sim_data->no_d;
  sim_data->lna_sps[0] = sim_data->n[0];
  sim_data->lna_sps[1] = sim_data->c[0];
  for(i=0; i< (no_d-1); i++) {
    aphids_ode(gsl_ode, sim_data, i);
    sim_data->n[i+1]  = sim_data->lna_sps[0];
    sim_data->c[i+1]  = sim_data->lna_sps[1];
  }
}



/* 
   Public functions
*/

void aphids_sim(gsl_rng *r, st_sim_data *sim_data, 
                st_gsl_ode *gsl_ode, int level) 
{

  if(level == 0) {
    odeSim(sim_data, gsl_ode);
  } else {
    gillespieSim(r, sim_data);
 }
  
}

int aphids_ode(st_gsl_ode *gsl_ode, st_sim_data *sim_data, int i)
{
  gsl_odeiv2_step *stp = gsl_ode->stp;
  gsl_odeiv2_control *c = gsl_ode->c;
  gsl_odeiv2_evolve *e = gsl_ode->e;
  gsl_odeiv2_system sys = gsl_ode->sys;

  double t = 0.0;
  double maxtime = sim_data->t_diff[i];

  update_LNA(sim_data, i);

  double *sps = sim_data->lna_sps;
  double h = 1e-6;

  while (t < maxtime) {
    gsl_odeiv2_evolve_apply (e, c, stp, &sys, &t, maxtime, &h, sps);
  }
  return(GSL_SUCCESS);
}



