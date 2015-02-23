#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_odeiv.h>

#include "include/st_gsl_ode.h"
#include "include/st_sim_data.h"
#include "include/simulation.h"
#include "include/ODE.h"


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


/* 
   Public functions
*/

void forwardSim(gsl_rng *r, st_sim_data *sim_data) {

  int i;
  int no_d = sim_data->no_d;
  for(i=0; i< (no_d-1); i++) {
    stepSim(r, sim_data, i);
  }
}

void odeSim(st_gsl_ode *gsl_ode, st_sim_data *sim_data) {

  int i;
  int no_d = sim_data->no_d;
  for(i=0; i< (no_d-1); i++) {
    ode(gsl_ode, sim_data, i);
    sim_data->n[i+1]  = floor(sim_data->lna_sps[0]);
    sim_data->c[i+1]  = floor(sim_data->lna_sps[1]);
  }
}


