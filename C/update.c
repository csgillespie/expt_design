#include "include/st_sim_data.h"


void updateSimTimes(st_sim_data *sim_data, double *times) 
{
  int i;
  int no_d = sim_data->no_d;
  sim_data->t[0] = times[0];
  for(i=1; i<no_d; i++) {
    sim_data->t[i] = times[i];
    sim_data->t_diff[i-1] = times[i] - times[i-1];
  }
}

void updateSimICs(st_sim_data *sim_data, 
                  double n, double c) 
{
  sim_data->n[0] = n;
  sim_data->c[0] = c;
}

void updateSimPars(st_sim_data * sim_data, 
                   double *pars)
{
  /*Could be move clever here */
  sim_data->pars[0] = pars[0];
  sim_data->pars[1] = pars[1];

}


/* Updates simulated data structure
 * IC, Times, Pars 
 */
void updateSimData(st_sim_data *sim_data, double *times, double *pars) {

  updateSimICs(sim_data, 28, 28);
  updateSimTimes(sim_data, times);
  updateSimPars(sim_data, pars);
}



void updateLNA(st_sim_data * sim_data, int i) 
{
  int j;
  sim_data->lna_sps[0] = sim_data->n[i];
  sim_data->lna_sps[1] = sim_data->c[i];

  /*2nd order moments */
  for(j=2; j<5; j++) {
    sim_data->lna_sps[j] = 0;
  }

}



