#include "../include/st_main.h"
#include "st_aphids.h"
#include "aphids_init.h"
#include "aphids_update.h"


void aphids_update_sim_pars(st_sim_data *sim_data, double *pars)
{
  sim_data->pars[0] = pars[0]; sim_data->pars[1] = pars[1];
}



/* Updates simulated data structure
 * IC, Times, Pars 
 */
/* void updateSimICs(st_sim_data *sim_data, 
 *                   double n, double c) 
 * {
 *   sim_data->n[0] = n;
 *   sim_data->c[0] = c;
 * } */

/* void aphids_update_sim_data(st_sim_data *sim_data, double *times, double *pars) {
 * 
 *   updateSimICs(sim_data, 28, 28);
 *   updateSimTimes(sim_data, times);
 *   aphids_update_sim_pars(sim_data, pars);
 * } */

void update_LNA(st_sim_data * sim_data, int i) 
{
  int j;
  sim_data->lna_sps[0] = sim_data->n[i];
  sim_data->lna_sps[1] = sim_data->c[i];

  /*2nd order moments */
  for(j=2; j<5; j++) {
    sim_data->lna_sps[j] = 0;
  }

}

