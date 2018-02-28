#ifndef APHIDS_UPDATE_H_INCLUDED
#define APHIDS_UPDATE_H_INCLUDED
//#include "aphids_init.h"
//void updateSimICs(st_sim_data *sim_data, double n, double c);
//void aphids_update_sim_pars(st_sim_data * sim_data, double *pars);
//void aphids_update_sim_data(st_sim_data *sim_data, double *times, double *pars);
void aphids_update_sim_pars(st_sim_data * sim_data, double *pars);
void update_LNA(st_sim_data * sim_data, int i);
#endif
