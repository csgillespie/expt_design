#include <stdio.h>
#include <stdlib.h> /* String */
#include <string.h> /* String */

#include <gsl/gsl_matrix.h>
#include "st_main.h"
#include "st_aphids.h"
#include "aphids_print.h"

void aph_print_sim_pars(st_sim_data *sim_data) 
{
  printf("%f, %f\n", sim_data->pars[0],sim_data->pars[1]);
}

void aph_print_sim_n(st_sim_data *sim_data) 
{
  int i;
  int no_d = sim_data->no_d;
  aph_print_sim_pars(sim_data);
  
  for(i=0; i< no_d; i++) {
    printf("%3.1f, %3.1f\n", sim_data->n[i], sim_data->c[i]);
  }

  for(i=0; i<(no_d-1); i++) {
    printf("%3.2f, %3.3f\n", sim_data->t[i], sim_data->t_diff[i]);
  }
  printf("%3.3f\n", sim_data->t[no_d-1]);
}
