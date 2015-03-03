#include <stdio.h>
#include <stdlib.h> /* String */
#include <string.h> /* String */

#include <gsl/gsl_matrix.h>
#include "include/st_sim_data.h"
#include "include/st_mcmc_par.h"

void add_newline(int nl)
{
  if(nl == 1)
    printf("\n");
}

/* Public */

void print_options(int no_d, int no_threads, int N, 
                   int max_levels, double *levels) 
{
  int i;
  printf("d:%d, t:%d, N:%d, l:%d\n", no_d, no_threads, N, max_levels);
  for(i=0; i<max_levels; i++)
    printf("%.0f ", levels[i]);
  add_newline(1);
}


void print_sim_pars(st_sim_data *sim_data, int nl) 
{
  int i;
  for(i=0; i<2; i++)
    printf("%f,\n", sim_data->pars[i]);
  add_newline(nl);
}


void print_times(double *times, int no_d, int nl) 
{
  int i;
  for(i=0; i<no_d; i++) 
    printf("%.0f,", times[i]);
  add_newline(nl);
}

void print_sim_n(st_sim_data *sim_data, int nl) 
{
  int i;
  int no_d = sim_data->no_d;

  for(i=0; i<2; i++)
    printf("%f,\n", sim_data->pars[i]);
  
  for(i=0; i< 5; i++) {
    printf("%3.1f, %3.1f\n", sim_data->n[i], sim_data->c[i]);
  }

  for(i=0; i<(no_d-1); i++) {
    printf("%3.2f, %3.3f\n", sim_data->t[i], sim_data->t_diff[i]);
  }
  printf("%3.3f\n", sim_data->t[no_d-1]);
}

void print_utils(st_mcmc_1par *util, int nl)
{
  printf("%f,%f", util->cur, util->prop);
  add_newline(nl);
}

void print_accept(int j, double no_accept, int nl) 
{
  printf("%.0f", no_accept*100.0/(j+1));
  add_newline(nl);
}

void print_mcmc_status(int j, int no_d, double power_up, 
                         double *times, st_mcmc_1par *util,
                       double no_accept)
{
  printf("%d,%.0f,", j, power_up);
  print_times(times, no_d, 0);
  print_utils(util, 0);
  printf(",");
  print_accept(j, no_accept, 0);

  add_newline(1);
}

