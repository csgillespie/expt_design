#include <stdio.h>
#include <stdlib.h> /* String */
#include <string.h> /* String */

#include <gsl/gsl_matrix.h>
#include "st_main.h"

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



void print_times(double *times, int no_d, int nl) 
{
  int i;
  for(i=0; i<no_d; i++) 
    printf("%.0f,", times[i]);
  add_newline(nl);
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

