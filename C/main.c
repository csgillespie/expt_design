#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <gsl/gsl_rng.h>

#include "include/io.h"
#include "include/inference.h"
#include "include/st_mcmc_par.h"
#include "include/print.h"

double *get_levels(int max_level)
{
  int i;
  double *levels;
  
  levels  = (double *) malloc(max_level*sizeof(double));
  for(i=0; i<max_level; i++){
    if(i < 2) levels[i] = 0;
    else if(i < 4) levels[i] = 1;
    else levels[i] = 2;
    levels[i] = 2;
  }
  return(levels);
}


int main(int argc, char *argv[])
{
  int verbose = get_verbose(argc, argv);
  int no_d = get_d(argc, argv);/* includes time t= 0*/
  int no_threads = get_threads(argc, argv);
  int N = get_N(argc, argv);
  int max_levels = get_max_level(argc, argv);
  double *levels = get_levels(max_levels);

  if(verbose == 1)
    print_options(no_d, no_threads, N, max_levels, levels);
  mcmc(no_d, no_threads, N, max_levels, levels, verbose);
  return(GSL_SUCCESS);
}




















