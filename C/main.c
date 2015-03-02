#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>

#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/

#include "include/inference.h"

int main(int argc, char *argv[])
{
  int no_d = 3; /* includes time t= 0*/
  int no_threads = 4;
  int N = 10;
 
  mcmc(no_d, no_threads, N);
 
  return(GSL_SUCCESS);
}




















