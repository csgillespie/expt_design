#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv.h>

#include <pthread.h> /*Thread library*/
#include <semaphore.h> /*Thread library*/


/* #include "include/st_moments.h"
 * #include "include/st_gsl_ode.h"
 * #include "include/st_mcmc_par.h"
 * #include "include/st_parallel.h" */


/* #include "include/init.h"
 * #include "include/simulation.h"
 * #include "include/likelihood.h" */
#include "include/inference.h"

/* #include "include/print.h"
 * #include "include/update.h"
 * #include "include/prior.h" */

int main(int argc, char *argv[])
{
  int no_d = 6; /* includes time t= 0*/
  int no_threads = 20;
 
  inferTime(no_d, no_threads);
 
  return(GSL_SUCCESS);
}




















