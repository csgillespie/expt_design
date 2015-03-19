#ifndef INFERENCE_H_INCLUDED
#define INFERENCE_H_INCLUDED
//#include "st_model_funcs.h"

int mcmc(int no_d, int no_threads, int N, int max_levels, 
         double *levels, int verbose);
#endif
