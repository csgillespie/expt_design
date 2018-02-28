#ifndef INIT_H_INCLUDED
#define INIT_H_INCLUDED
#include "st_main.h"

st_mcmc_npar *initMCMCnPar(int n);
st_mcmc_1par *initMCMC1Par();
// st_parallel *init_parallel(int no_threads, int no_d);
void free_mcmc_n_par(st_mcmc_npar * mcmc_par);
#endif
