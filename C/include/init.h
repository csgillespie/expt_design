//st_sim_data *initSimData(int D);
st_mcmc_npar *initMCMCnPar(int n);
st_mcmc_1par *initMCMC1Par();
st_moments *initMoments();
st_gsl_ode *initGSLODE(st_sim_data *sim_data);
st_parallel *init_parallel(int no_threads, int no_d);

void *free_mcmc_n_par(st_mcmc_npar * mcmc_par);
