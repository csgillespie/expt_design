int propose_tps (gsl_rng *r, 
                 st_mcmc_npar *times, 
                 gsl_matrix *prop_particles, 
                 int i); 

void propose_pars(gsl_rng *r, double *pars);
