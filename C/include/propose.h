int propose_tps (gsl_rng *r, 
                 st_mcmc_npar *times, 
                 gsl_matrix *prop_particles,  gsl_ran_discrete_t *g, 
                 int level, int *row); 

void propose_pars(gsl_rng *r, double *pars);
