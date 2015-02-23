int propose_tps (gsl_rng *r, 
                 st_mcmc_npar *times, 
                 gsl_matrix *prop_particles, 
                 int i); 
void swapParticles(gsl_matrix **m1, gsl_matrix **m2);
