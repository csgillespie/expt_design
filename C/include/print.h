void print_options(int no_d, int no_threads, int N, 
                   int max_levels, double *levels);
void print_mcmc_status(int j, int no_d, double power_up, 
                       double *times, st_mcmc_1par *util,
                       double no_accept);
void print_times(double *times, int no_d, int nl);
