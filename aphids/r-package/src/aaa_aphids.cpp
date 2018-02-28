#include <Rcpp.h>
using namespace Rcpp;
#include <gsl/gsl_rng.h>
#include <gsl/gsl_math.h> /*GSL_POSINF*/
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>

#include "aphids_init.h"
#include "aphids_prior.h"
#include "aphids_simulation.h"
#include "aphids_inference.h"
int no_d = 2;
int no_cores = 1001;

gsl_rng **init_gsl() {
  int i;
  gsl_rng **r_vec = (gsl_rng **)malloc(no_cores*sizeof(gsl_rng *));

  for(i = 0; i < no_cores; i++) {
    r_vec[i] = gsl_rng_alloc(gsl_rng_mt19937);
    gsl_rng_set(r_vec[i], rand());
  }
  return r_vec;
}

st_sim_data **init_sim_data2(int no_d) {
  int i;
  st_sim_data **r_vec = (st_sim_data **) malloc(no_cores*sizeof(st_sim_data *));
  
  for(i = 0; i < no_cores; i++) {
    r_vec[i] = init_aphids_data(no_d);
  }
  return r_vec;
}


st_gsl_ode **init_gsl_ode2(st_sim_data **sim_data_vec) {
  int i;
  st_gsl_ode **r_vec = (st_gsl_ode **) malloc(no_cores*sizeof(st_gsl_ode *));
  
  for(i = 0; i < no_cores; i++) {
    r_vec[i] = init_gsl_ode(sim_data_vec[i]);
  }
  return r_vec;
}



int i;
st_sim_data **sim_data_vec = init_sim_data2(no_d);
st_gsl_ode **gsl_ode_vec = init_gsl_ode2(sim_data_vec);
//gsl_rng ** r_rng_vec = init_gsl();


// [[Rcpp::export]]
double get_utility(int core_no, int seed, NumericVector times)
{
  int i;
  if(core_no >= no_cores) stop("Error");
  gsl_rng *r = gsl_rng_alloc(gsl_rng_mt19937);
  gsl_rng_set(r, seed);
  
  if(times.length() != no_d) stop("Error");
  aphids_sample_priors(r, sim_data_vec[core_no]);
  aphids_sample_ic(r, sim_data_vec[core_no]);
  

  //utility = aphids_calculate_utility(r, gsl_ode, sim_data);
  //return utility;
  for(i=0;i <no_d; i++) {
    sim_data_vec[core_no]->t[i] = times[i];
  }
  for(i=0; i < (no_d-1); i++) {
    sim_data_vec[core_no]->t_diff[i] = times[i+1] - times[i];
  }
  aphids_sim(r, sim_data_vec[core_no], gsl_ode_vec[core_no], 2); 

  return aphids_calculate_utility(r, gsl_ode_vec[core_no], sim_data_vec[core_no]);
}
