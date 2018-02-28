#include <RcppGSL.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
// [[Rcpp::depends(RcppGSL)]]
// [[Rcpp::export]]
Rcpp::NumericVector colNorm(const RcppGSL::Matrix & G) {
  int k = G.ncol();
  Rcpp::NumericVector n(k);           // to store results
  for (int j = 0; j < k; j++) {
    RcppGSL::VectorView colview = gsl_matrix_const_column (G, j);
    n[j] = gsl_blas_dnrm2(colview);
  }
  return n;                           // return vector
}


// [[Rcpp::export]]
double ode() {
  return 1;
}
// gsl_ode->T = gsl_odeiv2_step_rkf45;
// gsl_ode->stp =  gsl_odeiv2_step_alloc(gsl_ode->T, 5);
// gsl_ode->c =  gsl_odeiv2_control_y_new(1e-5, 0.0);
// gsl_ode->e = gsl_odeiv2_evolve_alloc(5);
// gsl_odeiv2_system sys = {func, 0, 5, sim_data->pars};