/*
  Struct used to store online moment estimates
*/
typedef struct
{
  int n;
  double lam_m1;
  double mu_m1;
  double lam_m2;
  double mu_m2;
  double cov;
} st_moments;
