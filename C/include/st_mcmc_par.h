/*
  Struct used the proposed and current value in mcmc loop.
  E.g. Lambda: lambda_prop & lambda_cur
*/
typedef struct
{
  double *prop;
  double *cur;
  int n;
} st_mcmc_npar;


typedef struct
{
  double prop;
  double cur;
} st_mcmc_1par;

