/*
  Simulation data structure
*/

typedef struct
{
  double *n; //Length D
  double *c; //Length D
  double *t; //Length D
  double *t_diff; //Length D-1: time differences
  double *pars; // Length 2

  double *lna_sps; // Length 5
  int no_d;
} st_sim_data;
