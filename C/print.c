#include <stdio.h>
#include <stdlib.h> /* String */
#include <string.h> /* String */

#include <gsl/gsl_matrix.h>
#include "include/st_sim_data.h"

void printSimData(st_sim_data *sim_data) 
{
  int i;
  int no_d = sim_data->no_d;

  printf("%f, %f\n", 
         sim_data->pars[0], sim_data->pars[1]);
  
  for(i=0; i< 5; i++) {
    printf("%3.1f, %3.1f\n", sim_data->n[i], sim_data->c[i]);
  }

  for(i=0; i<(no_d-1); i++) {
    printf("%3.2f, %3.3f\n", sim_data->t[i], sim_data->t_diff[i]);
  }
  printf("%3.3f\n", sim_data->t[no_d-1]);

}

void saveParticles(gsl_matrix *particles, int i) {

  /*Assume max 50 characters file name */
  char fname[50], buffer [40];

  strcpy(fname, "parts_");
  sprintf(buffer, "%d", i);
  strcat(fname, buffer);
  strcat(fname, ".csv");

  int j, k;
  FILE *fp;
  fp = fopen(fname, "w");

  for(j=0; j<particles->size1; j++) {
    for(k=0; k<(particles->size2-1); k++) {
      fprintf(fp, "%d,", (int) gsl_matrix_get(particles, j, k));
    }
    fprintf(fp, "%d\n", (int) gsl_matrix_get(particles, j, particles->size2-1));
  }

  fclose(fp);

}
