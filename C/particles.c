#include <stdio.h>
#include <stdlib.h> /* String */
#include <string.h> /* String */

#include <gsl/gsl_matrix.h>


void update_particles(int j, gsl_matrix *cur_particles, double *times, double util) 
{
  int k, no_d;
  no_d = cur_particles->size2 - 1;

  for(k=0; k<no_d; k++) {
    gsl_matrix_set(cur_particles, j, k, times[k]);
  }
  gsl_matrix_set(cur_particles, j, k, util);
}

void swap_particles(gsl_matrix **m1, gsl_matrix **m2) 
{
  gsl_matrix *tmp = *m1;
  *m1 = *m2;
  *m2 = tmp;
}

void save_particles(gsl_matrix *particles, int i) {

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
    fprintf(fp, "%f\n",  gsl_matrix_get(particles, j, particles->size2-1));
  }
  fclose(fp);
}
