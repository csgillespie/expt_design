#ifndef ST_PARTICLES_H_INCLUDED
#define ST_PARTICLES_H_INCLUDED
void update_particles(int j, gsl_matrix *cur_particles, double *times, 
                      double util); 
void swap_particles(gsl_matrix **m1, gsl_matrix **m2);
void save_particles(gsl_matrix *particles, int i);
#endif

