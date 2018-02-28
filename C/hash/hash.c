#include <stdlib.h>  /* atoi, malloc */
#include <string.h>  /* strcpy */
#include "hash/uthash.h"

#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sort_vector.h>

typedef struct {
  int times[7];
} record_key_t;

typedef struct {
  record_key_t key;
  int row_number;                    /* key/id */
  UT_hash_handle hh;         /* makes this structure hashable */
} record_t;

void add_row(int row_number, gsl_matrix *m, record_t **records)
{
  record_t *r;
  record_t *result;
  record_t l;
  int i;
  r = (record_t*) malloc( sizeof(record_t) );
  memset(r, 0, sizeof(record_t));
  r->row_number = row_number;
  for(i=0; i<6; i++) {
    r->key.times[i] = (int) gsl_matrix_get(m, row_number, i);
  }

  //  HASH_ADD(hh, records, mat_row, sizeof(gsl_vector_view), r);
  HASH_ADD(hh, *records, key, sizeof(record_key_t), r);
  memset(&l, 0, sizeof(record_t));
  for(i=0; i<6; i++) {
    l.key.times[i] = (int) gsl_matrix_get(m, row_number, i);
  }

  //  HASH_FIND(hh, records, &l.mat_row, sizeof(gsl_vector_view), result);
  HASH_FIND(hh, *records, &l.key, sizeof(record_key_t), result);
  //  HASH_FIND(hh, *records, &(r->key), sizeof(record_key_t), result);
 
  if (result) {
    printf("Found %d\n", row_number);
    //printf("found %c %d\n", p->key.a, p->key.b);
  } else {
    printf("lost %d\n", row_number);
  }
}

int get_times(int row_number, record_t **records) 
{
  int i;
  record_t *r;
  r = (record_t*) malloc( sizeof(record_t) );
  HASH_FIND(hh, *records, &row_number, sizeof(record_key_t), r);
  //  HASH_FIND_INT( records, &row_number, s );  /* s: output pointer */
  //  return s;
  //  return 1;
  // for(i=0;i<6; i++) {
  if(r) {
    printf("%d", r->row_number);
  } else{ 
    printf("AAAA\n\n\n");
  }
    //}
  return 1;
}

int main(int argc, char *argv[]) 
{
  int i, j, row_number;
  record_t *records = NULL;
  record_t **records_ptr = &records;
  record_t *result;
  record_t l;
  gsl_matrix *m = gsl_matrix_alloc(4, 6);
  for(i=0; i<4; i++){
    for(j=0; j<6; j++) {
      gsl_matrix_set(m, i, j, (double) rand());
    }
    add_row(i, m, &records);
  }
  i = 0;
  row_number = i;
  memset(&l, 0, sizeof(record_t));

  for(j=0; j<6; j++) {
    l.key.times[j] = (int) gsl_matrix_get(m, row_number, j);
  }
    HASH_FIND(hh, records, &l.key, sizeof(record_key_t), result);
  //  HASH_FIND(hh, *records, &(r->key), sizeof(record_key_t), result);
  
  if (result) {
    printf("Found %d\n", result->row_number);
    //printf("found %c %d\n", p->key.a, p->key.b);
  } else {
    printf("lost %d\n", row_number);
  }
  
  printf("\n\n\n");

  get_times(0, &records);

    fflush(stdout);
  return(GSL_SUCCESS);
}
