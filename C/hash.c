#include "include/uthash.h"
#include "hash.h"

void add_times(int row_number, int no_d, double *times, record_t **records) {
  
  record_t *r;//, *result;
  //  record_t l;
  int i;//, row_number = -1o;

  /* Create a record for the hash map */
  r = (record_t*) malloc(sizeof(record_t));
  memset(r, 0, sizeof(record_t));

  /* Store row_number and times */
  r->row_number = row_number;
  for(i=0; i<(no_d+1); i++) {
    r->key.times[i] = times[i];
  }
  HASH_ADD(hh, *records, key, sizeof(record_key_t), r);
  //HASH_FIND(hh, *records, &r.key, sizeof(record_key_t), result);
}

int find_times(int no_d, double *times, record_t **records) 
{

  record_t *r;
  int i, row_number = -1;
  record_t *result;
  r = (record_t*) malloc(sizeof(record_t));
   memset(r, 0, sizeof(record_t));
  
  for(i=0; i<(no_d+1); i++) {
    r->key.times[i] = times[i];
  }
  HASH_FIND(hh, *records, &(r->key), sizeof(record_key_t), result);
  if (result) {
    row_number = result->row_number;
  }
  /* If row_number is negative, not in filter */
  return(row_number);
}

/* int get_times(int row_number, record_t **records) 
 * {
 *   struct record_t *s;
 * 
 *   HASH_FIND_INT( records, &row_number, s );  /\* s: output pointer *\/
 *   //  return s;
 *   //  return 1;
 * } */
