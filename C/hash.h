#include "include/uthash.h"

/* XXX: Evil. Magic number below is hardcoded. It
   really should be malloc from no_d. However, to 
   get the hasmap to work, I would need pointers to 
   a vector of integers, so I could hashmap on pointers
*/
 typedef struct {
  int times[10];
} record_key_t;

typedef struct {
  record_key_t key; /* hashmap key */
  int row_number;                   
  UT_hash_handle hh;         /* makes this structure hashable */
} record_t;

void add_times(int row_number, int no_d, double *times, record_t **records);
int find_times(int no_d, double *times, record_t **records) ;
