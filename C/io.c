#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <getopt.h>
#include <gsl/gsl_rng.h>
  
char *getSimulatorOptions()
{
  char *options = "t:N:d:l:v:";
  return(options);
}

/* Public */
int get_N(int argc, char *argv[]) 
{
  int opt, no_threads=1;
  char *options = getSimulatorOptions();

  while ((opt = getopt (argc, argv, options)) != -1) {
    switch(opt) 
      {
      case 'N':
        no_threads = atoi(optarg);
        break;
      }
  }
  optind = 1;
  return(no_threads);
}

int get_threads(int argc, char *argv[]) 
{

  int opt, no_threads=1;
  char *options = getSimulatorOptions();

  while ((opt = getopt (argc, argv, options)) != -1) {
    switch(opt) 
      {
      case 't':
        no_threads = atoi(optarg);
        break;
      }
  }
 if(no_threads < 1) {
    printf("Error: No threads should be > 0\n");
    exit(GSL_FAILURE);
  }
  optind = 1;
  return(no_threads);
}
int get_d(int argc, char *argv[]) 
{

  int opt, no_d=0;
  char *options = getSimulatorOptions();

  while ((opt = getopt (argc, argv, options)) != -1) {
    switch(opt) 
      {
      case 'd':
        no_d = atoi(optarg);
        break;
      }
  }
  if(no_d < 2) {
    printf("Error: Need more design points\n");
    exit(GSL_FAILURE);
  }
  optind = 1;
  return(no_d);
}

int get_max_level(int argc, char *argv[]) 
{
  int opt, max_level=1;
  char *options = getSimulatorOptions();

  while ((opt = getopt (argc, argv, options)) != -1) {
    switch(opt) 
      {
      case 'l':
        max_level = atoi(optarg);
        break;
      }
  }
  if(max_level < 1) {
    printf("Error: Need more levels\n");
    exit(GSL_FAILURE);
  }
  optind = 1;
  return(max_level);
}

int get_verbose(int argc, char *argv[]) 
{
  int opt, verbose=1;
  char *options = getSimulatorOptions();

  while ((opt = getopt (argc, argv, options)) != -1) {
    switch(opt) 
      {
      case 'v':
        verbose = atoi(optarg);
        break;
      }
  }
  if(verbose != 1) verbose = 0;
  optind = 1;
  return(verbose);
}
