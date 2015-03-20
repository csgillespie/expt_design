###########################################################
# Makefile
###########################################################
### Variables ###
#Created CSG
# Use GCC compiler


CC =  gcc
CFLAGS = -Wall
#CFLAGS =  -Wall -O3 -DHAVE_INLINE -DGSL_RANGE_CHECK_OFF  
CFLAGS += -Iinclude/ 
OBJECTS = inference.o init.o particles.o print.o io.o propose_time.o

APHIDS = aphids.o  aphids_init.o  aphids_inference.o aphids_print.o 
APHIDS += aphids_prior.o  aphids_propose.o  aphids_simulation.o  aphids_update.o
APHIDS_OBJECTS = $(foreach obj,$(APHIDS),aphids/$(obj))
OBJECTS += $(APHIDS_OBJECTS)

LDFLAGS =  -lgsl -lgslcblas -lm -lpthread
FILE=main

#SHELL = /bin/bash
.SHELLFLAGS = -e

SHELL=/bin/bash -l 

$(FILE): $(OBJECTS) $(FILE).c
	$(CC) $(CFLAGS) $(OBJECTS) -o $(FILE) $(FILE).c $(LDFLAGS) $(DEBUG)



%.o : %.c
	$(CC) $(CFLAGS) -o $@ -c $< $(DEBUG)
#$(CC) $(CFLAGS) -c $< $(DEBUG)

load:
	module load gsl/1.16
	module load gcc/4.9.2

run: $(FILE)
	./$(FILE) -d 2 -l 3 -N 3


# build:
# 	make clean 
# 	make load
# 	make $(MAIN)
submit:
	make clean
	make $(FILE)
	qsub submit.qsub

copy:
	scp parts_*.csv clyde:/tmp/

clean:
	rm -fv *.tab *~ $(FILE) $(OBJECTS)

cleaner:
	make clean
	rm -fvr tmp.csv parts_?.csv
	rm -fvr *.qsub.*
