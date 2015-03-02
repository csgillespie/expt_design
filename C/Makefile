###########################################################
# Makefile
###########################################################
### Variables ###
#Created CSG
# Use GCC compiler


CC =  gcc
CFLAGS = -Wall 
CFLAGS =  -Wall -O3 -DHAVE_INLINE -DGSL_RANGE_CHECK_OFF  
OBJECTS = init.o ODE.o print.o simulation.o update.o  propose.o likelihood.o 
OBJECTS += prior.o inference.o
LDFLAGS =  -lgsl -lgslcblas -lm -lpthread
FILE=main

SHELL = /bin/bash
.SHELLFLAGS = -e



$(FILE): $(OBJECTS) $(FILE).c
	$(CC) $(CFLAGS) $(OBJECTS) -o $(FILE) $(FILE).c $(LDFLAGS) $(DEBUG)

%.o : %.c
	$(CC) $(CFLAGS) -c $< $(DEBUG)

load:
	module load gsl/1.16
	module load gcc/4.9.2

run: $(FILE)
	./$(FILE)


# build:
# 	make clean 
# 	make load
# 	make $(MAIN)
submit:
	qsub myscript.qsub

copy:
	scp parts_*.csv clyde:/tmp/

clean:
	rm -fv *.o *.tab *~ $(FILE)

cleaner:
	make clean
	rm -fvr tmp.csv parts_?.csv
	rm -fvr *.qsub.*
