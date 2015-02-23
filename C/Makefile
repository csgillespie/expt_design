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



$(FILE): $(OBJECTS) $(FILE).c
	$(CC) $(CFLAGS) $(OBJECTS) -o $(FILE) $(FILE).c $(LDFLAGS) $(DEBUG)

%.o : %.c
	$(CC) $(CFLAGS) -c $< $(DEBUG)

run: $(FILE)
	./$(FILE)

clean:
	rm -fv *.o *.tab *~ $(FILE)

cleaner:
	make clean
	rm -fvr tmp.csv parts_?.csv
