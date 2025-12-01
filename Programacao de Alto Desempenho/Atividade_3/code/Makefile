CC=mpicc
CFLAGS=-O3 -std=c11
SERIAL_CC=gcc
SERIAL_CFLAGS=-O3 -std=c11

all: mpi_gol serial_gol

mpi_gol: mpi_gol.c
	$(CC) $(CFLAGS) -o mpi_gol mpi_gol.c

serial_gol: serial_gol.c
	$(SERIAL_CC) $(SERIAL_CFLAGS) -o serial_gol serial_gol.c

clean:
	rm -f mpi_gol serial_gol *.o *.csv
