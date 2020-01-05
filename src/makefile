
prefix = ~/bin
mpif90 = /opt/local/lib/openmpi/bin/mpif90
f90 = gfortran
flags = -ftree-vectorize -O3 -fopenmp -ffree-line-length-none
lapacklib = /usr/local/lib

all: string_2d.f90
	$(f90) string_2d.f90  $(flags) -o string_2d.x
	
clean:
	rm -f *.o *.mod *.x

